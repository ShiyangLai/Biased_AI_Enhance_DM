# ==============================================================================
# balance_check_education.R — covariate balance for the classroom experiment, in the
# same shape as notebooks/R/balance_check_investment.R so the two experiments'
# love plots can be drawn by the same plotting code.
#
#   continuous (ordinal + count) -> absolute standardized mean difference (ASMD),
#                                   pooled-variance denominator, Welch's t
#   categorical (nominal)        -> Cramer's V on the 2 x k table, chi-squared
#   negligible-imbalance reference line at 0.10
#
# CONTRAST. Default-vs-biased needs no baseline balance check: every student
# contributes default weeks, so that contrast is within student and baseline
# covariates are identical by construction. The randomized between-student
# contrast is which students received a novelty week (n = 19) vs a reliability
# week (n = 20); 6 students hold both and appear in both groups.
#
# WHY THIS SCRIPT ADDS A RANDOMIZATION NULL. With n = 19 vs 20 the sampling SD of
# an ASMD under pure randomization is sqrt(1/19 + 1/20) ~ 0.32, so a 0.10
# threshold sits at 0.31 SD and roughly 75% of covariates clear it by chance
# alone. Reporting "18 of 26 exceed 0.10" therefore reads as a randomization
# failure when it is the expected signature of a small sample. Every covariate is
# accordingly also given a permutation p-value and the 95th percentile of its own
# null, obtained by re-randomizing the arm labels; those calibrate the 0.10 line
# rather than replacing it.
#
# Input  : data/supplementary_dnr_student_week.csv
# Outputs: figures/balance_education.csv  (feeds upstream/figure_balance_education.py)
#   setwd("education/code"); source("_setup.R"); source("balance_check_education.R")
# ==============================================================================
set.seed(42)
NPERM <- 5000

IN <- DATA_DIR; OUT <- FIG_DIR

# code, type, display label (short enough for the love-plot y axis)
CODEBOOK <- read.csv(text = "
code,type,label
stop_good_enough,continuous,Stops searching when good enough
pref_one_answer,continuous,Prefers one best answer
verify_freq,continuous,Verifies AI outputs
expect_learn,continuous,Expects AI to help learning
pref_reliable,continuous,Prefers reliable/standard solutions
hours_ai_7d,continuous,AI hours past 7 d
eval_confidence,continuous,Confidence evaluating AI code
claude_freq,continuous,Prior Claude Code use
n_ai_tools,continuous,AI coding tools used
academic_level,categorical,Academic level
pref_novel,continuous,Prefers unconventional approaches
comfort_reject,continuous,Comfortable rejecting AI
prior_ide_tool,categorical,Prior IDE-integrated AI tool
use_learning,continuous,AI use: learning
prior_prog,continuous,Prior programming experience
prior_ml,continuous,Prior ML/AI experience
pref_several,continuous,Prefers several strategies
worry_dependence,continuous,Worries about AI dependence
n_ai_tasks,continuous,AI coding tasks
effort_better,continuous,Willing to spend extra effort
use_coding,continuous,AI use: coding
enjoy_explore,continuous,Enjoys exploring alternatives
n_languages,continuous,Languages/tools used
n_ai_settings,continuous,AI tool settings used
pref_safe_pressure,continuous,Chooses safest under pressure
use_writing,continuous,AI use: writing
", stringsAsFactors = FALSE, strip.white = TRUE)
CODEBOOK <- CODEBOOK[CODEBOOK$code != "", ]

# ── Metrics (identical formulas to the investment script) ────────────────────
asmd <- function(x, g) {
  a <- x[g == "NOVELTY_BIASED"]; b <- x[g == "RELIABILITY_BIASED"]
  a <- a[is.finite(a)]; b <- b[is.finite(b)]
  if (length(a) < 2 || length(b) < 2) return(NA_real_)
  s2 <- (var(a) + var(b)) / 2
  if (!is.finite(s2) || s2 <= 0) return(NA_real_)
  abs(mean(a) - mean(b)) / sqrt(s2)
}
cramers_v <- function(x, g) {
  lv <- ifelse(is.na(x) | x == "", "Missing", as.character(x))
  tb <- table(g, lv)
  if (nrow(tb) < 2 || ncol(tb) < 2) return(NA_real_)
  ct <- suppressWarnings(chisq.test(tb))
  sqrt(unname(ct$statistic) / (sum(tb) * (min(dim(tb)) - 1)))
}
metric_of <- function(code, typ, x, g)
  if (typ == "continuous") asmd(as.numeric(x), g) else cramers_v(x, g)

# ── Data: one row per randomized novelty/reliability assignment ───────────────
dnr <- read.csv(file.path(IN, "supplementary_dnr_student_week.csv"),
                stringsAsFactors = FALSE, check.names = FALSE)
bal <- dnr[dnr$policy != "DEFAULT", ]
grp <- as.character(bal$policy)
cat(sprintf("Balance sample: %d randomized assignments (novelty %d, reliability %d; %d students in both)\n",
            nrow(bal), sum(grp == "NOVELTY_BIASED"), sum(grp == "RELIABILITY_BIASED"),
            sum(table(bal$student_id) == 2)))

# ── Observed metrics + classical tests ───────────────────────────────────────
res <- do.call(rbind, lapply(seq_len(nrow(CODEBOOK)), function(i) {
  code <- CODEBOOK$code[i]; typ <- CODEBOOK$type[i]; x <- bal[[code]]
  eff <- metric_of(code, typ, x, grp)
  if (typ == "continuous") {
    v <- as.numeric(x); ok <- is.finite(v)
    tt <- t.test(v[ok & grp == "NOVELTY_BIASED"], v[ok & grp == "RELIABILITY_BIASED"],
                 var.equal = FALSE)
    stat <- unname(tt$statistic); df <- unname(tt$parameter); p <- tt$p.value
  } else {
    lv <- ifelse(is.na(x) | x == "", "Missing", as.character(x))
    ct <- suppressWarnings(chisq.test(table(grp, lv)))
    stat <- unname(ct$statistic); df <- unname(ct$parameter); p <- ct$p.value
  }
  data.frame(Variable = CODEBOOK$label[i], code = code, type = typ,
             effect = eff, stat = stat, df = df, p = p, stringsAsFactors = FALSE)
}))

# ── Randomization null: re-randomize the arm labels, recompute every metric ───
cat(sprintf("Permuting arm labels (%d draws) ...\n", NPERM))
PERM <- replicate(NPERM, {
  gp <- sample(grp)
  vapply(seq_len(nrow(CODEBOOK)),
         function(i) metric_of(CODEBOOK$code[i], CODEBOOK$type[i],
                               bal[[CODEBOOK$code[i]]], gp), numeric(1))
})
res$rand_p   <- vapply(seq_len(nrow(res)), function(i) mean(PERM[i, ] >= res$effect[i]), numeric(1))
res$null_p50 <- apply(PERM, 1, median, na.rm = TRUE)
res$null_p95 <- apply(PERM, 1, quantile, probs = .95, na.rm = TRUE)
res$p_fdr      <- p.adjust(res$p, "fdr")
res$rand_p_fdr <- p.adjust(res$rand_p, "fdr")
res$over_0.10  <- ifelse(res$effect >= 0.10, "*", "")

res <- res[order(-res$effect), ]
print(res[, c("Variable", "type", "effect", "over_0.10", "p", "p_fdr", "rand_p", "null_p95")],
      row.names = FALSE, digits = 3)

# ── How many exceed 0.10, and how many would under pure randomization ─────────
n_flag <- colSums(PERM >= 0.10, na.rm = TRUE)
obs_flag <- sum(res$effect >= 0.10, na.rm = TRUE)
cat(sprintf("\nExceeding 0.10: OBSERVED %d of %d\n", obs_flag, nrow(res)))
cat(sprintf("  under re-randomization: median %.0f, 90%%%% interval [%.0f, %.0f], P(>= observed) = %.3f\n",
            median(n_flag), quantile(n_flag, .05), quantile(n_flag, .95), mean(n_flag >= obs_flag)))
cat(sprintf("  analytic: sd(ASMD) ~ %.3f at n = 19 vs 20, so P(|ASMD| > 0.10) ~ %.2f per covariate\n",
            sqrt(1/19 + 1/20), 2 * (1 - pnorm(0.10 / sqrt(1/19 + 1/20)))))
cat(sprintf("Nominally significant (p < .05): %d; after BH-FDR: %d; permutation p < .05: %d\n",
            sum(res$p < .05, na.rm = TRUE), sum(res$p_fdr < .05, na.rm = TRUE),
            sum(res$rand_p < .05, na.rm = TRUE)))

write.csv(res, file.path(DATA_DIR, "balance_education.csv"), row.names = FALSE)
cat(sprintf("\nSaved %s\n", file.path(DATA_DIR, "balance_education.csv")))
cat("Love plot: python3 upstream/figure_balance_education.py\n")
cat("Done: balance_check_education.R\n")
