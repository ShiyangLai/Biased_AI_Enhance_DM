# ==============================================================================
# perceived_spec_check.R — EXPLORATORY specification multiverse for perceived
# improvement (bm's panel 3), asking whether any arm pairwise — especially
# Reliability vs Novelty — is significant under alternative model choices.
#
# READ THIS BEFORE INTERPRETING: perceived_performance is observed for only ~91
# student-weeks, of which ~13 are in the biased arms (Novelty 7, Reliability 6).
# At that size the fit is prior-/spec-sensitive and a pairwise test has very low
# power. Running many specifications and keeping the significant one is a garden
# of forking paths: the point here is the OPPOSITE — show every spec together so
# the robustness (or fragility) of any effect is visible. Treat a lone p<.05
# among these as hypothesis-generating, not confirmatory.
#
# Specs (all use arm = Default / Novelty / Reliability; contrasts on each model's
# natural scale — latent for ordinal, response for linear):
#   A  MCMCglmm ordinal, student RE, week FE + prior covariates   (bm's spec)
#   B  clmm     ordinal, student RE, week FE + prior covariates    (frequentist twin of A)
#   C  clm      ordinal, no RE,      week FE + prior covariates
#   D  clm      ordinal, no RE,      arm only                      (simplest / most power)
#   E  lmer     linear,  student RE, week FE + prior covariates    (score as interval)
#   F  lm       linear,  arm only, student-clustered SE
#   G  biased-only Reliability vs Novelty: Wilcoxon + t + arm-only clm
#
# Inputs : data/bm_student_week.csv
# Outputs: figures/perceived_spec_check.csv  (+ console table)
#   setwd("education/code"); source("_setup.R"); source("perceived_spec_check.R")
# ==============================================================================
set.seed(123)
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(emmeans)
  library(ordinal); library(lme4); library(lmerTest)
  library(MCMCglmm); library(sandwich)
})

# Resolve this script's own folder so data/ and figures/ resolve whether it is run
# via Rscript, sourced in RStudio, or pasted line-by-line from any working directory.
IN <- DATA_DIR; OUT <- FIG_DIR

LV <- c("Default", "Novelty", "Reliability")
d <- read_csv(file.path(IN, "bm_student_week.csv"), show_col_types = FALSE) %>%
  mutate(arm = factor(dplyr::recode(direction, None = "Default",
                                    Novelty = "Novelty", Reliability = "Reliability"), levels = LV),
         week = factor(week), student_id = as.factor(student_id))
for (cv in c("prior_prog_exp", "prior_ml_exp")) {
  d[[cv]] <- as.numeric(d[[cv]]); d[[cv]][is.na(d[[cv]])] <- median(d[[cv]], na.rm = TRUE)
}
dpv <- d %>% filter(!is.na(perceived_performance))
dpv$yo <- factor(dpv$perceived_performance, ordered = TRUE)   # ordinal outcome
dpv$y  <- as.numeric(dpv$perceived_performance)               # linear outcome
cat("Perceived improvement observations by arm:\n"); print(table(dpv$arm))

# tidy the 3 pairwise contrasts of an emmeans grid, with within-spec FDR
pw_tbl <- function(emm, spec, type, n) {
  pw <- suppressWarnings(as.data.frame(summary(pairs(emm, adjust = "none"), infer = TRUE)))
  pcol <- grep("^p\\.value$", names(pw), value = TRUE)
  data.frame(spec = spec, type = type, n = n, contrast = as.character(pw$contrast),
             estimate = pw$estimate, se = pw$SE, p_raw = pw[[pcol]],
             p_fdr = p.adjust(pw[[pcol]], "fdr"), row.names = NULL)
}
try_spec <- function(expr, spec) tryCatch(expr, error = function(e) {
  cat(sprintf("  [%s] FAILED: %s\n", spec, conditionMessage(e))); NULL })

res <- list()

# ── A. MCMCglmm ordinal (bm's spec): posterior latent pairwise ────────────────
res$A <- try_spec({
  prior <- list(R = list(V = 1, fix = 1),
                G = list(G1 = list(V = 1, nu = 1, alpha.mu = 0, alpha.V = 25^2)))
  mm <- MCMCglmm(yo ~ arm + week + prior_prog_exp + prior_ml_exp, random = ~student_id,
                 family = "ordinal", nitt = 55000, thin = 25, burnin = 5000,
                 prior = prior, data = as.data.frame(dpv), verbose = FALSE)
  P <- as.matrix(mm$Sol); cn <- colnames(P)
  mw <- names(sort(table(dpv$week), decreasing = TRUE))[1]
  shift <- P[, "prior_prog_exp"] * mean(dpv$prior_prog_exp) +
           P[, "prior_ml_exp"] * mean(dpv$prior_ml_exp)
  M <- sapply(LV, function(lv) {
    lp <- P[, "(Intercept)"] + shift
    ce <- paste0("arm", lv); if (ce %in% cn) lp <- lp + P[, ce]
    we <- paste0("week", mw); if (we %in% cn) lp <- lp + P[, we]
    lp })
  rows <- list(); m <- 0
  for (i in 1:2) for (j in (i + 1):3) { m <- m + 1
    ds <- M[, i] - M[, j]; pv <- min(1, 2 * min(mean(ds < 0), mean(ds > 0)))
    rows[[m]] <- data.frame(contrast = paste(LV[i], "-", LV[j]),
                            estimate = mean(ds), se = sd(ds), p_raw = pv) }
  rows <- do.call(rbind, rows)
  data.frame(spec = "A. MCMCglmm ordinal + RE + week + priors", type = "ordinal-Bayes",
             n = nrow(dpv), rows, p_fdr = p.adjust(rows$p_raw, "fdr"), row.names = NULL)
}, "A")

# ── B-E. emmeans-based specs ──────────────────────────────────────────────────
res$B <- try_spec(pw_tbl(emmeans(clmm(yo ~ arm + week + prior_prog_exp + prior_ml_exp + (1 | student_id),
                                      data = dpv), ~ arm),
                         "B. clmm ordinal + RE + week + priors", "ordinal-clmm", nrow(dpv)), "B")
res$C <- try_spec(pw_tbl(emmeans(clm(yo ~ arm + week + prior_prog_exp + prior_ml_exp, data = dpv), ~ arm),
                         "C. clm ordinal + week + priors", "ordinal-clm", nrow(dpv)), "C")
res$D <- try_spec(pw_tbl(emmeans(clm(yo ~ arm, data = dpv), ~ arm),
                         "D. clm ordinal, arm only", "ordinal-clm", nrow(dpv)), "D")
res$E <- try_spec(pw_tbl(emmeans(lmer(y ~ arm + week + prior_prog_exp + prior_ml_exp + (1 | student_id),
                                      data = dpv), ~ arm),
                         "E. linear mixed + RE + week + priors", "linear-lmer", nrow(dpv)), "E")

# ── F. lm arm-only with student-clustered SE ──────────────────────────────────
res$F <- try_spec({
  mF <- lm(y ~ arm, data = dpv)
  emmF <- emmeans(mF, ~ arm, vcov. = vcovCL(mF, cluster = dpv$student_id))
  pw_tbl(emmF, "F. linear, arm only, student-clustered SE", "linear-lm", nrow(dpv))
}, "F")

# ── G. biased-only, Reliability vs Novelty (drop Default) ─────────────────────
res$G <- try_spec({
  db <- dpv %>% filter(arm != "Default") %>% mutate(arm = droplevels(arm))
  n_b <- nrow(db)
  w <- suppressWarnings(wilcox.test(y ~ arm, data = db))
  tt <- t.test(y ~ arm, data = db)
  cl <- pw_tbl(emmeans(clm(yo ~ arm, data = db), ~ arm),
               "G. biased-only clm (arm)", "ordinal-clm", n_b)
  rbind(
    data.frame(spec = "G. biased-only Wilcoxon", type = "rank test", n = n_b,
               contrast = "Novelty - Reliability", estimate = NA_real_, se = NA_real_,
               p_raw = w$p.value, p_fdr = w$p.value),
    data.frame(spec = "G. biased-only Welch t", type = "linear", n = n_b,
               contrast = "Novelty - Reliability",
               estimate = unname(diff(rev(tt$estimate))), se = NA_real_,
               p_raw = tt$p.value, p_fdr = tt$p.value),
    cl)
}, "G")

all <- bind_rows(res)
cat("\n=== ALL SPECIFICATIONS: arm pairwise contrasts (perceived improvement) ===\n")
print(as.data.frame(all %>% mutate(across(where(is.numeric), ~round(., 3)))), row.names = FALSE)

cat("\n=== FOCUS: Reliability vs Novelty across specifications ===\n")
cat("(estimate sign is Novelty - Reliability; positive = Novelty perceives more)\n")
rn <- all %>% filter(grepl("Novelty - Reliability|Reliability - Novelty", contrast)) %>%
  mutate(sig_raw = ifelse(p_raw < .05, "*", ""),
         across(c(estimate, se, p_raw, p_fdr), ~round(., 3)))
print(as.data.frame(rn %>% dplyr::select(spec, type, n, estimate, p_raw, sig_raw)), row.names = FALSE)
cat(sprintf("\n%d of %d specifications reach raw p<.05 for Reliability vs Novelty.\n",
            sum(rn$p_raw < .05, na.rm = TRUE), nrow(rn)))
cat("None of these pairwise p-values are corrected across specifications; with ~13 biased\n",
    "observations, read this as exploratory robustness, not confirmation.\n")

write_csv(all, file.path(OUT, "perceived_spec_check.csv"))
cat("\nSaved output/perceived_spec_check.csv\n")
cat("Done: perceived_spec_check.R\n")
