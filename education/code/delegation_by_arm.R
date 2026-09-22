# ==============================================================================
# delegation_by_arm.R — delegation vs autonomy of student prompts across the two
# biased arms (NEW analysis; no reference-paper sibling). Consumes the LLM
# delegation annotations the way a3 consumes the engagement annotations, so the
# self-contained R folder turns EVERY annotated dataset into output.
#
# Question: does the DIRECTION of the AI's bias change how students hand work to
#   the assistant? Novelty-biased vs Reliability-biased arms, over five prompt
#   categories (full / partial delegation, guided iteration, question asking,
#   verification/other) and a 0-3 autonomy score. There is no default-arm
#   delegation data (annotation was scoped to the biased arms), so the contrast
#   is Novelty vs Reliability only.
#
# Design & inference (this is a SMALL experiment — 15 students, 7 vs 11
#   student-weeks, message counts of 2-376 per week):
#   PRIMARY  — the student-week is the randomization unit. Each student-week gets
#     one weight (equal), which is robust to a few heavy-usage weeks dominating
#     the pooled message stream. The arm difference (Reliability - Novelty) in
#     mean autonomy and in each category share is tested with an EXACT
#     permutation test (all C(n, n_nov) arm-label assignments enumerated) plus a
#     student-week bootstrap CI. No distributional or large-sample assumptions.
#   SECONDARY — message level, using the within-week richness: a mixed model for
#     autonomy (student random intercept) and a student-clustered logit per
#     category. Reported as triangulation; the effective N is 15 students, so
#     these are read as descriptive, not as the inferential backbone.
#   Caveat noted in output: 3 students appear in both arms across weeks; the
#   student-week permutation ignores that mild pairing (standard for
#   week-assigned treatment).
#
# Inputs : figures/delegation_annotations_v1.csv (message level, from
#          the upstream delegation-annotation step)
# Outputs: figures/d1_delegation_shares.csv, figures/d1_delegation_contrasts.csv,
#          figures/delegation_by_arm.png
#   setwd("education/code"); source("_setup.R"); source("delegation_by_arm.R")
# ==============================================================================
set.seed(123)
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(ggplot2)
  library(scales); library(patchwork); library(lme4); library(lmerTest)
  library(sandwich); library(lmtest)
})

# Resolve this script's own folder so data/ and figures/ resolve whether it is run
# via Rscript, sourced in RStudio, or pasted line-by-line from any working directory.
IN <- DATA_DIR; OUT <- FIG_DIR

# Route any implicit graphics (e.g. grid text measurement) to a throwaway PNG rather than
# the default pdf device (some R builds can't render custom fonts to pdf). Figures are PNG.

ANN_PATH <- file.path(IN, "delegation_annotations.csv")   # v2 adds the NEUTRAL arm (697 msgs, 5 student-weeks); biased-arm rows identical to v1
if (!file.exists(ANN_PATH)) {
  stop("data/delegation_annotations.csv not found.")
}

CATS <- c("full_delegation", "partial_delegation", "guided_iteration",
          "question_asking", "verification_other")
CAT_LAB <- c("Full delegation", "Partial delegation", "Guided iteration",
             "Question asking", "Verification / other")
# Neutral is included automatically once the upstream delegation-annotation step has been run on
# the NEUTRAL transcripts (895 messages, 5 student-weeks in the raw log; NOT yet
# annotated as of Jul 2026 -- the current file covers only the two biased arms).
ARMS <- c("Novelty", "Neutralized", "Reliability")   # display order; absent arms drop

# ========================================
# Load: keep annotated rows; real prompts exclude the residual not_a_prompt
# ========================================
ann <- read_csv(ANN_PATH, show_col_types = FALSE) %>%
  filter(status == "annotated") %>%
  mutate(arm = factor(dplyr::recode(policy_used, NOVELTY_BIASED = "Novelty",
                                    NEUTRAL = "Neutralized",
                                    RELIABILITY_BIASED = "Reliability"), levels = ARMS)) %>%
  filter(!is.na(arm)) %>%                      # any other policy (e.g. QUESTIONING) stays out
  mutate(arm = droplevels(arm),
         autonomy_score = as.numeric(autonomy_score),
         category = factor(category, levels = c(CATS, "not_a_prompt")))
prompts <- ann %>% filter(category != "not_a_prompt") %>% droplevels()
if (!"Neutralized" %in% levels(prompts$arm))
  cat("NOTE: no NEUTRAL rows in the annotation file yet -- figure shows biased arms only.\n")

cat(sprintf("Delegation annotations: %d messages (%d student-prompts), %d students, %d student-weeks\n",
            nrow(ann), nrow(prompts), n_distinct(ann$student_id),
            n_distinct(paste(ann$student_id, ann$week))))
cat("By arm (messages | student-weeks | students):\n")
print(ann %>% group_by(arm) %>%
        summarise(messages = n(), student_weeks = n_distinct(paste(student_id, week)),
                  students = n_distinct(student_id), .groups = "drop") %>% as.data.frame())

# Message-pooled shares (descriptive only — dominated by heavy-usage weeks)
cat("\n=== message-pooled category shares by arm (descriptive) ===\n")
print(prompts %>% count(arm, category) %>% group_by(arm) %>%
        mutate(share = n / sum(n)) %>% dplyr::select(-n) %>%
        pivot_wider(names_from = arm, values_from = share) %>% as.data.frame(), digits = 3)

# ========================================
# Student-week units (PRIMARY): equal-weight per week; drop weeks with 0 prompts
# ========================================
sw <- prompts %>%
  group_by(student_id, week, arm) %>%
  summarise(n_prompts = n(), mean_autonomy = mean(autonomy_score),
            !!!setNames(lapply(CATS, function(c) rlang::expr(mean(category == !!c))),
                        paste0("share_", CATS)), .groups = "drop")
cat(sprintf("\nStudent-week analysis units: %d (%s); each week weighted equally\n",
            nrow(sw), paste(names(table(sw$arm)), table(sw$arm), sep = " ", collapse = ", ")))

# ---- exact permutation test + bootstrap CI: Reliability - Novelty (primary) ----
# The two-arm inference below is unchanged when Neutralized rows exist: it runs on
# the biased-arm subset only. Neutral pairwise contrasts get their own block after.
sw_b <- sw %>% filter(arm %in% c("Novelty", "Reliability")) %>% mutate(arm = droplevels(arm))
arm_vec <- sw_b$arm
combos <- combn(nrow(sw_b), sum(arm_vec == "Novelty"))   # each column = weeks -> Novelty
cat(sprintf("Exact permutation: enumerating all %s arm-label assignments\n",
            format(ncol(combos), big.mark = ",")))

perm_boot <- function(vals) {
  obs <- mean(vals[arm_vec == "Reliability"]) - mean(vals[arm_vec == "Novelty"])
  diffs <- apply(combos, 2, function(idx) {
    is_nov <- logical(length(vals)); is_nov[idx] <- TRUE
    mean(vals[!is_nov]) - mean(vals[is_nov])
  })
  p_exact <- mean(abs(diffs) >= abs(obs) - 1e-12)
  nov <- vals[arm_vec == "Novelty"]; rel <- vals[arm_vec == "Reliability"]
  bs <- replicate(5000, mean(sample(rel, replace = TRUE)) - mean(sample(nov, replace = TRUE)))
  c(nov_mean = mean(nov), rel_mean = mean(rel), diff = obs,
    ci_lo = unname(quantile(bs, .025)), ci_hi = unname(quantile(bs, .975)), p_exact = p_exact)
}

metrics <- c(setNames(paste0("share_", CATS), CAT_LAB), c(`Mean autonomy (0-3)` = "mean_autonomy"))
prim <- lapply(names(metrics), function(lab) {
  r <- perm_boot(sw_b[[metrics[[lab]]]])
  data.frame(metric = lab, column = metrics[[lab]], t(r), row.names = NULL)
}) %>% bind_rows()
prim$p_fdr <- p.adjust(prim$p_exact, "fdr")
cat("\n=== PRIMARY: student-week arm contrast (Reliability - Novelty), exact permutation ===\n")
print(prim %>% mutate(across(where(is.numeric), ~round(., 3))) %>% as.data.frame(), row.names = FALSE)

# ---- Neutralized pairwise contrasts (exact permutation), once annotations exist ----
if ("Neutralized" %in% levels(sw$arm)) {
  perm_pair <- function(dat, a, b, col) {          # mean(a) - mean(b), two-sided exact
    v <- dat[[col]][dat$arm %in% c(a, b)]; lab <- droplevels(dat$arm[dat$arm %in% c(a, b)])
    cmb <- combn(length(v), sum(lab == a))
    obs <- mean(v[lab == a]) - mean(v[lab == b])
    dd <- apply(cmb, 2, function(i) { z <- logical(length(v)); z[i] <- TRUE
                                      mean(v[z]) - mean(v[!z]) })
    c(diff = obs, p_exact = mean(abs(dd) >= abs(obs) - 1e-12))
  }
  cat("\n=== NEUTRALIZED pairwise (exact permutation; full delegation + autonomy) ===\n")
  for (other in intersect(c("Novelty", "Reliability"), levels(sw$arm)))
    for (cl in c("share_full_delegation", "mean_autonomy")) {
      r <- perm_pair(sw, "Neutralized", other, cl)
      cat(sprintf("  Neutralized - %-11s %-22s diff %+0.3f  p = %.3f\n", other, cl, r[1], r[2]))
    }
}

# per-arm share table with student-week SE (for the figure + a shares CSV)
shares_tbl <- sw %>%
  pivot_longer(all_of(paste0("share_", CATS)), names_to = "column", values_to = "share") %>%
  mutate(category = factor(CAT_LAB[match(sub("^share_", "", column), CATS)], levels = CAT_LAB)) %>%
  group_by(arm, category) %>%
  summarise(mean_share = mean(share), se = sd(share) / sqrt(n()), n_weeks = n(), .groups = "drop")
write_csv(shares_tbl, file.path(OUT, "d1_delegation_shares.csv"))

# ========================================
# SECONDARY (message level): mixed autonomy + student-clustered logit per category
# ========================================
cat("\n=== SECONDARY: message-level models (effective N = students; triangulation) ===\n")
m_aut <- lmer(autonomy_score ~ arm + (1 | student_id), data = prompts)
aut_fx <- as.data.frame(coef(summary(m_aut)))["armReliability", ]
m_aut_ols <- lm(autonomy_score ~ arm, data = prompts)
aut_cl <- coeftest(m_aut_ols, vcov = vcovCL(m_aut_ols, cluster = prompts$student_id))["armReliability", ]
cat(sprintf("Autonomy ~ arm: mixed(1|student) Reliability-Novelty = %.3f (SE %.3f, p %.3f) | student-clustered OLS p %.3f\n",
            aut_fx[["Estimate"]], aut_fx[["Std. Error"]], aut_fx[["Pr(>|t|)"]], aut_cl[["Pr(>|t|)"]]))

logit_rows <- lapply(seq_along(CATS), function(i) {
  prompts$.y <- as.integer(prompts$category == CATS[i])
  m <- glm(.y ~ arm, family = binomial, data = prompts)
  ct <- coeftest(m, vcov = vcovCL(m, cluster = prompts$student_id))["armReliability", ]
  data.frame(metric = CAT_LAB[i], model = "clustered logit (student)",
             log_odds = ct[["Estimate"]], odds_ratio = exp(ct[["Estimate"]]),
             se = ct[["Std. Error"]], p_value = ct[["Pr(>|z|)"]])
}) %>% bind_rows()
logit_rows$p_fdr <- p.adjust(logit_rows$p_value, "fdr")
cat("Per-category student-clustered logit (Reliability vs Novelty):\n")
print(logit_rows %>% mutate(across(where(is.numeric), ~round(., 3))) %>% as.data.frame(), row.names = FALSE)

# contrasts CSV: primary permutation rows + secondary model rows, one tidy table
contrasts_out <- bind_rows(
  prim %>% transmute(metric, level = "student-week (primary)", model = "exact permutation",
                     estimate = diff, ci_lo, ci_hi, p_value = p_exact, p_fdr),
  logit_rows %>% transmute(metric, level = "message (secondary)", model,
                           estimate = log_odds, ci_lo = NA_real_, ci_hi = NA_real_,
                           p_value, p_fdr),
  data.frame(metric = "Mean autonomy (0-3)", level = "message (secondary)",
             model = "mixed (1|student)", estimate = aut_fx[["Estimate"]],
             ci_lo = NA_real_, ci_hi = NA_real_, p_value = aut_fx[["Pr(>|t|)"]],
             p_fdr = NA_real_))
write_csv(contrasts_out, file.path(OUT, "d1_delegation_contrasts.csv"))

# ========================================
# Figure: (A) stacked category shares by arm, (B) mean autonomy by arm
# ========================================
nature_theme <- theme_classic() +
  theme(text = element_text(family = FONT, size = 8),
        axis.title = element_text(family = FONT, size = 9),
        axis.text = element_text(family = FONT, size = 8, color = "black"),
        axis.line = element_line(color = "black", linewidth = 0.5),
        axis.ticks = element_line(color = "black", linewidth = 0.5),
        panel.grid = element_blank(),
        plot.margin = margin(t = 10, r = 14, b = 6, l = 8))

# delegation (hand it over, red) -> ownership/inquiry (blue)
CAT_COL <- setNames(c("#E66101", "#FDB863", "#FEE0B6", "#B2ABD2", "#5E3C99"), CAT_LAB)  # PuOr: delegation orange -> ownership purple

# Vertical layout: arms on the x-axis, category shares stacked on the y-axis.
# Single panel -- the overall mean-autonomy comparison is dropped from the figure
# (still computed above and kept in d1_delegation_contrasts.csv for reference).
fig <- ggplot(shares_tbl, aes(x = arm, y = mean_share, fill = category)) +
  geom_col(width = 0.7, color = "white", linewidth = 0.3, alpha = 0.8) +
  scale_fill_manual(values = CAT_COL, name = NULL) +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.02)),
                     name = "Share of student prompts") +
  labs(x = NULL) +
  nature_theme + theme(legend.position = "none")

save_png(file.path(OUT, "delegation_by_arm.png"), fig, width = 2.6, height = 3.4, dpi = 500)

fig
cat("\nSaved d1_delegation_shares.csv, d1_delegation_contrasts.csv, delegation_by_arm.png in", OUT, "\n")
cat("Done: delegation_by_arm.R\n")
