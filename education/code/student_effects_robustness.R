# ==============================================================================
# student_effects_robustness.R — reports the bm treatment contrasts with a STUDENT
# EFFECT added, since treatment is sprinkled WITHIN students (every student has
# Default weeks; 24 of 39 also have biased weeks; none are biased-only). Three
# specs per outcome:
#   S1  clustered SE (bm's current):  lm(y ~ arm + week + prior_prog + prior_ml),
#                                     student-clustered SE  — no student effect in the mean
#   S2  student RE:                   lmer(y ~ arm + week + prior_prog + prior_ml + (1|student))
#   S3  student FE:                   lm(y ~ arm + week + factor(student)),
#                                     student-clustered SE  — WITHIN estimator
#
# S3 (FE) is each student as their own control: it subtracts every time-invariant
# student trait, so prior_prog_exp/prior_ml_exp (and any unobserved student
# covariate) are absorbed and drop from the model — this is the strong answer to
# the covariate-imbalance question. Continuous outcomes only (perceived is also
# shown on its 1-7 scale as a linear approximation so FE/RE are comparable; its
# primary model stays the ordinal MCMC-RE in bm).
#
# Inputs : data/bm_student_week.csv, data/scores_all_graders_student_week.csv
# Outputs: figures/bm_student_effects.csv  (+ console tables)
#   setwd("education/code"); source("_setup.R"); source("student_effects_robustness.R")
# ==============================================================================
set.seed(123)
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(emmeans)
  library(lme4); library(lmerTest); library(sandwich); library(lmtest)
})

# Resolve this script's own folder so data/ and figures/ resolve whether it is run
# via Rscript, sourced in RStudio, or pasted line-by-line from any working directory.
IN <- DATA_DIR; OUT <- FIG_DIR

LV <- c("Default", "Novelty", "Reliability")
grades <- read_csv(file.path(IN, "scores_all_graders_student_week.csv"), show_col_types = FALSE) %>%
  dplyr::select(student_id, week, combined_gemini_asgn_score)
d <- read_csv(file.path(IN, "bm_student_week.csv"), show_col_types = FALSE) %>%
  left_join(grades, by = c("student_id", "week")) %>%
  mutate(arm = factor(dplyr::recode(direction, None = "Default",
                                    Novelty = "Novelty", Reliability = "Reliability"), levels = LV),
         week = factor(week), student_id = as.factor(student_id),
         conv_round_log = log1p(conv_round))
for (cv in c("prior_prog_exp", "prior_ml_exp")) {
  d[[cv]] <- as.numeric(d[[cv]]); d[[cv]][is.na(d[[cv]])] <- median(d[[cv]], na.rm = TRUE) }

OUTS <- list(
  coding    = list(col = "combined_gemini_asgn_score", pos = TRUE,  label = "Coding (Gemini+human)"),
  turns     = list(col = "conv_round_log",             pos = FALSE, label = "Reply turns (log 1+x)", conv = TRUE),
  perceived = list(col = "perceived_performance",      pos = FALSE, label = "Perceived improve (1-7, linear)"),
  memo      = list(col = "combined_all_memo_score",    pos = TRUE,  label = "Memo (3-grader)"))

pw_tbl <- function(emm, outcome, spec, ysd) {
  pw <- suppressWarnings(as.data.frame(summary(pairs(emm, adjust = "none"), infer = TRUE)))
  pcol <- grep("^p\\.value$", names(pw), value = TRUE)
  data.frame(outcome = outcome, spec = spec, contrast = as.character(pw$contrast),
             estimate = pw$estimate, se = pw$SE, std_effect = pw$estimate / ysd,
             p_raw = pw[[pcol]], p_fdr = p.adjust(pw[[pcol]], "fdr"), row.names = NULL)
}

all_rows <- list(); icc_rows <- list()
for (nm in names(OUTS)) {
  o <- OUTS[[nm]]; col <- o$col
  dat <- d[!is.na(d[[col]]), ]
  if (isTRUE(o$pos)) dat <- dat[dat[[col]] > 0, ]
  if (isTRUE(o$conv)) dat <- dat[dat$conv_round > 0, ]
  dat <- droplevels(dat)
  ysd <- sd(dat[[col]], na.rm = TRUE)
  n_both <- dat %>% group_by(student_id) %>%
    summarise(b = ("Default" %in% arm) && any(arm != "Default"), .groups = "drop") %>%
    summarise(sum(b)) %>% pull()
  cat(sprintf("\n#### %s — n=%d, students=%d (%d identify the within-student contrast) ####\n",
              o$label, nrow(dat), nlevels(dat$student_id), n_both))

  # S1 clustered SE (bm current)
  m1 <- lm(as.formula(paste(col, "~ arm + week + prior_prog_exp + prior_ml_exp")), data = dat)
  e1 <- emmeans(m1, ~ arm, vcov. = vcovCL(m1, cluster = dat$student_id))
  all_rows[[paste(nm, 1)]] <- pw_tbl(e1, o$label, "S1 clustered SE (current)", ysd)

  # S2 student RE
  m2 <- lmer(as.formula(paste(col, "~ arm + week + prior_prog_exp + prior_ml_exp + (1|student_id)")),
             data = dat)
  all_rows[[paste(nm, 2)]] <- pw_tbl(emmeans(m2, ~ arm), o$label, "S2 student RE", ysd)
  vc <- as.data.frame(VarCorr(m2)); icc <- vc$vcov[vc$grp == "student_id"] / sum(vc$vcov)
  icc_rows[[nm]] <- data.frame(outcome = o$label, icc_student = round(icc, 3))

  # S3 student FE (within estimator; priors absorbed by student dummies)
  m3 <- lm(as.formula(paste(col, "~ arm + week + factor(student_id)")), data = dat)
  e3 <- emmeans(m3, ~ arm, vcov. = vcovCL(m3, cluster = dat$student_id))
  all_rows[[paste(nm, 3)]] <- pw_tbl(e3, o$label, "S3 student FE", ysd)
}
all <- bind_rows(all_rows)

cat("\n\n===== STUDENT-EFFECT SPECS: all arm pairwise (FDR within outcome x spec) =====\n")
print(as.data.frame(all %>% mutate(across(where(is.numeric), ~round(., 3)))), row.names = FALSE)

cat("\n===== FOCUS: Default vs each biased arm (std_effect = estimate / outcome SD) =====\n")
foc <- all %>% filter(grepl("Default", contrast)) %>%
  mutate(flip = grepl("^\\s*Default", contrast),
         estimate = ifelse(flip, -estimate, estimate), std_effect = ifelse(flip, -std_effect, std_effect),
         biased = trimws(gsub("Default|-", "", contrast)),
         comparison = paste0(biased, " vs Default")) %>%
  group_by(outcome, spec) %>% mutate(p_fdr = p.adjust(p_raw, "fdr")) %>% ungroup() %>%
  dplyr::select(outcome, spec, comparison, estimate, std_effect, p_raw, p_fdr) %>%
  mutate(sig = ifelse(p_fdr < .05, "*", ""))
print(as.data.frame(foc %>% mutate(across(where(is.numeric), ~round(., 3)))), row.names = FALSE)

cat("\n===== FOCUS: Reliability vs Novelty =====\n")
rn <- all %>% filter(grepl("Novelty - Reliability", contrast)) %>%
  mutate(across(c(estimate, se, std_effect, p_raw, p_fdr), ~round(., 3))) %>%
  dplyr::select(outcome, spec, estimate, std_effect, p_raw)
print(as.data.frame(rn), row.names = FALSE)

cat("\nStudent-RE intraclass correlations:\n"); print(bind_rows(icc_rows), row.names = FALSE)
write_csv(all, file.path(OUT, "bm_student_effects.csv"))
cat("\nSaved output/bm_student_effects.csv\nDone: student_effects_robustness.R\n")
