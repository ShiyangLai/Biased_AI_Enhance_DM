# final_project_models.R — final-project (end-of-course) analyses.
#
# Replicates the report's final-grades section from two pseudonymous inputs:
#   data/final_project_student.csv  one row per analysis student (N = 39):
#       human grades (presentation / essay / overall = mean of the two),
#       AI-grader essay scores (GPT, Gemini, 3-grader mean), assigned-arm
#       indicators (got_nov/got_rel/got_neu/got_ques; every student was assigned
#       two of the four treatment arms), the randomized biased-week dose
#       (n_biased_assigned, 0-2), realized usage from the proxy transcript log
#       (prompts / session minutes, biased-arm and all-arm, plus week 1-3/4-6/7-9
#       splits), weekly-performance transfer predictors, engagement/delegation
#       means (users only), embedding distinctiveness and LLM innovativeness of
#       the student's project, and baseline preference z-scores. doc_code links
#       students sharing one project (team projects are graded once).
#   data/final_project_docs.csv    one row per unique project (24 graded):
#       team size, word/visual counts, embedding distinctiveness (novelty_z),
#       LLM innovativeness ratings, essay scores, member-mean exposures.
#   data/pre_survey_covariates_student.csv  baseline covariates (for M2/M2p).
#
# Models mirror the report: M1 = simple OLS; M2p = the PREREGISTERED end-of-course
# specification (covariates = pre-class ML + programming skill); M2 = top-5
# four-arm imbalanced pre-survey covariates; all with HC3 robust SEs.
# Outputs: figures/final_project_models.csv, figures/final_project_correlations.csv,
#          figures/final_project_doc_correlations.csv (+ console summary).

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr)
  library(sandwich); library(lmtest)
})

IN <- DATA_DIR; OUT <- FIG_DIR
stu  <- read_csv(file.path(IN, "final_project_student.csv"), show_col_types = FALSE)
docs <- read_csv(file.path(IN, "final_project_docs.csv"), show_col_types = FALSE)
pre  <- read_csv(file.path(IN, "pre_survey_covariates_student.csv"), show_col_types = FALSE)

M2P_COVS <- c("prior machine learning / AI programming experience?",
              "prior programming experience in general?")
M2_COVS  <- c(M2P_COVS[1],
              "I would rather have an assistant propose several candidate strategies (with pros/cons) than one answer.",
              M2P_COVS[2],
              "I expect AI tools to help me learn, not just finish tasks faster.",
              "Roughly how many total hours did you use AI assistants in the last 7 days?")
stopifnot(all(M2_COVS %in% names(pre)))
d <- stu %>% left_join(pre %>% dplyr::select(student_id, all_of(M2_COVS)), by = "student_id") %>%
  mutate(log_prompts_biased = log1p(prompts_biased),
         log_minutes_biased = log1p(minutes_biased),
         log_prompts_total  = log1p(prompts_total),
         log_minutes_total  = log1p(minutes_total),
         log_minutes_early  = log1p(minutes_early),
         log_minutes_mid    = log1p(minutes_mid),
         log_minutes_late   = log1p(minutes_late))
# median-impute covariates (same rule as the python pipeline's encoder)
for (cv in M2_COVS) d[[cv]][is.na(d[[cv]])] <- median(d[[cv]], na.rm = TRUE)

OUTCOMES  <- c("presentation", "essay", "overall")
EXPOSURES <- c("n_biased_assigned", "any_biased_use", "log_prompts_biased",
               "log_minutes_biased", "log_prompts_total", "log_minutes_total")

hc3 <- function(fit) coeftest(fit, vcov = vcovHC(fit, type = "HC3"))

# ── 1. Correlation grid (Table 69) ───────────────────────────────────────────
corr <- expand.grid(outcome = OUTCOMES, exposure = EXPOSURES,
                    stringsAsFactors = FALSE) %>%
  rowwise() %>%
  mutate(pearson_r  = cor(d[[exposure]], d[[outcome]]),
         pearson_p  = cor.test(d[[exposure]], d[[outcome]])$p.value,
         spearman   = cor(d[[exposure]], d[[outcome]], method = "spearman"),
         n = sum(complete.cases(d[[exposure]], d[[outcome]]))) %>%
  ungroup() %>%
  mutate(q_bh = p.adjust(pearson_p, method = "BH"))
write_csv(corr, file.path(OUT, "final_project_correlations.csv"))

# ── 2. Exposure models M1 / M2p / M2 (Tables 70-71) + AI outcomes (Table 78) ──
bt <- function(v) if (length(v) == 0) character(0) else paste0("`", v, "`")
fit_row <- function(outcome, rhs_terms, covs, model_id, keep_terms) {
  f <- as.formula(paste(bt(outcome), "~",
                        paste(c(rhs_terms, bt(covs)), collapse = " + ")))
  fit <- lm(f, data = d)
  ct <- hc3(fit)
  out <- lapply(intersect(keep_terms, rownames(ct)), function(t)
    tibble(outcome = outcome, model = model_id, term = t,
           coef = ct[t, 1], se = ct[t, 2], p = ct[t, 4], n = nobs(fit)))
  list(rows = bind_rows(out), fit = fit)
}

rows <- list()
AI_OUTCOMES <- intersect(c("essay_gpt", "essay_gemini", "essay_3grader"), names(d))
for (out in c(OUTCOMES, AI_OUTCOMES)) {
  for (e in EXPOSURES) {
    for (m in list(c("M1", ""), c("M2p", "p"), c("M2", "f"))) {
      covs <- switch(m[2], "p" = M2P_COVS, "f" = M2_COVS, character(0))
      rows[[length(rows) + 1]] <- fit_row(out, e, covs, m[1], e)$rows
    }
  }
}

# ITT: three assigned-arm indicators (questioning-oriented omitted), joint +
# novelty-vs-reliability Wald tests (the preregistered contrasts)
itt_terms <- c("got_nov", "got_rel", "got_neu")
for (out in OUTCOMES) {
  for (m in list(c("M1", ""), c("M2p", "p"), c("M2", "f"))) {
    covs <- switch(m[2], "p" = M2P_COVS, "f" = M2_COVS, character(0))
    fr <- fit_row(out, itt_terms, covs, m[1], itt_terms)
    rows[[length(rows) + 1]] <- fr$rows
    V <- vcovHC(fr$fit, type = "HC3")
    b <- coef(fr$fit)
    joint <- tryCatch({  # Wald chi2 for got_nov = got_rel = got_neu = 0
      R <- matrix(0, 3, length(b), dimnames = list(NULL, names(b)))
      R[1, "got_nov"] <- 1; R[2, "got_rel"] <- 1; R[3, "got_neu"] <- 1
      w <- t(R %*% b) %*% solve(R %*% V %*% t(R)) %*% (R %*% b)
      pchisq(as.numeric(w), df = 3, lower.tail = FALSE)
    }, error = function(e) NA)
    nvr <- tryCatch({    # got_nov = got_rel
      r1 <- rep(0, length(b)); names(r1) <- names(b)
      r1["got_nov"] <- 1; r1["got_rel"] <- -1
      w <- (sum(r1 * b))^2 / as.numeric(t(r1) %*% V %*% r1)
      pchisq(w, df = 1, lower.tail = FALSE)
    }, error = function(e) NA)
    rows[[length(rows) + 1]] <- tibble(
      outcome = out, model = m[1], term = c("joint_wald", "nov_vs_rel_wald"),
      coef = NA, se = NA, p = c(joint, nvr), n = nobs(fr$fit))
  }
}

# H3 moderation: assigned arm x baseline exploration-exploitation preference
if (all(c("novelty_pref_z", "reliability_pref_z") %in% names(d))) {
  for (out in OUTCOMES) {
    f <- as.formula(paste(bt(out), "~ got_nov * novelty_pref_z + got_rel * reliability_pref_z"))
    fit <- lm(f, data = d)
    ct <- hc3(fit)
    keep <- c("got_nov", "novelty_pref_z", "got_nov:novelty_pref_z",
              "got_rel", "reliability_pref_z", "got_rel:reliability_pref_z")
    rows[[length(rows) + 1]] <- bind_rows(lapply(intersect(keep, rownames(ct)), function(t)
      tibble(outcome = out, model = "H3", term = t,
             coef = ct[t, 1], se = ct[t, 2], p = ct[t, 4], n = nobs(fit))))
  }
}
models <- bind_rows(rows)
write_csv(models, file.path(OUT, "final_project_models.csv"))

# ── 3. Project-level content measures (Tables 79 + innovativeness) ────────────
doc_preds <- intersect(c("n_biased_mean", "log_min_biased_mean", "log_min_total_mean",
                         "human_essay", "gpt_essay", "gemini_essay"), names(docs))
doc_outs  <- intersect(c("novelty_z", "innov_mean"), names(docs))
dc <- expand.grid(outcome = doc_outs, predictor = doc_preds, stringsAsFactors = FALSE) %>%
  rowwise() %>%
  mutate(n = sum(complete.cases(docs[[predictor]], docs[[outcome]])),
         pearson_r = ifelse(n >= 5, cor(docs[[predictor]], docs[[outcome]],
                                        use = "complete.obs"), NA),
         pearson_p = ifelse(n >= 5, cor.test(docs[[predictor]], docs[[outcome]])$p.value, NA),
         spearman  = ifelse(n >= 5, cor(docs[[predictor]], docs[[outcome]],
                                        method = "spearman", use = "complete.obs"), NA)) %>%
  ungroup() %>%
  group_by(outcome) %>% mutate(q_bh = p.adjust(pearson_p, method = "BH")) %>% ungroup()
write_csv(dc, file.path(OUT, "final_project_doc_correlations.csv"))

# ── Console summary ───────────────────────────────────────────────────────────
cat("\n== d1 final-project replication ==\n")
cat(sprintf("students: %d | projects: %d\n", nrow(d), nrow(docs)))
cat("\nPreregistered ITT (M2p) essay coefficients:\n")
print(models %>% filter(model == "M2p", outcome == "essay",
                        term %in% c(itt_terms, "joint_wald", "nov_vs_rel_wald")) %>%
        mutate(across(where(is.numeric), ~round(., 3))), n = 10)
cat("\nUsage-essay headline (log_minutes_total):\n")
print(models %>% filter(term == "log_minutes_total", outcome == "essay") %>%
        mutate(across(where(is.numeric), ~round(., 3))), n = 5)
cat("\nProject distinctiveness vs member-mean biased minutes:\n")
print(dc %>% filter(outcome == "novelty_z", predictor == "log_min_biased_mean") %>%
        mutate(across(where(is.numeric), ~round(., 3))))
cat("\nDone. Outputs: output/final_project_models.csv, final_project_correlations.csv, final_project_doc_correlations.csv\n")
