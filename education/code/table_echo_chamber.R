# ==============================================================================
# table_echo_chamber.R — regression table for the echo-chamber vs
# oppositional-bias contrast (Fig. 2g), CODING grade only, in the layout of the
# fact-checking / investment SI tables and of table_performance_three_arm.R.
#
# Sample: biased-arm student-weeks (novelty or reliability) whose student has a
# defined pre-survey lean (direction_score != 0), with a submitted coding
# assignment. "Echo-chamber" = the assigned bias matches the student's lean;
# "Oppositional" = it opposes it. Reference category = Oppositional, so the
# Echo-chamber coefficient equals the Same - Opposite contrast reported in the
# main text. Lean reference = Novel-leaning.
#
# Models (all OLS with student-clustered SE unless noted; week FE throughout):
#   M1  Echo-chamber only                  (pooled BiasSide main effect)
#   M2  Echo-chamber x Lean                (primary; the Fig. 2g four-cell model)
#   M3  M2 + prior-experience covariates   (c1 'dominant_sign_cov')
#   M4  M2 on |direction_score| >= 0.5     (drops near-neutral leans; c1 band)
#   M5  M2 weighted by assignment volume   (effective_source_chars; pre-treatment)
# No student fixed or random effects: the sample is near-cross-sectional (23
# students for 27 rows), so a student FE absorbs the contrast and a random
# intercept is degenerate -- fitted, it takes 95% of the variance and collapses the
# residual SD to 0.20, which is why bias_side_performance.R also omits it.
# The 'marginal' row is the equal-weight Echo - Oppositional contrast over the two
# Lean cells from each interaction model (emmeans, clustered vcov); for M2 it is the
# Same - Opposite value quoted in the main text and drawn in Fig. 2g.
#
# Outputs: figures/table_echo_chamber.md, figures/table_echo_chamber.rds
#   setwd("education/code"); source("_setup.R"); source("table_echo_chamber.R")
# ==============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(sandwich); library(lmtest); library(emmeans)
})
IN <- DATA_DIR; OUT <- FIG_DIR

cw <- read_csv(file.path(IN,"c1_echo_student_week.csv"), show_col_types=FALSE) %>%
  left_join(read_csv(file.path(IN,"coding_regrade_student_week.csv"), show_col_types=FALSE),
            by=c("student_id","week")) %>%
  left_join(read_csv(file.path(IN,"scores_all_graders_student_week.csv"), show_col_types=FALSE) %>%
              dplyr::select(student_id, week, gem_asgn = gemini_asgn_score), by=c("student_id","week")) %>%
  left_join(read_csv(file.path(IN,"week_workload_summary.csv"), show_col_types=FALSE), by="week")
GEM_SHIFT <- mean(cw$coding_regrade[!is.na(cw$gem_asgn)], na.rm=TRUE) -
             mean(cw$gem_asgn[!is.na(cw$coding_regrade)], na.rm=TRUE)
cw$coding <- ifelse(is.na(cw$gem_asgn), cw$coding_regrade, (2*cw$coding_regrade + cw$gem_asgn + GEM_SHIFT)/3)
cw$coding[is.na(cw$coding_regrade)] <- NA
for (cv in c("prior_prog_exp","prior_ml_exp")) {
  cw[[cv]] <- as.numeric(cw[[cv]]); cw[[cv]][is.na(cw[[cv]])] <- median(cw[[cv]], na.rm=TRUE) }

rep <- cw %>%
  filter(is_default_reference == 0, !is.na(lean), bias_side_dominant %in% c("Same","Opposite"),
         !is.na(coding), coding > 0) %>%
  mutate(Echo = factor(ifelse(bias_side_dominant=="Same","Echo","Oppositional"),
                       levels=c("Oppositional","Echo")),
         Lean = factor(ifelse(lean=="novelty","Novel-leaning","Reliable-leaning"),
                       levels=c("Novel-leaning","Reliable-leaning")),
         wk = factor(week), sid = factor(student_id),
         w_vol = effective_source_chars / mean(effective_source_chars))
cat(sprintf("Analysis sample: %d student-weeks, %d students\n", nrow(rep), n_distinct(rep$sid)))
print(table(rep$Echo, rep$Lean))
cat(sprintf("students contributing >1 row: %d\n", sum(table(rep$sid) > 1)))
band <- rep %>% filter(abs(direction_score) >= 0.5)
cat(sprintf("band (|direction| >= 0.5) sample: %d student-weeks\n\n", nrow(band)))

star <- function(p) if (is.na(p)) "" else if (p<.001) "***" else if (p<.01) "**" else
                    if (p<.05) "*" else if (p<.1) "†" else ""
fmt  <- function(b,se,p) sprintf("%.3f%s (%.3f)", b, star(p), se)
TERMS <- c(cons="(Intercept)", echo="EchoEcho", lean="LeanReliable-leaning",
           int="EchoEcho:LeanReliable-leaning", prog="prior_prog_exp", ml="prior_ml_exp")

fit_one <- function(dat, rhs, wt=NULL) {
  f <- as.formula(paste("coding ~", rhs))
  m <- if (is.null(wt)) lm(f, data=dat) else lm(f, data=dat, weights=dat[[wt]])
  vc <- vcovCL(m, cluster=dat$sid[as.integer(rownames(model.frame(m)))])
  ct <- coeftest(m, vcov=vc)
  get <- function(k) if (k %in% rownames(ct)) fmt(ct[k,1], ct[k,2], ct[k,4]) else "–"
  marg <- if (grepl("Lean", rhs)) {
    pw <- as.data.frame(summary(pairs(emmeans(m, ~ Echo, vcov.=vc)), infer=TRUE))
    fmt(-pw$estimate, pw$SE, pw$p.value)         # Echo - Oppositional
  } else "–"
  c(lapply(TERMS, get), list(marg=marg, adjr=sprintf("%.3f", summary(m)$adj.r.squared),
    rsd=sprintf("%.3f (df = %d)", sigma(m), df.residual(m)), n=nobs(m)))
}
res <- list(
  M1 = fit_one(rep,  "Echo + wk"),
  M2 = fit_one(rep,  "Echo * Lean + wk"),
  M3 = fit_one(rep,  "Echo * Lean + prior_prog_exp + prior_ml_exp + wk"),
  M4 = fit_one(band, "Echo * Lean + wk"),
  M5 = fit_one(rep,  "Echo * Lean + wk", wt="w_vol"))
saveRDS(res, file.path(OUT,"table_echo_chamber.rds"))

row <- function(k, lab, f=identity) sprintf("| %s | %s |", lab,
  paste(sapply(res, function(r) { v <- r[[k]]; if (is.null(v) || (length(v)==1 && is.na(v))) "–" else f(v) }), collapse=" | "))
L <- c("| | M1 | M2 | M3 | M4 | M5 |", "|---|---|---|---|---|---|",
  row("cons","Constant"),
  row("echo","Echo-chamber bias"),
  row("lean","Reliable-leaning student"),
  row("int","Echo-chamber × Reliable-leaning"),
  row("marg","Echo-chamber bias, marginal over lean"),
  row("prog","Prior programming experience"),
  row("ml","Prior ML/AI experience"),
  "| Week fixed effects | Yes | Yes | Yes | Yes | Yes |",
  "| Near-neutral leans dropped | No | No | No | Yes | No |",
  "| Assignment-volume weighting | No | No | No | No | Yes |",
  row("n","Observations", function(v) format(v, big.mark=",")),
  row("adjr","Adjusted R2"),
  row("rsd","Residual SD"))
writeLines(L, file.path(OUT,"table_echo_chamber.md"))
cat(paste(L, collapse="\n"), "\n")
