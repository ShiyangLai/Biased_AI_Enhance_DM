# ==============================================================================
# table_performance_four_arm.R — Table S56: assignment performance across the
# FOUR arms (Extended Data Fig. 4i). Default is the reference; the neutralized
# arm joins the novelty / reliability ladder.
#
# Data spine and outcomes follow neutral_arm_performance.R exactly:
#   spine   scores_all_graders_student_week.csv (authoritative 5-arm assignment),
#           QUESTIONING_ORIENTED excluded
#   coding  2 human graders + Gemini, Gemini mean-aligned to the human scale
#   memo    the 2 human graders only
#   sample  student-weeks with a submitted assignment (grade > 0)
# Specifications match Table S4 (three-arm) so the two are read side by side:
#   M1 primary: arm + week FE + student FE, student-clustered SE
#   M2 no student effect: arm + week FE + prior-experience covariates, clustered
#   M3 no student effect: arm + week FE + the 18 balance-flagged covariates
#   M4 primary, weighted by assignment volume (effective_source_chars)
#   M5 primary + the stand-alone AI grade for that arm and week (coding only)
#
# NOTE ON MULTIPLICITY. Adding the fourth arm makes the focal family three
# contrasts against Default rather than two, so FDR-adjusted p-values are not
# comparable to the three-arm table. Adjusted p for the M1 focal family is
# printed beneath each panel; starred codes elsewhere are unadjusted, as in the
# other tables.
#
# Outputs: figures/table_performance_four_arm.md, figures/table_performance_four_arm.rds
#   setwd("education/code"); source("_setup.R"); source("table_performance_four_arm.R")
# ==============================================================================
suppressPackageStartupMessages({ library(dplyr); library(readr); library(sandwich); library(lmtest) })
IN <- DATA_DIR; OUT <- FIG_DIR
ARMS <- c("Default","Novelty","Neutralized","Reliability")
FLAG <- c("stop_good_enough","pref_one_answer","verify_freq","expect_learn","pref_reliable",
          "hours_ai_7d","eval_confidence","claude_freq","n_ai_tools","academic_level",
          "pref_novel","comfort_reject","prior_ide_tool","use_learning","prior_prog",
          "prior_ml","pref_several","worry_dependence")

d <- read_csv(file.path(IN,"scores_all_graders_student_week.csv"), show_col_types=FALSE) %>%
  dplyr::select(student_id, week, policy, gem = gemini_asgn_score) %>%
  filter(policy != "QUESTIONING_ORIENTED") %>%
  left_join(read_csv(file.path(IN,"coding_regrade_student_week.csv"), show_col_types=FALSE), by=c("student_id","week")) %>%
  left_join(read_csv(file.path(IN,"memo_regrade_student_week.csv"),   show_col_types=FALSE), by=c("student_id","week")) %>%
  left_join(read_csv(file.path(IN,"bm_student_week.csv"), show_col_types=FALSE) %>%
              dplyr::select(student_id, week, prior_prog_exp, prior_ml_exp), by=c("student_id","week")) %>%
  left_join(read_csv(file.path(IN,"week_workload_summary.csv"), show_col_types=FALSE), by="week") %>%
  left_join(read.csv(file.path(IN,"supplementary_dnr_student_week.csv"), check.names=FALSE)[, c("student_id","week",FLAG)],
            by=c("student_id","week")) %>%
  left_join(read.csv(file.path(IN, "ai_standalone_grades.csv"), stringsAsFactors=FALSE) %>%
              transmute(week, policy = dplyr::recode(condition, default="DEFAULT", novelty="NOVELTY_BIASED",
                                                     reliability="RELIABILITY_BIASED", neutral="NEUTRAL"),
                        sa_grade = composite), by=c("week","policy")) %>%
  mutate(arm = factor(dplyr::recode(policy, DEFAULT="Default", NEUTRAL="Neutralized",
                                    NOVELTY_BIASED="Novelty", RELIABILITY_BIASED="Reliability"), levels=ARMS),
         wk = factor(week), sid = factor(student_id))
GEM <- mean(d$coding_regrade[!is.na(d$gem)], na.rm=TRUE) - mean(d$gem[!is.na(d$coding_regrade)], na.rm=TRUE)
d$coding <- ifelse(is.na(d$gem), d$coding_regrade, (2*d$coding_regrade + d$gem + GEM)/3)
d$coding[is.na(d$coding_regrade)] <- NA
d$memo <- d$memo_regrade
for (cv in c("prior_prog_exp","prior_ml_exp", setdiff(FLAG, c("academic_level","prior_ide_tool")))) {
  d[[cv]] <- as.numeric(d[[cv]]); d[[cv]][is.na(d[[cv]])] <- median(d[[cv]], na.rm=TRUE) }
for (cv in c("academic_level","prior_ide_tool"))
  d[[cv]] <- factor(ifelse(is.na(d[[cv]]) | d[[cv]]=="", "Missing", as.character(d[[cv]])))

star <- function(p) if (is.na(p)) "" else if (p<.001) "***" else if (p<.01) "**" else
                    if (p<.05) "*" else if (p<.1) "†" else ""
fmt  <- function(b,se,p) sprintf("%.3f%s (%.3f)", b, star(p), se)
SPECS <- list(
  M1 = list(rhs="arm + wk + sid", wt=NULL, sa=FALSE),
  M2 = list(rhs="arm + wk + prior_prog_exp + prior_ml_exp", wt=NULL, sa=FALSE),
  M3 = list(rhs=paste("arm + wk +", paste(FLAG, collapse=" + ")), wt=NULL, sa=FALSE),
  M4 = list(rhs="arm + wk + sid", wt="effective_source_chars", sa=FALSE),
  M5 = list(rhs="arm + wk + sid + sa_grade", wt=NULL, sa=TRUE))
fit <- function(out, spec) {
  dat <- d[!is.na(d[[out]]) & d[[out]] > 0, ]
  if (spec$sa) dat <- dat[!is.na(dat$sa_grade), ]
  w <- if (!is.null(spec$wt)) dat[[spec$wt]] / mean(dat[[spec$wt]]) else NULL
  f <- as.formula(paste(out, "~", spec$rhs))
  m <- if (is.null(w)) lm(f, data=dat) else lm(f, data=dat, weights=w)
  vc <- vcovCL(m, cluster=dat$sid[as.integer(rownames(model.frame(m)))]); ct <- coeftest(m, vcov=vc)
  g <- function(k) if (k %in% rownames(ct)) fmt(ct[k,1], ct[k,2], ct[k,4]) else "–"
  foc <- intersect(c("armNovelty","armNeutralized","armReliability"), rownames(ct))
  list(cons=g("(Intercept)"), nov=g("armNovelty"), neu=g("armNeutralized"), rel=g("armReliability"),
       sa=g("sa_grade"), n=nobs(m), adjr=sprintf("%.3f", summary(m)$adj.r.squared),
       rsd=sprintf("%.3f (df = %d)", sigma(m), df.residual(m)),
       fdr=setNames(p.adjust(ct[foc,4], "fdr"), foc),
       narm=sapply(ARMS[-1], function(a) sum(dat$arm==a)))
}
res <- list(); for (out in c("coding","memo")) for (nm in names(SPECS)) {
  if (out=="memo" && nm=="M5") next; res[[paste(out,nm)]] <- fit(out, SPECS[[nm]]) }
saveRDS(res, file.path(OUT,"table_performance_four_arm.rds"))
MODS <- names(SPECS)
rw <- function(out, f, g=identity) sapply(MODS, function(nm) { r <- res[[paste(out,nm)]]
  if (is.null(r)) "–" else g(r[[f]]) })
L <- c("| | M1 | M2 | M3 | M4 | M5 |", "|---|---|---|---|---|---|")
for (out in c("coding","memo")) {
  r1 <- res[[paste(out,"M1")]]
  L <- c(L, sprintf("| **%s Performance** | | | | | |", if (out=="coding") "Coding" else "Memo"),
    sprintf("| Constant | %s |", paste(rw(out,"cons"), collapse=" | ")),
    sprintf("| Novelty-biased | %s |", paste(rw(out,"nov"), collapse=" | ")),
    sprintf("| Neutralized | %s |", paste(rw(out,"neu"), collapse=" | ")),
    sprintf("| Reliability-biased | %s |", paste(rw(out,"rel"), collapse=" | ")),
    sprintf("| Stand-alone AI grade | %s |", paste(rw(out,"sa"), collapse=" | ")),
    sprintf("| M1 FDR-adjusted p (Nov / Neu / Rel) | %.3f / %.3f / %.3f | | | | |",
            r1$fdr[["armNovelty"]], r1$fdr[["armNeutralized"]], r1$fdr[["armReliability"]]),
    sprintf("| Observations | %s |", paste(rw(out,"n", function(v) format(v, big.mark=",")), collapse=" | ")),
    sprintf("| Student-weeks (Nov / Neu / Rel) | %d / %d / %d | | | | |",
            r1$narm[["Novelty"]], r1$narm[["Neutralized"]], r1$narm[["Reliability"]]),
    sprintf("| Adjusted R2 | %s |", paste(rw(out,"adjr"), collapse=" | ")),
    sprintf("| Residual SD | %s |", paste(rw(out,"rsd"), collapse=" | ")))
}
ind <- rbind(c("Week fixed effects","Yes","Yes","Yes","Yes","Yes"),
             c("Student fixed effects","Yes","No","No","Yes","Yes"),
             c("Prior-experience covariates","No","Yes","No","No","No"),
             c("Imbalanced covariates (18)","No","No","Yes","No","No"),
             c("Assignment-volume weighting","No","No","No","Yes","No"),
             c("Stand-alone AI grade","No","No","No","No","Yes"))
for (i in seq_len(nrow(ind))) L <- c(L, paste0("| ", paste(ind[i,], collapse=" | "), " |"))
writeLines(L, file.path(OUT,"table_performance_four_arm.md")); cat(paste(L, collapse="\n"), "\n")
