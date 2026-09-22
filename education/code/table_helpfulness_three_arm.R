# ==============================================================================
# table_helpfulness_three_arm.R — Table S53: perceived helpfulness (post-assignment
# survey "usefulness" item, 1-7) across the three arms, same layout as S52.
#
# Panels: use-conditioned (surveyed student-weeks with >= 1 reply turn; the sample
# drawn in Fig. 3d and in bias_magnitude_outcomes.R block 3) and all surveyed
# weeks (companion). M1-M4 as in S4/S52. M5 is a cumulative-link (ordinal) model
# with week FE and the prior-experience covariates; its coefficients are on the
# latent scale and are not comparable in magnitude to the linear columns.
#
# Outputs: figures/table_helpfulness_three_arm.md, figures/table_helpfulness_three_arm.rds
#   setwd("education/code"); source("_setup.R"); source("table_helpfulness_three_arm.R")
# ==============================================================================
suppressPackageStartupMessages({ library(dplyr); library(readr); library(sandwich); library(lmtest); library(ordinal) })
IN <- DATA_DIR; OUT <- FIG_DIR
post <- read_csv(file.path(IN,"post_survey_student_week.csv"), show_col_types=FALSE) %>%
  dplyr::select(student_id, week, policy, usefulness) %>% filter(!is.na(usefulness))
bm <- read_csv(file.path(IN,"bm_student_week.csv"), show_col_types=FALSE) %>%
        dplyr::select(student_id, week, prior_prog_exp, prior_ml_exp)
wl <- read_csv(file.path(IN,"week_workload_summary.csv"), show_col_types=FALSE)
dn <- read.csv(file.path(IN,"supplementary_dnr_student_week.csv"), check.names=FALSE)
tel <- read_csv(file.path(IN, "telemetry_student_week.csv"), show_col_types = FALSE)
FLAG <- c("stop_good_enough","pref_one_answer","verify_freq","expect_learn","pref_reliable",
          "hours_ai_7d","eval_confidence","claude_freq","n_ai_tools","academic_level",
          "pref_novel","comfort_reject","prior_ide_tool","use_learning","prior_prog",
          "prior_ml","pref_several","worry_dependence")
d <- post %>% filter(policy %in% c("DEFAULT","NOVELTY_BIASED","RELIABILITY_BIASED")) %>%
  left_join(tel, by=c("student_id","week")) %>% left_join(bm, by=c("student_id","week")) %>%
  left_join(wl, by="week") %>% left_join(dn[, c("student_id","week", FLAG)], by=c("student_id","week")) %>%
  mutate(conv_round = ifelse(is.na(conv_round), 0, conv_round), y = usefulness,
         arm = relevel(factor(dplyr::recode(policy, DEFAULT="Default", NOVELTY_BIASED="Novelty",
                                            RELIABILITY_BIASED="Reliability")), ref="Default"),
         wk = factor(week), sid = factor(student_id), yo = factor(usefulness, ordered=TRUE))
for (cv in c("prior_prog_exp","prior_ml_exp", setdiff(FLAG, c("academic_level","prior_ide_tool")))) {
  d[[cv]] <- as.numeric(d[[cv]]); d[[cv]][is.na(d[[cv]])] <- median(d[[cv]], na.rm=TRUE) }
for (cv in c("academic_level","prior_ide_tool"))
  d[[cv]] <- factor(ifelse(is.na(d[[cv]]) | d[[cv]]=="", "Missing", as.character(d[[cv]])))

star <- function(p) if (is.na(p)) "" else if (p<.001) "***" else if (p<.01) "**" else
                    if (p<.05) "*" else if (p<.1) "†" else ""
fmt  <- function(b,se,p) sprintf("%.3f%s (%.3f)", b, star(p), se)
SPECS <- list(
  M1 = list(rhs="arm + wk + sid", wt=NULL, ord=FALSE),
  M2 = list(rhs="arm + wk + prior_prog_exp + prior_ml_exp", wt=NULL, ord=FALSE),
  M3 = list(rhs=paste("arm + wk +", paste(FLAG, collapse=" + ")), wt=NULL, ord=FALSE),
  M4 = list(rhs="arm + wk + sid", wt="effective_source_chars", ord=FALSE),
  M5 = list(rhs="arm + wk + prior_prog_exp + prior_ml_exp", wt=NULL, ord=TRUE))
fit <- function(dat, spec) {
  if (spec$ord) {
    m <- clm(as.formula(paste("yo ~", spec$rhs)), data=dat); ct <- coef(summary(m))
    getr <- function(k) if (k %in% rownames(ct)) fmt(ct[k,1], ct[k,2], ct[k,4]) else "–"
    return(list(nov=getr("armNovelty"), rel=getr("armReliability"), cons="–", n=nobs(m),
                n_nov=sum(dat$arm=="Novelty"), n_rel=sum(dat$arm=="Reliability"),
                adjr="–", rsd=sprintf("logLik %.1f", as.numeric(logLik(m)))))
  }
  # the 18 flagged covariates can span the student space in a small sample, in
  # which case M3 is numerically identical to the student-FE model and is not a
  # distinct specification; detect that and print it as such
  if (grepl("stop_good_enough", spec$rhs)) {
    Xc <- model.matrix(as.formula(paste("~", paste(FLAG, collapse="+"))), data=dat)
    if (qr(Xc)$rank == qr(model.matrix(~ sid, data=dat))$rank)   # covariates span ALL of the student space
      return(list(nov="= M1", rel="= M1", cons="= M1", n=nrow(dat), n_nov=sum(dat$arm=="Novelty"),
                  n_rel=sum(dat$arm=="Reliability"), adjr="= M1", rsd="= M1"))
  }
  w <- if (!is.null(spec$wt)) dat[[spec$wt]] / mean(dat[[spec$wt]]) else NULL
  f <- as.formula(paste("y ~", spec$rhs))
  m <- if (is.null(w)) lm(f, data=dat) else lm(f, data=dat, weights=w)
  vc <- vcovCL(m, cluster=dat$sid[as.integer(rownames(model.frame(m)))]); ct <- coeftest(m, vcov=vc)
  getr <- function(k) if (k %in% rownames(ct)) fmt(ct[k,1], ct[k,2], ct[k,4]) else "–"
  list(nov=getr("armNovelty"), rel=getr("armReliability"), cons=getr("(Intercept)"), n=nobs(m),
       n_nov=sum(dat$arm=="Novelty"), n_rel=sum(dat$arm=="Reliability"),
       adjr=sprintf("%.3f", summary(m)$adj.r.squared), rsd=sprintf("%.3f (df = %d)", sigma(m), df.residual(m)))
}
panels <- list(`Use-conditioned` = d[d$conv_round > 0, ], `All surveyed weeks` = d)
res <- list(); for (pn in names(panels)) for (nm in names(SPECS)) res[[paste(pn,nm)]] <- fit(panels[[pn]], SPECS[[nm]])
saveRDS(res, file.path(OUT,"table_helpfulness_three_arm.rds"))
rowv <- function(pn, f) sapply(names(SPECS), function(nm) res[[paste(pn,nm)]][[f]])
L <- c("| | M1 | M2 | M3 | M4 | M5 (ordinal) |", "|---|---|---|---|---|---|")
for (pn in names(panels)) L <- c(L, sprintf("| **%s** | | | | | |", pn),
  sprintf("| Constant | %s |", paste(rowv(pn,"cons"), collapse=" | ")),
  sprintf("| Novelty-biased | %s |", paste(rowv(pn,"nov"), collapse=" | ")),
  sprintf("| Reliability-biased | %s |", paste(rowv(pn,"rel"), collapse=" | ")),
  sprintf("| Observations | %s |", paste(rowv(pn,"n"), collapse=" | ")),
  sprintf("| Novelty / Reliability student-weeks | %s |", paste(paste(rowv(pn,"n_nov"), rowv(pn,"n_rel"), sep=" / "), collapse=" | ")),
  sprintf("| Adjusted R2 | %s |", paste(rowv(pn,"adjr"), collapse=" | ")),
  sprintf("| Residual SD | %s |", paste(rowv(pn,"rsd"), collapse=" | ")))
ind <- rbind(c("Week fixed effects","Yes","Yes","Yes","Yes","Yes"),
             c("Student fixed effects","Yes","No","No","Yes","No"),
             c("Prior-experience covariates","No","Yes","No","No","Yes"),
             c("Imbalanced covariates (18)","No","No","Yes","No","No"),
             c("Assignment-volume weighting","No","No","No","Yes","No"),
             c("Cumulative-link (ordinal) model","No","No","No","No","Yes"))
for (i in seq_len(nrow(ind))) L <- c(L, paste0("| ", paste(ind[i,], collapse=" | "), " |"))
writeLines(L, file.path(OUT,"table_helpfulness_three_arm.md")); cat(paste(L, collapse="\n"), "\n")
cat("\ncheck vs bm_default_contrasts (Perceived Helpfulness): Novelty -0.674 (0.279) p=.0235; Reliability -0.492 (0.626)\n")
