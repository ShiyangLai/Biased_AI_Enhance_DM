# ==============================================================================
# table_engagement_helpfulness_four_arm.R — Tables S56c and S56d: the four-arm engagement and
# perceived-helpfulness panels of Extended Data Fig. 4 (k and l).
#
#   S56c  conversational engagement, log(1 + reply turns)   [ED Fig. 4k, e3]
#   S56d  perceived helpfulness, post-survey item 1-7        [ED Fig. 4l, e4]
#
# Spine, outcomes and primary model follow neutral_arm_engagement.R and
# neutral_arm_helpfulness.R exactly: scores_all_graders_student_week.csv with
# QUESTIONING_ORIENTED excluded, reply turns rebuilt from the raw session log
# (a week with no rows is a zero), and lm(y ~ arm + week FE + student FE) with
# student-clustered standard errors. Default is the reference.
#
# Columns, as in Table S56a:
#   M1 primary: arm + week FE + student FE, student-clustered SE
#   M2 no student effect: arm + week FE + prior-experience covariates, clustered
#   M3 no student effect: arm + week FE + the 18 balance-flagged covariates
#   M4 primary, weighted by assignment volume (effective_source_chars)
# Each table has two panels: the use-conditioned sample that the figure draws,
# and an all-weeks companion.
#
# In small samples the 18 flagged covariates can span the student space exactly,
# making M3 numerically identical to M1; that is detected and marked rather than
# printed as a distinct specification.
#
# Outputs: figures/table_engagement_four_arm.md, figures/table_helpfulness_four_arm.md
#   setwd("education/code"); source("_setup.R"); source("table_engagement_helpfulness_four_arm.R")
# ==============================================================================
suppressPackageStartupMessages({ library(dplyr); library(readr); library(sandwich); library(lmtest) })
IN <- DATA_DIR; OUT <- FIG_DIR
ARMS <- c("Default","Novelty","Neutralized","Reliability")
FLAG <- c("stop_good_enough","pref_one_answer","verify_freq","expect_learn","pref_reliable",
          "hours_ai_7d","eval_confidence","claude_freq","n_ai_tools","academic_level",
          "pref_novel","comfort_reject","prior_ide_tool","use_learning","prior_prog",
          "prior_ml","pref_several","worry_dependence")
tel <- read_csv(file.path(IN, "telemetry_student_week.csv"), show_col_types = FALSE)
base <- read_csv(file.path(IN,"scores_all_graders_student_week.csv"), show_col_types=FALSE) %>%
  dplyr::select(student_id, week, policy) %>% filter(policy != "QUESTIONING_ORIENTED") %>%
  left_join(tel, by=c("student_id","week")) %>%
  mutate(conv_round = ifelse(is.na(conv_round), 0, conv_round)) %>%
  left_join(read_csv(file.path(IN,"bm_student_week.csv"), show_col_types=FALSE) %>%
              dplyr::select(student_id, week, prior_prog_exp, prior_ml_exp), by=c("student_id","week")) %>%
  left_join(read_csv(file.path(IN,"week_workload_summary.csv"), show_col_types=FALSE), by="week") %>%
  left_join(read.csv(file.path(IN,"supplementary_dnr_student_week.csv"), check.names=FALSE)[, c("student_id","week",FLAG)],
            by=c("student_id","week")) %>%
  mutate(arm = factor(dplyr::recode(policy, DEFAULT="Default", NEUTRAL="Neutralized",
                                    NOVELTY_BIASED="Novelty", RELIABILITY_BIASED="Reliability"), levels=ARMS),
         wk = factor(week), sid = factor(student_id))
for (cv in c("prior_prog_exp","prior_ml_exp", setdiff(FLAG, c("academic_level","prior_ide_tool")))) {
  base[[cv]] <- as.numeric(base[[cv]]); base[[cv]][is.na(base[[cv]])] <- median(base[[cv]], na.rm=TRUE) }
for (cv in c("academic_level","prior_ide_tool"))
  base[[cv]] <- factor(ifelse(is.na(base[[cv]]) | base[[cv]]=="", "Missing", as.character(base[[cv]])))
surv <- read_csv(file.path(IN,"post_survey_student_week.csv"), show_col_types=FALSE) %>%
  dplyr::select(student_id, week, usefulness) %>% filter(!is.na(usefulness))

star <- function(p) if (is.na(p)) "" else if (p<.001) "***" else if (p<.01) "**" else
                    if (p<.05) "*" else if (p<.1) "†" else ""
fmt  <- function(b,se,p) sprintf("%.3f%s (%.3f)", b, star(p), se)
SPECS <- list(
  M1 = list(rhs="arm + wk + sid", wt=NULL),
  M2 = list(rhs="arm + wk + prior_prog_exp + prior_ml_exp", wt=NULL),
  M3 = list(rhs=paste("arm + wk +", paste(FLAG, collapse=" + ")), wt=NULL),
  M4 = list(rhs="arm + wk + sid", wt="effective_source_chars"))
fit <- function(dat, spec) {
  if (grepl("stop_good_enough", spec$rhs)) {                    # M3 saturation guard
    Xc <- model.matrix(as.formula(paste("~", paste(FLAG, collapse="+"))), data=dat)
    if (qr(Xc)$rank == qr(model.matrix(~ sid, data=dat))$rank)
      return(list(cons="= M1", nov="= M1", neu="= M1", rel="= M1", n=nrow(dat),
                  narm=sapply(ARMS[-1], function(a) sum(dat$arm==a)), adjr="= M1", rsd="= M1"))
  }
  w <- if (!is.null(spec$wt)) dat[[spec$wt]] / mean(dat[[spec$wt]]) else NULL
  f <- as.formula(paste("y ~", spec$rhs))
  m <- if (is.null(w)) lm(f, data=dat) else lm(f, data=dat, weights=w)
  stopifnot(nobs(m) == nrow(dat))
  vc <- vcovCL(m, cluster=dat$sid[as.integer(rownames(model.frame(m)))]); ct <- coeftest(m, vcov=vc)
  g <- function(k) if (k %in% rownames(ct)) fmt(ct[k,1], ct[k,2], ct[k,4]) else "–"
  list(cons=g("(Intercept)"), nov=g("armNovelty"), neu=g("armNeutralized"), rel=g("armReliability"),
       n=nobs(m), narm=sapply(ARMS[-1], function(a) sum(dat$arm==a)),
       adjr=sprintf("%.3f", summary(m)$adj.r.squared),
       rsd=sprintf("%.3f (df = %d)", sigma(m), df.residual(m)))
}
build <- function(panels, file, tag) {
  res <- list(); for (pn in names(panels)) for (nm in names(SPECS)) res[[paste(pn,nm)]] <- fit(panels[[pn]], SPECS[[nm]])
  rw <- function(pn, f, g=identity) sapply(names(SPECS), function(nm) g(res[[paste(pn,nm)]][[f]]))
  L <- c("| | M1 | M2 | M3 | M4 |", "|---|---|---|---|---|")
  for (pn in names(panels)) { r1 <- res[[paste(pn,"M1")]]
    L <- c(L, sprintf("| **%s** | | | | |", pn),
      sprintf("| Constant | %s |", paste(rw(pn,"cons"), collapse=" | ")),
      sprintf("| Novelty-biased | %s |", paste(rw(pn,"nov"), collapse=" | ")),
      sprintf("| Neutralized | %s |", paste(rw(pn,"neu"), collapse=" | ")),
      sprintf("| Reliability-biased | %s |", paste(rw(pn,"rel"), collapse=" | ")),
      sprintf("| Observations | %s |", paste(rw(pn,"n", function(v) format(v, big.mark=",")), collapse=" | ")),
      sprintf("| Student-weeks (Nov / Neu / Rel) | %d / %d / %d | | | |",
              r1$narm[["Novelty"]], r1$narm[["Neutralized"]], r1$narm[["Reliability"]]),
      sprintf("| Adjusted R2 | %s |", paste(rw(pn,"adjr"), collapse=" | ")),
      sprintf("| Residual SD | %s |", paste(rw(pn,"rsd"), collapse=" | ")))
  }
  ind <- rbind(c("Week fixed effects","Yes","Yes","Yes","Yes"),
               c("Student fixed effects","Yes","No","No","Yes"),
               c("Prior-experience covariates","No","Yes","No","No"),
               c("Imbalanced covariates (18)","No","No","Yes","No"),
               c("Assignment-volume weighting","No","No","No","Yes"))
  for (i in seq_len(nrow(ind))) L <- c(L, paste0("| ", paste(ind[i,], collapse=" | "), " |"))
  writeLines(L, file.path(OUT, file)); cat(sprintf("\n##### %s (%s) #####\n", file, tag)); cat(paste(L, collapse="\n"), "\n")
}
# ── S56c: engagement ─────────────────────────────────────────────────────────
e <- base %>% mutate(y = log1p(conv_round))
build(list(`Use-conditioned` = e[e$conv_round > 0, ], `All weeks` = e),
      "table_engagement_four_arm.md", "ED Fig. 4k")
# ── S56d: perceived helpfulness ──────────────────────────────────────────────
h <- base %>% inner_join(surv, by=c("student_id","week")) %>% mutate(y = usefulness)
build(list(`Use-conditioned` = h[h$conv_round > 0, ], `All surveyed weeks` = h),
      "table_helpfulness_four_arm.md", "ED Fig. 4l")
