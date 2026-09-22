# ==============================================================================
# table_engagement_three_arm.R — Tables S52a/b: conversational engagement, measured as
# log(1 + number of reply turns), for the two bias-magnitude ladders
# (Default -> Novelty-biased, Default -> Reliability-biased). Same M1-M4
# specifications and layout as the performance tables (s4/s6).
#
# Outcome. Reply turns = user messages per student-week, rebuilt exactly from
# telemetry_student_week.csv (reproduces the authoritative bm column, r = 1.000);
# a student-week with no log rows is a zero, not missing. Two panels:
#   Use-conditioned  student-weeks with at least one reply turn (the primary
#                    sample in e3 / bm: a zero is an absence of conversation,
#                    not a short one)
#   All weeks        zeros retained (intention-to-treat companion)
# M5 (stand-alone AI grade) is omitted: it is a grader-preference decomposition
# of the assignment grade and has no analogue for an engagement outcome.
#
# Outputs: figures/table_engagement_novelty.md, figures/table_engagement_reliability.md,
#          figures/table_engagement_three_arm.rds
#   setwd("education/code"); source("_setup.R"); source("table_engagement_three_arm.R")
# ==============================================================================
suppressPackageStartupMessages({ library(dplyr); library(readr); library(sandwich); library(lmtest) })
IN <- DATA_DIR; OUT <- FIG_DIR

s  <- read_csv(file.path(IN,"scores_all_graders_student_week.csv"), show_col_types=FALSE)
bm <- read_csv(file.path(IN,"bm_student_week.csv"), show_col_types=FALSE) %>%
        dplyr::select(student_id, week, prior_prog_exp, prior_ml_exp)
wl <- read_csv(file.path(IN,"week_workload_summary.csv"), show_col_types=FALSE)
dn <- read.csv(file.path(IN,"supplementary_dnr_student_week.csv"), check.names=FALSE)
tel <- read_csv(file.path(IN, "telemetry_student_week.csv"), show_col_types = FALSE)
FLAG <- c("stop_good_enough","pref_one_answer","verify_freq","expect_learn","pref_reliable",
          "hours_ai_7d","eval_confidence","claude_freq","n_ai_tools","academic_level",
          "pref_novel","comfort_reject","prior_ide_tool","use_learning","prior_prog",
          "prior_ml","pref_several","worry_dependence")
d <- s %>% filter(policy %in% c("DEFAULT","NOVELTY_BIASED","RELIABILITY_BIASED")) %>%
  left_join(tel, by=c("student_id","week")) %>% left_join(bm, by=c("student_id","week")) %>%
  left_join(wl, by="week") %>% left_join(dn[, c("student_id","week", FLAG)], by=c("student_id","week")) %>%
  mutate(conv_round = ifelse(is.na(conv_round), 0, conv_round),
         y = log1p(conv_round), wk = factor(week), sid = factor(student_id),
         len = as.numeric(scale(effective_source_chars)))
for (cv in c("prior_prog_exp","prior_ml_exp", setdiff(FLAG, c("academic_level","prior_ide_tool")))) {
  d[[cv]] <- as.numeric(d[[cv]]); d[[cv]][is.na(d[[cv]])] <- median(d[[cv]], na.rm=TRUE) }
for (cv in c("academic_level","prior_ide_tool"))
  d[[cv]] <- factor(ifelse(is.na(d[[cv]]) | d[[cv]]=="", "Missing", as.character(d[[cv]])))

star <- function(p) if (is.na(p)) "" else if (p<.001) "***" else if (p<.01) "**" else
                    if (p<.05) "*" else if (p<.1) "†" else ""
fmt  <- function(b,se,p) sprintf("%.3f%s (%.3f)", b, star(p), se)
SPECS <- list(
  M1 = list(rhs="arm + wk + sid", wt=NULL),
  M2 = list(rhs="arm + wk + prior_prog_exp + prior_ml_exp", wt=NULL),
  M3 = list(rhs=paste("arm + wk +", paste(FLAG, collapse=" + ")), wt=NULL),
  M4 = list(rhs="arm + wk + sid", wt="effective_source_chars"),
  # length as a covariate is collinear with week FE, so it can only enter when
  # the week FE are removed; M4 (weighting) keeps the week FE and is preferred
  M5 = list(rhs="arm + sid + len", wt=NULL))

fit_one <- function(dat, spec) {
  w <- if (!is.null(spec$wt)) dat[[spec$wt]] / mean(dat[[spec$wt]]) else NULL
  f <- as.formula(paste("y ~", spec$rhs))
  m <- if (is.null(w)) lm(f, data=dat) else lm(f, data=dat, weights=w)
  vc <- vcovCL(m, cluster=dat$sid[as.integer(rownames(model.frame(m)))])
  ct <- coeftest(m, vcov=vc)
  getr <- function(k) if (k %in% rownames(ct)) fmt(ct[k,1], ct[k,2], ct[k,4]) else "–"
  list(arm=getr("armBiased"), cons=getr("(Intercept)"), n=nobs(m),
       n_arm=sum(dat$arm=="Biased"),
       adjr=sprintf("%.3f", summary(m)$adj.r.squared),
       rsd=sprintf("%.3f (df = %d)", sigma(m), df.residual(m)))
}
build <- function(keep, arm_label, file) {
  dd <- d %>% filter(policy %in% c("DEFAULT", keep)) %>%
    mutate(arm = factor(ifelse(policy=="DEFAULT","Default","Biased"), levels=c("Default","Biased")))
  panels <- list(`Weeks with any assistant use` = dd[dd$conv_round > 0, ], `All student-weeks` = dd)
  res <- list()
  for (pn in names(panels)) for (nm in setdiff(names(SPECS),"M5")) res[[paste(pn,nm)]] <- fit_one(panels[[pn]], SPECS[[nm]])
  rowv <- function(pn, field, f=identity) sapply(setdiff(names(SPECS),"M5"), function(nm) f(res[[paste(pn,nm)]][[field]]))
  L <- c("| | M1 | M2 | M3 | M4 |", "|---|---|---|---|---|")
  for (pn in names(panels)) {
    L <- c(L, sprintf("| **%s** | | | | |", pn),
      sprintf("| Constant | %s |", paste(rowv(pn,"cons"), collapse=" | ")),
      sprintf("| %s | %s |", arm_label, paste(rowv(pn,"arm"), collapse=" | ")),
      sprintf("| Observations | %s |", paste(rowv(pn,"n", function(v) format(v, big.mark=",")), collapse=" | ")),
      sprintf("| Biased-arm student-weeks | %s |", paste(rowv(pn,"n_arm"), collapse=" | ")),
      sprintf("| Adjusted R2 | %s |", paste(rowv(pn,"adjr"), collapse=" | ")),
      sprintf("| Residual SD | %s |", paste(rowv(pn,"rsd"), collapse=" | ")))
  }
  ind <- rbind(c("Week fixed effects","Yes","Yes","Yes","Yes"),
               c("Student fixed effects","Yes","No","No","Yes"),
               c("Prior-experience covariates","No","Yes","No","No"),
               c("Imbalanced covariates (18)","No","No","Yes","No"),
               c("Assignment-volume weighting","No","No","No","Yes"))
  for (i in seq_len(nrow(ind))) L <- c(L, paste0("| ", paste(ind[i,], collapse=" | "), " |"))
  writeLines(L, file.path(OUT, file)); cat(sprintf("\n##### %s #####\n", file)); cat(paste(L, collapse="\n"), "\n")
  res
}
all <- list(novelty     = build("NOVELTY_BIASED",     "Novelty-biased",     "table_engagement_novelty.md"),
            reliability = build("RELIABILITY_BIASED", "Reliability-biased", "table_engagement_reliability.md"))
saveRDS(all, file.path(OUT,"table_engagement_three_arm.rds"))

# ── three-arm joint version: the exact model behind Fig. 3c ──────────────────
d3 <- d %>% mutate(arm = relevel(factor(dplyr::recode(policy, DEFAULT="Default",
             NOVELTY_BIASED="Novelty", RELIABILITY_BIASED="Reliability")), ref="Default"))
fit3 <- function(dat, spec) {
  w <- if (!is.null(spec$wt)) dat[[spec$wt]] / mean(dat[[spec$wt]]) else NULL
  m <- if (is.null(w)) lm(as.formula(paste("y ~", spec$rhs)), data=dat) else
                       lm(as.formula(paste("y ~", spec$rhs)), data=dat, weights=w)
  vc <- vcovCL(m, cluster=dat$sid[as.integer(rownames(model.frame(m)))]); ct <- coeftest(m, vcov=vc)
  getr <- function(k) if (k %in% rownames(ct)) fmt(ct[k,1], ct[k,2], ct[k,4]) else "–"
  list(nov=getr("armNovelty"), rel=getr("armReliability"), cons=getr("(Intercept)"), len=getr("len"), n=nobs(m),
       n_nov=sum(dat$arm=="Novelty"), n_rel=sum(dat$arm=="Reliability"),
       adjr=sprintf("%.3f", summary(m)$adj.r.squared), rsd=sprintf("%.3f (df = %d)", sigma(m), df.residual(m)))
}
panels3 <- list(`Weeks with any assistant use` = d3[d3$conv_round > 0, ], `All student-weeks` = d3)
res3 <- list(); for (pn in names(panels3)) for (nm in names(SPECS)) res3[[paste(pn,nm)]] <- fit3(panels3[[pn]], SPECS[[nm]])
rowv3 <- function(pn, f, g=identity) sapply(names(SPECS), function(nm) g(res3[[paste(pn,nm)]][[f]]))
L3 <- c("| | M1 | M2 | M3 | M4 | M5 |", "|---|---|---|---|---|---|")
for (pn in names(panels3)) L3 <- c(L3, sprintf("| **%s** | | | | | |", pn),
  sprintf("| Constant | %s |", paste(rowv3(pn,"cons"), collapse=" | ")),
  sprintf("| Novelty-biased | %s |", paste(rowv3(pn,"nov"), collapse=" | ")),
  sprintf("| Reliability-biased | %s |", paste(rowv3(pn,"rel"), collapse=" | ")),
  sprintf("| Assignment length (std.) | %s |", paste(rowv3(pn,"len"), collapse=" | ")),
  sprintf("| Observations | %s |", paste(rowv3(pn,"n"), collapse=" | ")),
  sprintf("| Novelty / Reliability student-weeks | %s |", paste(paste(rowv3(pn,"n_nov"), rowv3(pn,"n_rel"), sep=" / "), collapse=" | ")),
  sprintf("| Adjusted R2 | %s |", paste(rowv3(pn,"adjr"), collapse=" | ")),
  sprintf("| Residual SD | %s |", paste(rowv3(pn,"rsd"), collapse=" | ")))
for (i in seq_len(nrow(ind <- rbind(c("Week fixed effects","Yes","Yes","Yes","Yes","No"),
  c("Student fixed effects","Yes","No","No","Yes","Yes"), c("Prior-experience covariates","No","Yes","No","No","No"),
  c("Imbalanced covariates (18)","No","No","Yes","No","No"), c("Assignment-volume weighting","No","No","No","Yes","No"),
  c("Assignment-length covariate","No","No","No","No","Yes")))))
  L3 <- c(L3, paste0("| ", paste(ind[i,], collapse=" | "), " |"))
writeLines(L3, file.path(OUT, "table_engagement_three_arm.md"))
cat("\n##### table_engagement_three_arm.md #####\n"); cat(paste(L3, collapse="\n"), "\n")
suppressPackageStartupMessages(library(emmeans))
mU <- lm(y ~ arm + wk + sid, data=panels3[[1]]); vU <- vcovCL(mU, cluster=panels3[[1]]$sid)
cat("\nuse-conditioned three-arm marginal means (what Fig. 3c plots):\n")
print(as.data.frame(summary(emmeans(mU, ~arm, vcov.=vU)))[,c("arm","emmean","SE")], digits=4)
