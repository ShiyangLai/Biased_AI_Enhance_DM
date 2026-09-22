# ==============================================================================
# table_performance_by_direction.R — the two "bias-magnitude ladders" of the bm figure
# (Default -> Novelty-biased, Default -> Reliability-biased) as regression tables,
# one per direction, both graded outcomes, same six specifications as
# table_performance_three_arm.R.
#
# Each table restricts the sample to default weeks plus ONE biased arm, so the
# coefficient is the two-arm contrast the figure's dashed ladder draws. The joint
# three-arm estimates (both biased arms in one model) are in s4; the two differ
# through the week and student effects being estimated with or without the other
# arm's ~20 rows; the M1 coding coefficients move by up to ~0.05 (novelty 0.541
# joint vs 0.585 two-arm). M5 differs more: the stand-alone-grade slope is
# arm-specific (near zero in novelty weeks, large in reliability weeks), so the
# joint model's single pooled slope attenuates novelty in a way the two-arm model
# does not -- see the sa_grade x arm check in the session notes.
#
# A pooled Biased-vs-Default table (both directions collapsed, the memo result
# quoted in the main text) is also written, as table_performance_pooled.md.
#
# Outputs: figures/table_performance_novelty.md, figures/table_performance_reliability.md,
#          figures/table_performance_pooled.md, figures/table_performance_by_direction.rds
#   setwd("education/code"); source("_setup.R"); source("table_performance_by_direction.R")
# ==============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(sandwich); library(lmtest); })
IN <- DATA_DIR; OUT <- FIG_DIR

# ── data (identical construction to s4) ──────────────────────────────────────
s  <- read_csv(file.path(IN,"scores_all_graders_student_week.csv"), show_col_types=FALSE)
g  <- read_csv(file.path(IN,"coding_regrade_student_week.csv"), show_col_types=FALSE)
mm <- read_csv(file.path(IN,"memo_regrade_student_week.csv"), show_col_types=FALSE)
bm <- read_csv(file.path(IN,"bm_student_week.csv"), show_col_types=FALSE) %>%
        dplyr::select(student_id, week, prior_prog_exp, prior_ml_exp)
wl <- read_csv(file.path(IN,"week_workload_summary.csv"), show_col_types=FALSE)
dn <- read.csv(file.path(IN,"supplementary_dnr_student_week.csv"), check.names=FALSE)
sa <- read.csv(file.path(IN, "ai_standalone_grades.csv"), stringsAsFactors=FALSE) %>%
  transmute(week, policy = dplyr::recode(condition, default="DEFAULT", novelty="NOVELTY_BIASED",
                                         reliability="RELIABILITY_BIASED", neutral="NEUTRAL"),
            sa_grade = composite)
FLAG <- c("stop_good_enough","pref_one_answer","verify_freq","expect_learn","pref_reliable",
          "hours_ai_7d","eval_confidence","claude_freq","n_ai_tools","academic_level",
          "pref_novel","comfort_reject","prior_ide_tool","use_learning","prior_prog",
          "prior_ml","pref_several","worry_dependence")
d <- s %>% filter(policy %in% c("DEFAULT","NOVELTY_BIASED","RELIABILITY_BIASED")) %>%
  left_join(g, by=c("student_id","week")) %>% left_join(mm, by=c("student_id","week")) %>%
  left_join(bm, by=c("student_id","week")) %>% left_join(wl, by="week") %>%
  left_join(sa, by=c("week","policy")) %>%
  left_join(dn[, c("student_id","week", FLAG)], by=c("student_id","week")) %>%
  mutate(wk = factor(week), sid = factor(student_id))
sh <- mean(d$coding_regrade[!is.na(d$gemini_asgn_score)], na.rm=TRUE) -
      mean(d$gemini_asgn_score[!is.na(d$coding_regrade)], na.rm=TRUE)
d$coding <- ifelse(is.na(d$gemini_asgn_score), d$coding_regrade, (2*d$coding_regrade + d$gemini_asgn_score + sh)/3)
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
  M1 = list(rhs="arm + wk + sid", mixed=FALSE, wt=NULL, needs_sa=FALSE),
  M2 = list(rhs="arm + wk + prior_prog_exp + prior_ml_exp", mixed=FALSE, wt=NULL, needs_sa=FALSE),
  M3 = list(rhs=paste("arm + wk +", paste(FLAG, collapse=" + ")), mixed=FALSE, wt=NULL, needs_sa=FALSE),
  M4 = list(rhs="arm + wk + sid", mixed=FALSE, wt="effective_source_chars", needs_sa=FALSE),
  M5 = list(rhs="arm + wk + sid + sa_grade", mixed=FALSE, wt=NULL, needs_sa=TRUE))

fit_one <- function(dd, out, spec) {
  dat <- dd[!is.na(dd[[out]]) & dd[[out]] > 0, ]
  if (spec$needs_sa) dat <- dat[!is.na(dat$sa_grade), ]
  f <- as.formula(paste(out, "~", spec$rhs))
  if (spec$mixed) {
    m <- lmer(f, data=dat); ct <- summary(m)$coefficients
    getr <- function(k) if (k %in% rownames(ct)) fmt(ct[k,"Estimate"], ct[k,"Std. Error"], ct[k,"Pr(>|t|)"]) else "–"
    list(arm=getr("armBiased"), cons=getr("(Intercept)"), sa=getr("sa_grade"),
         n=nobs(m), adjr="–", rsd=sprintf("%.3f", sigma(m)))
  } else {
    w <- if (!is.null(spec$wt)) dat[[spec$wt]] / mean(dat[[spec$wt]]) else NULL
    m <- if (is.null(w)) lm(f, data=dat) else lm(f, data=dat, weights=w)
    vc <- vcovCL(m, cluster=dat$sid[as.integer(rownames(model.frame(m)))])
    ct <- coeftest(m, vcov=vc)
    getr <- function(k) if (k %in% rownames(ct)) fmt(ct[k,1], ct[k,2], ct[k,4]) else "–"
    list(arm=getr("armBiased"), cons=getr("(Intercept)"), sa=getr("sa_grade"),
         n=nobs(m), adjr=sprintf("%.3f", summary(m)$adj.r.squared),
         rsd=sprintf("%.3f (df = %d)", sigma(m), df.residual(m)))
  }
}

build <- function(keep_policies, arm_label, file) {
  dd <- d %>% filter(policy %in% c("DEFAULT", keep_policies)) %>%
    mutate(arm = factor(ifelse(policy=="DEFAULT","Default","Biased"), levels=c("Default","Biased")))
  res <- list()
  for (out in c("coding","memo")) for (nm in names(SPECS)) {
    if (out=="memo" && nm=="M5") next
    res[[paste(out,nm)]] <- fit_one(dd, out, SPECS[[nm]]) }
  MODS <- names(SPECS)
  rowv <- function(out, field, f=identity) sapply(MODS, function(nm) {
    r <- res[[paste(out,nm)]]; if (is.null(r)) "–" else f(r[[field]]) })
  L <- c("| | M1 | M2 | M3 | M4 | M5 |", "|---|---|---|---|---|---|")
  for (out in c("coding","memo")) {
    L <- c(L, sprintf("| **%s Performance** | | | | | |", if (out=="coding") "Coding" else "Memo"),
      sprintf("| Constant | %s |", paste(rowv(out,"cons"), collapse=" | ")),
      sprintf("| %s | %s |", arm_label, paste(rowv(out,"arm"), collapse=" | ")),
      sprintf("| Stand-alone AI grade | %s |", paste(rowv(out,"sa"), collapse=" | ")),
      sprintf("| Observations | %s |", paste(rowv(out,"n", function(v) format(v, big.mark=",")), collapse=" | ")),
      sprintf("| Adjusted R2 | %s |", paste(rowv(out,"adjr"), collapse=" | ")),
      sprintf("| Residual SD | %s |", paste(rowv(out,"rsd"), collapse=" | ")))
  }
  ind <- rbind(c("Week fixed effects","Yes","Yes","Yes","Yes","Yes"),
               c("Student fixed effects","Yes","No","No","Yes","Yes"),
                              c("Prior-experience covariates","No","Yes","No","No","No"),
               c("Imbalanced covariates (18)","No","No","Yes","No","No"),
               c("Assignment-volume weighting","No","No","No","Yes","No"),
               c("Stand-alone AI grade","No","No","No","No","Yes"))
  for (i in seq_len(nrow(ind))) L <- c(L, paste0("| ", paste(ind[i,], collapse=" | "), " |"))
  writeLines(L, file.path(OUT, file)); cat(sprintf("\n##### %s #####\n", file)); cat(paste(L, collapse="\n"), "\n")
  res
}
all <- list(
  novelty     = build("NOVELTY_BIASED",     "Novelty-biased",     "table_performance_novelty.md"),
  reliability = build("RELIABILITY_BIASED", "Reliability-biased", "table_performance_reliability.md"),
  pooled      = build(c("NOVELTY_BIASED","RELIABILITY_BIASED"), "Biased (pooled)", "table_performance_pooled.md"))
saveRDS(all, file.path(OUT,"table_performance_by_direction.rds"))
