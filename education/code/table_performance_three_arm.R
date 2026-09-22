# ==============================================================================
# table_performance_three_arm.R — regression table for the three-arm classroom contrast
# (novelty-biased / reliability-biased vs default) on both graded outcomes,
# in the layout of the fact-checking and investment SI tables.
#
# Models (reference category = Default throughout):
#   M1  primary: arm + week FE + student FE, student-clustered SE
#   M2  no student effect: arm + week FE + prior-experience covariates, clustered
#   M3  no student effect: arm + week FE + the 18 covariates flagged by the
#       balance check, clustered
#   M4  primary, weighted by assignment volume (effective_source_chars), a
#       week-level quantity fixed before assignment and so unaffected by treatment
#   M5  primary + the stand-alone AI grade for that configuration and week.
#       Stand-alone runs exist for the coding assignment only, so this column is
#       unavailable for the memo. Because the stand-alone grade varies with
#       configuration, M5 is a decomposition rather than a robustness check.
#
# Outputs: figures/s4_regression_table.csv  (tidy)
#          figures/table_performance_three_arm.md   (rendered)
#   setwd("education/code"); source("_setup.R"); source("table_performance_three_arm.R")
# ==============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(sandwich); library(lmtest)
  })
IN <- DATA_DIR; OUT <- FIG_DIR

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
  mutate(arm = relevel(factor(dplyr::recode(policy, DEFAULT="Default",
            NOVELTY_BIASED="Novelty", RELIABILITY_BIASED="Reliability")), ref="Default"),
         wk = factor(week), sid = factor(student_id))
sh <- mean(d$coding_regrade[!is.na(d$gemini_asgn_score)], na.rm=TRUE) -
      mean(d$gemini_asgn_score[!is.na(d$coding_regrade)], na.rm=TRUE)
d$coding <- ifelse(is.na(d$gemini_asgn_score), d$coding_regrade,
                   (2*d$coding_regrade + d$gemini_asgn_score + sh)/3)
d$coding[is.na(d$coding_regrade)] <- NA
d$memo <- d$memo_regrade
for (cv in c("prior_prog_exp","prior_ml_exp")) {
  d[[cv]] <- as.numeric(d[[cv]]); d[[cv]][is.na(d[[cv]])] <- median(d[[cv]], na.rm=TRUE) }
NUMFLAG <- setdiff(FLAG, c("academic_level","prior_ide_tool"))
for (cv in NUMFLAG) { d[[cv]] <- as.numeric(d[[cv]]); d[[cv]][is.na(d[[cv]])] <- median(d[[cv]], na.rm=TRUE) }
for (cv in c("academic_level","prior_ide_tool"))
  d[[cv]] <- factor(ifelse(is.na(d[[cv]]) | d[[cv]]=="", "Missing", as.character(d[[cv]])))

star <- function(p) if (is.na(p)) "" else if (p<.001) "***" else if (p<.01) "**" else
                    if (p<.05) "*" else if (p<.1) "†" else ""
fmt  <- function(b,se,p) sprintf("%.3f%s (%.3f)", b, star(p), se)

fit_one <- function(out, spec) {
  dat <- d[!is.na(d[[out]]) & d[[out]] > 0, ]
  if (spec$needs_sa) dat <- dat[!is.na(dat$sa_grade), ]
  f <- as.formula(paste(out, "~", spec$rhs))
  if (spec$mixed) {
    m  <- lmer(f, data=dat); ct <- summary(m)$coefficients
    getr <- function(k) if (k %in% rownames(ct))
      fmt(ct[k,"Estimate"], ct[k,"Std. Error"], ct[k,"Pr(>|t|)"]) else "–"
    rsd <- sigma(m); dfr <- NA; adjr <- NA; n <- nobs(m)
  } else {
    # normalise weights to mean 1 so sigma() stays on the response scale;
    # this rescaling changes neither the coefficients nor the clustered SEs
    w <- if (!is.null(spec$wt)) dat[[spec$wt]] / mean(dat[[spec$wt]]) else NULL
    m  <- if (is.null(w)) lm(f, data=dat) else lm(f, data=dat, weights=w)
    vc <- vcovCL(m, cluster=dat$sid[as.integer(rownames(model.frame(m)))])
    ct <- coeftest(m, vcov=vc)
    getr <- function(k) if (k %in% rownames(ct)) fmt(ct[k,1], ct[k,2], ct[k,4]) else "–"
    rsd <- sigma(m); dfr <- df.residual(m); adjr <- summary(m)$adj.r.squared; n <- nobs(m)
  }
  list(nov = getr("armNovelty"), rel = getr("armReliability"),
       cons = getr("(Intercept)"), sa = getr("sa_grade"), n = n,
       adjr = adjr, rsd = rsd, dfr = dfr)
}

SPECS <- list(
  M1 = list(rhs="arm + wk + sid", mixed=FALSE, wt=NULL, needs_sa=FALSE),
  M2 = list(rhs="arm + wk + prior_prog_exp + prior_ml_exp", mixed=FALSE, wt=NULL, needs_sa=FALSE),
  M3 = list(rhs=paste("arm + wk +", paste(FLAG, collapse=" + ")), mixed=FALSE, wt=NULL, needs_sa=FALSE),
  M4 = list(rhs="arm + wk + sid", mixed=FALSE, wt="effective_source_chars", needs_sa=FALSE),
  M5 = list(rhs="arm + wk + sid + sa_grade", mixed=FALSE, wt=NULL, needs_sa=TRUE))

res <- list()
for (out in c("coding","memo")) for (nm in names(SPECS)) {
  if (out=="memo" && nm=="M5") { res[[paste(out,nm)]] <- NULL; next }
  res[[paste(out,nm)]] <- fit_one(out, SPECS[[nm]])
}
saveRDS(res, file.path(OUT,"table_performance_three_arm.rds"))

MODS <- names(SPECS)
row_of <- function(out, field) sapply(MODS, function(nm) {
  r <- res[[paste(out,nm)]]; if (is.null(r)) "–" else {
    v <- r[[field]]
    if (field %in% c("n")) format(v, big.mark=",")
    else if (field=="adjr") if (is.na(v)) "–" else sprintf("%.3f", v)
    else if (field=="rsd") if (is.na(r$dfr)) sprintf("%.3f", v) else sprintf("%.3f (df = %d)", v, r$dfr)
    else v } })

lines <- c("| | M1 | M2 | M3 | M4 | M5 |","|---|---|---|---|---|---|")
for (out in c("coding","memo")) {
  lab <- if (out=="coding") "**Coding Performance**" else "**Memo Performance**"
  lines <- c(lines, sprintf("| %s | | | | | |", lab))
  lines <- c(lines, sprintf("| Constant | %s |", paste(row_of(out,"cons"), collapse=" | ")))
  lines <- c(lines, sprintf("| Novelty-biased | %s |", paste(row_of(out,"nov"), collapse=" | ")))
  lines <- c(lines, sprintf("| Reliability-biased | %s |", paste(row_of(out,"rel"), collapse=" | ")))
  lines <- c(lines, sprintf("| Stand-alone AI grade | %s |", paste(row_of(out,"sa"), collapse=" | ")))
  lines <- c(lines, sprintf("| Observations | %s |", paste(row_of(out,"n"), collapse=" | ")))
  lines <- c(lines, sprintf("| Adjusted R2 | %s |", paste(row_of(out,"adjr"), collapse=" | ")))
  lines <- c(lines, sprintf("| Residual SD | %s |", paste(row_of(out,"rsd"), collapse=" | ")))
}
ind <- rbind(
  c("Week fixed effects","Yes","Yes","Yes","Yes","Yes"),
  c("Student fixed effects","Yes","No","No","Yes","Yes"),
    c("Prior-experience covariates","No","Yes","No","No","No"),
  c("Imbalanced covariates (18)","No","No","Yes","No","No"),
  c("Assignment-volume weighting","No","No","No","Yes","No"),
  c("Stand-alone AI grade","No","No","No","No","Yes"))
for (i in seq_len(nrow(ind))) lines <- c(lines, paste0("| ", paste(ind[i,], collapse=" | "), " |"))
writeLines(lines, file.path(OUT,"table_performance_three_arm.md"))
cat(paste(lines, collapse="\n"), "\n")
