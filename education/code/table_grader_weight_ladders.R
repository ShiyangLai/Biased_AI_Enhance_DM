# ==============================================================================
# table_grader_weight_ladders.R — Tables Sy and Sz for the robustness section.
#
#   Sy  grader-composite ladder: how the treatment estimates move with the choice
#       of which graders enter the outcome.
#   Sz  weighting ladder: how they move with the choice of weight, with the Kish
#       effective sample size for each.
#
# GRADER LABELS. coding_regrade / memo_regrade are already the MEAN OF THE TWO
# HUMAN GRADERS; the replication inputs carry no per-grader columns, so composites
# that would give a single human grader equal footing with an LLM cannot be built
# here. "Humans and Gemini equally weighted" therefore means the two-human mean
# and Gemini at half weight each, NOT one human plus Gemini.
#
# WEIGHTS. All weights in the primary block are week-level properties of the
# assignment template, fixed before any assistant was assigned and so unaffected
# by treatment. The post-treatment block (reply turns, words written) is shown
# only to document why it is not used: it is realised after assignment, and the
# raw variants collapse the effective sample.
#
# Outputs: figures/table_grader_composites.md, figures/table_weighting_ladder.md
#   setwd("education/code"); source("_setup.R"); source("table_grader_weight_ladders.R")
# ==============================================================================
suppressPackageStartupMessages({ library(dplyr); library(readr); library(sandwich); library(lmtest) })
IN <- DATA_DIR; OUT <- FIG_DIR
s  <- read_csv(file.path(IN,"scores_all_graders_student_week.csv"), show_col_types=FALSE)
g  <- read_csv(file.path(IN,"coding_regrade_student_week.csv"), show_col_types=FALSE)
mm <- read_csv(file.path(IN,"memo_regrade_student_week.csv"), show_col_types=FALSE)
wl <- read_csv(file.path(IN,"week_workload_summary.csv"), show_col_types=FALSE)
tel <- read_csv(file.path(IN, "telemetry_student_week.csv"), show_col_types = FALSE)
d <- s %>% filter(policy %in% c("DEFAULT","NOVELTY_BIASED","RELIABILITY_BIASED")) %>%
  left_join(g, by=c("student_id","week")) %>% left_join(mm, by=c("student_id","week")) %>%
  left_join(wl, by="week") %>% left_join(tel, by=c("student_id","week")) %>%
  mutate(across(c(conv_round, conv_length), ~ifelse(is.na(.x), 0, .x)),
         arm = relevel(factor(dplyr::recode(policy, DEFAULT="Default", NOVELTY_BIASED="Novelty",
                                            RELIABILITY_BIASED="Reliability")), ref="Default"),
         wk = factor(week), sid = factor(student_id))
al <- function(x, ref) x + (mean(ref[!is.na(x)&!is.na(ref)], na.rm=TRUE) - mean(x[!is.na(x)&!is.na(ref)], na.rm=TRUE))
star <- function(p) if (is.na(p)) "" else if (p<.001) "***" else if (p<.01) "**" else
                    if (p<.05) "*" else if (p<.1) "†" else ""
fmt  <- function(b,se,p) sprintf("%.3f%s (%.3f)", b, star(p), se)
est <- function(dat, ycol, w=NULL) {                       # primary spec, one outcome
  f <- as.formula(paste(ycol, "~ arm + wk + sid"))
  m <- if (is.null(w)) lm(f, data=dat) else lm(f, data=dat, weights=w)
  # zero-weight rows are dropped by lm(); for the post-treatment usage weights this
  # is the "unconsulted weeks fall out" behaviour, so record it rather than abort
  vc <- vcovCL(m, cluster=dat$sid[as.integer(rownames(model.frame(m)))]); ct <- coeftest(m, vcov=vc)
  list(nov=fmt(ct["armNovelty",1], ct["armNovelty",2], ct["armNovelty",4]),
       rel=fmt(ct["armReliability",1], ct["armReliability",2], ct["armReliability",4]),
       n=nobs(m), dropped=nrow(dat) - nobs(m))
}
# ── Sy: grader composites ────────────────────────────────────────────────────
d$H  <- d$coding_regrade                                                        # two-human mean
d$HG <- ifelse(is.na(d$gemini_asgn_score), d$H, (2*d$H + al(d$gemini_asgn_score,d$H))/3)
d$H_G<- ifelse(is.na(d$gemini_asgn_score), d$H, (d$H + al(d$gemini_asgn_score,d$H))/2)
d$HGP<- ifelse(is.na(d$gemini_asgn_score)|is.na(d$ai_asgn_score), d$HG,
               (2*d$H + al(d$gemini_asgn_score,d$H) + al(d$ai_asgn_score,d$H))/4)
d$MH  <- d$memo_regrade
d$MHG <- ifelse(is.na(d$gemini_memo_score), d$MH, (2*d$MH + al(d$gemini_memo_score,d$MH))/3)
d$MHGP<- ifelse(is.na(d$gemini_memo_score)|is.na(d$ai_memo_score), d$MHG,
                (2*d$MH + al(d$gemini_memo_score,d$MH) + al(d$ai_memo_score,d$MH))/4)
SY <- list(
  list("Two human graders only", "H", "MH"),
  list("Two humans + Gemini, equal per-grader weight (primary)", "HG", "MHG"),
  list("Human mean and Gemini equally weighted", "H_G", NA),
  list("Two humans + Gemini + GPT, equal per-grader weight", "HGP", "MHGP"))
L <- c("| Grader composite | Coding: Novelty | Coding: Reliability | Memo: Novelty | Memo: Reliability | n (coding / memo) |",
       "|---|---|---|---|---|---|")
for (r in SY) {
  dc <- d[!is.na(d[[r[[2]]]]) & d[[r[[2]]]] > 0, ]; ec <- est(dc, r[[2]])
  if (is.na(r[[3]])) { em <- list(nov="–", rel="–", n=NA)
  } else { dm <- d[!is.na(d[[r[[3]]]]) & d[[r[[3]]]] > 0, ]; em <- est(dm, r[[3]]) }
  L <- c(L, sprintf("| %s | %s | %s | %s | %s | %d / %s |", r[[1]], ec$nov, ec$rel, em$nov, em$rel,
                    ec$n, ifelse(is.na(em$n), "–", em$n)))
}
writeLines(L, file.path(OUT,"table_grader_composites.md")); cat("##### Table Sy #####\n"); cat(paste(L, collapse="\n"), "\n\n")
# ── Sz: weighting ladder ─────────────────────────────────────────────────────
dc <- d[!is.na(d$HG) & d$HG > 0, ]; dm <- d[!is.na(d$MH) & d$MH > 0, ]
kish <- function(w) sum(w)^2 / sum(w^2)
VOL <- names(wl)[-1]
rowf <- function(lab, wc, wm, note="") {
  ec <- est(dc, "HG", wc); em <- est(dm, "MH", wm)
  drop <- if (ec$dropped + em$dropped > 0)
            sprintf(" (drops %d / %d weeks)", ec$dropped, em$dropped) else ""
  sprintf("| %s%s | %s | %s | %s | %s | %.0f / %.0f |", lab, drop, ec$nov, ec$rel, em$nov, em$rel,
          if (is.null(wc)) nrow(dc) else kish(wc), if (is.null(wm)) nrow(dm) else kish(wm))
}
L <- c("| Weight | Coding: Novelty | Coding: Reliability | Memo: Novelty | Memo: Reliability | Kish effective n (coding / memo) |",
       "|---|---|---|---|---|---|",
       "| **Unweighted** | | | | | |", rowf("Equal weight per student-week (primary)", NULL, NULL),
       "| **Assignment volume (pre-treatment)** | | | | | |")
for (v in VOL) L <- c(L, rowf(v, dc[[v]], dm[[v]]))
L <- c(L, "| **Realised usage (post-treatment; not used)** | | | | | |",
       rowf("log(1 + reply turns)", log1p(dc$conv_round), log1p(dm$conv_round)),
       rowf("Reply turns", dc$conv_round, dm$conv_round),
       rowf("log(1 + words written)", log1p(dc$conv_length), log1p(dm$conv_length)),
       rowf("Words written", dc$conv_length, dm$conv_length))
writeLines(L, file.path(OUT,"table_weighting_ladder.md")); cat("##### Table Sz #####\n"); cat(paste(L, collapse="\n"), "\n")
