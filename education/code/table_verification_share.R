# ==============================================================================
# table_verification_share.R — Table S55: the single contrast quoted in the main
# text -- share of verification / other prompts, reliability-biased vs
# novelty-biased exchanges (44.4% vs 25.1%; delta = -0.194; exact permutation
# p = 0.055).
#
# One specification, matching delegation_by_arm.R's primary unit: each student-week
# is one observation weighted equally, with its category share as the outcome.
# HC3 standard errors; the exact permutation p enumerates all 12,376 arm-label
# assignments and the interval is its percentile bootstrap companion, both taken
# from d1_delegation_contrasts.csv so the table and Extended Data Fig. 2d cannot
# drift apart.
#
# Inputs : figures/delegation_annotations.csv, figures/d1_delegation_contrasts.csv
# Output : figures/table_verification_share.md
#   setwd("education/code"); source("_setup.R"); source("table_verification_share.R")
# ==============================================================================
suppressPackageStartupMessages({ library(dplyr); library(readr); library(sandwich); library(lmtest) })
IN <- DATA_DIR; OUT <- FIG_DIR
ann <- read_csv(file.path(IN, "delegation_annotations.csv"), show_col_types=FALSE) %>%
  filter(status=="annotated", policy_used %in% c("NOVELTY_BIASED","RELIABILITY_BIASED"),
         category != "not_a_prompt") %>%
  mutate(arm = factor(ifelse(policy_used=="NOVELTY_BIASED","Novelty","Reliability"),
                      levels=c("Novelty","Reliability")),
         .y = as.integer(category == "verification_other"))
sw <- ann %>% group_by(student_id, week, arm) %>%
  summarise(share = mean(.y), n_prompts = n(), .groups="drop")
prim <- read_csv(file.path(OUT,"d1_delegation_contrasts.csv"), show_col_types=FALSE) %>%
  filter(level=="student-week (primary)", metric=="Verification / other")

star <- function(p) if (is.na(p)) "" else if (p<.001) "***" else if (p<.01) "**" else
                    if (p<.05) "*" else if (p<.1) "†" else ""
fmt  <- function(b,se,p) sprintf("%.3f%s (%.3f)", b, star(p), se)
m  <- lm(share ~ arm, data=sw)
ct <- coeftest(m, vcov=vcovHC(m, "HC3"))
nn <- sum(sw$arm=="Novelty"); nr <- sum(sw$arm=="Reliability")

L <- c(
  "| | Share of verification prompts |",
  "|---|---|",
  sprintf("| Constant (novelty-biased) | %s |", fmt(ct["(Intercept)",1], ct["(Intercept)",2], ct["(Intercept)",4])),
  sprintf("| Reliability-biased | %s |", fmt(ct["armReliability",1], ct["armReliability",2], ct["armReliability",4])),
  sprintf("| Exact permutation p | %.3f |", prim$p_value),
  sprintf("| 95%% bootstrap CI | [%.3f, %.3f] |", prim$ci_lo, prim$ci_hi),
  "| | |",
  sprintf("| Implied share, novelty-biased | %.3f |", coef(m)[["(Intercept)"]]),
  sprintf("| Implied share, reliability-biased | %.3f |", sum(coef(m))),
  "| | |",
  sprintf("| Student-weeks (novelty / reliability) | %d / %d |", nn, nr),
  sprintf("| Annotated prompts | %s |", format(nrow(ann), big.mark=",")),
  sprintf("| Students | %d |", n_distinct(ann$student_id)),
  sprintf("| Adjusted R2 | %.3f |", summary(m)$adj.r.squared),
  sprintf("| Residual SD | %.3f (df = %d) |", sigma(m), df.residual(m)))
writeLines(L, file.path(OUT,"table_verification_share.md")); cat(paste(L, collapse="\n"), "\n")
