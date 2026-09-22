# ==============================================================================
# table_delegation.R — Table S54: delegation categories of student prompts by
# assigned arm (Extended Data Fig. 2d), one panel per category, plus the 0-3
# autonomy score. Reference arm = Novelty, matching the Reliability - Novelty
# contrast that delegation_by_arm.R treats as primary.
#
# Two units of analysis, as in d1:
#   M1  student-week OLS on the category SHARE (each week weighted equally; the
#       unit behind the figure and the exact permutation test), HC3 SEs. The
#       exact-permutation p for Reliability - Novelty from d1 is shown beneath.
#   M2  message-level logit, 1[prompt is category k] ~ arm, student-clustered SE
#       (d1's secondary model; coefficients are log-odds)
#   M3  M2 + week fixed effects
#   M4  M2 with every student-week given equal total weight (quasi-binomial), so the
#       message model answers the same equal-week question as M1
# For the autonomy panel the same four columns are linear (0-3 scale).
#
# Inputs : figures/delegation_annotations.csv, figures/d1_delegation_contrasts.csv
# Outputs: figures/table_delegation.md, figures/table_delegation.rds
#   setwd("education/code"); source("_setup.R"); source("table_delegation.R")
# ==============================================================================
suppressPackageStartupMessages({ library(dplyr); library(readr); library(sandwich); library(lmtest) })
IN <- DATA_DIR; OUT <- FIG_DIR
CATS <- c("full_delegation","partial_delegation","guided_iteration","question_asking","verification_other")
CAT_LAB <- c("Full delegation","Partial delegation","Guided iteration","Question asking","Verification / other")
ARMS <- c("Novelty","Neutralized","Reliability")
ann <- read_csv(file.path(IN, "delegation_annotations.csv"), show_col_types=FALSE) %>%
  filter(status=="annotated") %>%
  mutate(arm = factor(dplyr::recode(policy_used, NOVELTY_BIASED="Novelty", NEUTRAL="Neutralized",
                                    RELIABILITY_BIASED="Reliability"), levels=ARMS)) %>%
  filter(!is.na(arm), category != "not_a_prompt") %>%
  mutate(autonomy_score = as.numeric(autonomy_score), wk = factor(week),
         swk = paste(student_id, week))
ann <- ann %>% group_by(swk) %>% mutate(w_eq = 1 / n()) %>% ungroup() %>%   # equal total weight per student-week
  mutate(w_eq = w_eq / mean(w_eq))                                          # mean-1 so sigma() stays on the response scale
sw <- ann %>% group_by(student_id, week, arm) %>%
  summarise(n_prompts=n(), mean_autonomy=mean(autonomy_score),
            !!!setNames(lapply(CATS, function(c) rlang::expr(mean(category == !!c))), paste0("share_",CATS)),
            .groups="drop")
cat(sprintf("prompts: %d | student-weeks: %d (%s) | students: %d\n", nrow(ann), nrow(sw),
            paste(names(table(sw$arm)), table(sw$arm), collapse=", "), n_distinct(ann$student_id)))
perm <- read_csv(file.path(OUT,"d1_delegation_contrasts.csv"), show_col_types=FALSE) %>%
  filter(level=="student-week (primary)") %>% dplyr::select(metric, p_value)

star <- function(p) if (is.na(p)) "" else if (p<.001) "***" else if (p<.01) "**" else if (p<.05) "*" else if (p<.1) "†" else ""
fmt  <- function(b,se,p) sprintf("%.3f%s (%.3f)", b, star(p), se)
getr <- function(ct,k) if (k %in% rownames(ct)) fmt(ct[k,1], ct[k,2], ct[k,4]) else "–"

fit_panel <- function(ycol_sw, ybin, lab, linear=FALSE) {
  # M1: student-week OLS, HC3
  m1 <- lm(as.formula(paste(ycol_sw, "~ arm")), data=sw); c1 <- coeftest(m1, vcov=vcovHC(m1,"HC3"))
  r1 <- list(cons=getr(c1,"(Intercept)"), neu=getr(c1,"armNeutralized"), rel=getr(c1,"armReliability"),
             n=nobs(m1), fit=sprintf("%.3f", summary(m1)$adj.r.squared),
             sd=sprintf("%.3f (df = %d)", sigma(m1), df.residual(m1)))
  pp <- perm$p_value[perm$metric==lab]; r1$perm <- if (length(pp)) sprintf("%.3f", pp) else "–"
  # message-level
  d <- ann; d$.y <- if (linear) d$autonomy_score else as.integer(d$category == ybin)
  mfit <- function(f, w=NULL) {
    if (!is.null(w)) d$.w <- w                      # weights are looked up inside `data`
    if (linear) { m <- if (is.null(w)) lm(f, data=d) else lm(f, data=d, weights=.w)
    } else { m <- if (is.null(w)) glm(f, family=binomial, data=d) else glm(f, family=quasibinomial, data=d, weights=.w) }
    ct <- coeftest(m, vcov=vcovCL(m, cluster=d$student_id))
    list(cons=getr(ct,"(Intercept)"), neu=getr(ct,"armNeutralized"), rel=getr(ct,"armReliability"), n=nobs(m),
         fit=if (linear) sprintf("%.3f", summary(m)$adj.r.squared) else "–",
         sd=if (linear) sprintf("%.3f (df = %d)", sigma(m), df.residual(m)) else
            if (is.null(w)) sprintf("logLik %.1f", as.numeric(logLik(m))) else "quasi-binomial", perm="–")
  }
  list(M1=r1, M2=mfit(.y ~ arm), M3=mfit(.y ~ arm + wk), M4=mfit(.y ~ arm, w=d$w_eq))
}
res <- list()
for (i in seq_along(CATS)) res[[CAT_LAB[i]]] <- fit_panel(paste0("share_",CATS[i]), CATS[i], CAT_LAB[i])
res[["Autonomy score (0-3)"]] <- fit_panel("mean_autonomy", NA, "Mean autonomy (0-3)", linear=TRUE)
saveRDS(res, file.path(OUT,"table_delegation.rds"))
row <- function(r, f) paste(sapply(c("M1","M2","M3","M4"), function(m) r[[m]][[f]]), collapse=" | ")
L <- c("| | M1 | M2 | M3 | M4 |", "|---|---|---|---|---|")
for (pn in names(res)) { r <- res[[pn]]
  L <- c(L, sprintf("| **%s** | | | | |", pn),
    sprintf("| Constant | %s |", row(r,"cons")),
    sprintf("| Neutralized | %s |", row(r,"neu")),
    sprintf("| Reliability-biased | %s |", row(r,"rel")),
    sprintf("| Exact permutation p, Reliability − Novelty | %s |", row(r,"perm")),
    sprintf("| Observations | %s |", row(r,"n")),
    sprintf("| Adjusted R2 | %s |", row(r,"fit")),
    sprintf("| Residual SD / fit | %s |", row(r,"sd")))
}
ind <- rbind(c("Unit of analysis","student-week","prompt","prompt","prompt"),
             c("Estimator","OLS (share)","logit","logit","logit, equal-week weights"),
             c("Week fixed effects","No","No","Yes","No"),
             c("Standard errors","HC3","student-clustered","student-clustered","student-clustered"))
for (i in seq_len(nrow(ind))) L <- c(L, paste0("| ", paste(ind[i,], collapse=" | "), " |"))
writeLines(L, file.path(OUT,"table_delegation.md")); cat(paste(L, collapse="\n"), "\n")
