# ==============================================================================
# table_final_project.R — Table S56b: assigned-arm exposure and final-project
# innovativeness (Extended Data Fig. 4j).
#
# UNIT AND REFERENCE. Team projects are graded once, so the project is the unit
# (n = 21 with an innovativeness score, from 22 projects and 39 students).
# Exposure is the team-mean share of members assigned each arm. Every student
# received exactly two of {novelty, reliability, neutralized, questioning}, and
# questioning is dropped, so NO project has zero studied exposure and there is no
# observable baseline arm: the three coefficients are effects relative to a common
# unmodelled reference, which is what the ladder in Fig. 4j plots. The contrast of
# interest is therefore between arms, and the neutralized-versus-pooled-biased row
# is reported beneath.
#
# Columns:
#   M1 Huber robust regression (the figure's model)
#   M2 OLS with HC3 standard errors
#   M3 OLS with classical standard errors
# All three share the project unit and the same regressors, differing only in how
# the coefficients and their standard errors are estimated; the table is therefore
# a sensitivity check on inference rather than on specification. A student-level
# model clustered by project and a team-size-weighted variant were also fitted and
# agree in sign (see table_final_project.rds).
# Randomization inference for the neutralized-versus-pooled-biased contrast
# (20,000 re-draws of the arm assignment) is reported for M1.
#
# Inputs : data/final_project_student.csv, data/final_project_docs.csv
# Output : figures/table_final_project.md
#   setwd("education/code"); source("_setup.R"); source("table_final_project.R")
# ==============================================================================
set.seed(11)
suppressPackageStartupMessages({ library(dplyr); library(readr); library(sandwich); library(lmtest) })
IN <- DATA_DIR; OUT <- FIG_DIR
stu <- read_csv(file.path(IN,"final_project_student.csv"), show_col_types=FALSE) %>% filter(!is.na(doc_code))
doc <- read_csv(file.path(IN,"final_project_docs.csv"), show_col_types=FALSE)
pa <- stu %>% group_by(doc_code) %>%
  summarise(nov=mean(got_nov), rel=mean(got_rel), neu=mean(got_neu),
            team=n(), .groups="drop") %>%
  left_join(doc %>% dplyr::select(doc_code, innov_mean), by="doc_code") %>% filter(!is.na(innov_mean))
sl <- stu %>% left_join(doc %>% dplyr::select(doc_code, innov_mean), by="doc_code") %>% filter(!is.na(innov_mean))
cat(sprintf("projects: %d | students: %d | team size %d-%d\n", nrow(pa), nrow(sl), min(pa$team), max(pa$team)))

star <- function(p) if (is.na(p)) "" else if (p<.001) "***" else if (p<.01) "**" else
                    if (p<.05) "*" else if (p<.1) "†" else ""
fmt  <- function(b,se,p) sprintf("%.3f%s (%.3f)", b, star(p), se)
# contrast helper: k'b with a supplied vcov
con <- function(b, V, k, df) { e <- sum(k*b); s <- sqrt(as.numeric(t(k) %*% V %*% k))
  c(e, s, 2*pt(abs(e/s), df, lower.tail=FALSE)) }
pack <- function(b, V, df, n, adjr, rsd) {
  nm <- names(b); K <- function(...) { k <- setNames(rep(0,length(b)), nm); for (a in list(...)) k[a[[1]]] <- a[[2]]; k }
  g <- function(t) { r <- con(b,V,K(list(t,1)),df); fmt(r[1],r[2],r[3]) }
  cb <- con(b, V, K(list("neu",1), list("nov",-.5), list("rel",-.5)), df)
  list(nov=g("nov"), rel=g("rel"), neu=g("neu"),
       vsb=fmt(cb[1],cb[2],cb[3]), n=n, adjr=adjr, rsd=rsd)
}
# M1 Huber
m1 <- MASS::rlm(innov_mean ~ nov+rel+neu, data=pa); df1 <- nrow(pa)-length(coef(m1))
R1 <- pack(coef(m1), vcov(m1), df1, nrow(pa), "–", sprintf("%.3f (df = %d)", m1$s, df1))
# M2/M3 OLS
mo <- lm(innov_mean ~ nov+rel+neu, data=pa); dfo <- df.residual(mo)
R2 <- pack(coef(mo), vcovHC(mo,"HC3"), dfo, nrow(pa), sprintf("%.3f", summary(mo)$adj.r.squared),
           sprintf("%.3f (df = %d)", sigma(mo), dfo))
R3 <- pack(coef(mo), vcov(mo), dfo, nrow(pa), sprintf("%.3f", summary(mo)$adj.r.squared),
           sprintf("%.3f (df = %d)", sigma(mo), dfo))
# M4 student level, clustered by project
ms <- lm(innov_mean ~ got_nov+got_rel+got_neu, data=sl)
bs <- coef(ms); names(bs) <- sub("^got_","",names(bs))
Vs <- vcovCL(ms, cluster=sl$doc_code); dimnames(Vs) <- list(names(bs), names(bs))
R4 <- pack(bs, Vs, n_distinct(sl$doc_code)-4, nrow(sl), sprintf("%.3f", summary(ms)$adj.r.squared),
           sprintf("%.3f (df = %d)", sigma(ms), df.residual(ms)))
# M5 weighted by team AI minutes
pa$w_team <- pa$team / mean(pa$team)
mw <- lm(innov_mean ~ nov+rel+neu, data=pa, weights=w_team); dfw <- df.residual(mw)
stopifnot(nobs(mw) == nrow(pa))   # guard: a zero weight would silently drop a project
R5 <- pack(coef(mw), vcovHC(mw,"HC3"), dfw, nrow(pa), sprintf("%.3f", summary(mw)$adj.r.squared),
           sprintf("%.3f (df = %d)", sigma(mw), dfw))
# randomization inference on the M1 pooled contrast
obs <- { b <- coef(m1); b[["neu"]] - .5*(b[["nov"]]+b[["rel"]]) }
perm <- replicate(20000, { i <- sample(nrow(pa)); x <- pa
  x$nov <- pa$nov[i]; x$rel <- pa$rel[i]; x$neu <- pa$neu[i]
  cc <- coef(MASS::rlm(innov_mean ~ nov+rel+neu, data=x)); cc[["neu"]] - .5*(cc[["nov"]]+cc[["rel"]]) })
ri <- mean(abs(perm) >= abs(obs))
saveRDS(list(M1=R1,M2=R2,M3=R3,M4=R4,M5=R5), file.path(OUT,"table_final_project.rds"))
res <- list(M1=R1,M2=R2,M3=R3)   # reported columns
row <- function(f) paste(sapply(res, `[[`, f), collapse=" | ")
L <- c("| | M1 | M2 | M3 |", "|---|---|---|---|",
  sprintf("| Novelty-biased exposure | %s |", row("nov")),
  sprintf("| Reliability-biased exposure | %s |", row("rel")),
  sprintf("| Neutralized exposure | %s |", row("neu")),
  sprintf("| Neutralized − pooled biased | %s |", row("vsb")),
  sprintf("| Randomization-inference p (M1 contrast) | %.3f | | |", ri),
  sprintf("| Observations | %s |", row("n")),
  "| Unit of analysis | project | project | project |",
  sprintf("| Adjusted R2 | %s |", row("adjr")),
  sprintf("| Residual SD | %s |", row("rsd")),
  "| Estimator | Huber robust | OLS | OLS |",
  "| Standard errors | Huber | HC3 | classical |")
writeLines(L, file.path(OUT,"table_final_project.md")); cat(paste(L, collapse="\n"), "\n")
