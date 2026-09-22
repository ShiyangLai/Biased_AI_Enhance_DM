# ==============================================================================
# usefulness_spec_check.R — EXPLORATORY specification multiverse for the
# usefulness ("perceived helpfulness") outcome: does ANY reasonable model reach
# significance on any arm contrast? Same spirit as perceived_spec_check.R:
# report every spec together, don't cherry-pick. n=91 (biased: 7 novelty, 6
# reliability), so read a lone p<.05 as hypothesis-generating, not confirmatory.
#
# Reports both the 3-arm pairwise (Default/Novelty/Reliability) and the pooled
# Biased-vs-Default contrast, across: student FE (bm's spec), student RE, ordinal
# (clm/clmm/MCMC), and arm-only variants; plus a biased-only Reliability-vs-Novelty.
# p_fdr uses bm's family convention (biased-vs-Default focal, ÷ its size).
#
# Inputs : data/bm_student_week.csv, data/post_survey_student_week.csv
# Outputs: figures/usefulness_spec_check.csv (+ console)
#   setwd("education/code"); source("_setup.R"); source("usefulness_spec_check.R")
# ==============================================================================
set.seed(123)
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(emmeans); library(ordinal)
  library(lme4); library(lmerTest); library(MCMCglmm); library(sandwich)
})

IN <- DATA_DIR; OUT <- FIG_DIR

ps <- read_csv(file.path(IN, "post_survey_student_week.csv"), show_col_types = FALSE) %>%
  dplyr::select(student_id, week, usefulness)
d <- read_csv(file.path(IN, "bm_student_week.csv"), show_col_types = FALSE) %>%
  left_join(ps, by = c("student_id", "week")) %>%
  mutate(arm = factor(dplyr::recode(direction, None = "Default",
                                    Novelty = "Novelty", Reliability = "Reliability"),
                      levels = c("Default", "Novelty", "Reliability")),
         BiasedCat = factor(bias_pooled, levels = c("No Bias", "Biased")),
         week = factor(week), student_id = as.factor(student_id))
for (cv in c("prior_prog_exp", "prior_ml_exp")) {
  d[[cv]] <- as.numeric(d[[cv]]); d[[cv]][is.na(d[[cv]])] <- median(d[[cv]], na.rm = TRUE) }
dv <- d %>% filter(!is.na(usefulness))
dv$yo <- factor(dv$usefulness, ordered = TRUE); dv$y <- as.numeric(dv$usefulness)
cat("usefulness n =", nrow(dv), "| by arm:", paste(names(table(dv$arm)), table(dv$arm), sep="=", collapse=", "), "\n")

pw_emm <- function(emm, spec, ref) {
  pw <- suppressWarnings(as.data.frame(summary(pairs(emm, adjust = "none"), infer = TRUE)))
  pc <- grep("^p\\.value$", names(pw), value = TRUE)
  is_ref <- grepl(ref, as.character(pw$contrast), fixed = TRUE)
  pf <- rep(NA_real_, nrow(pw)); pf[is_ref] <- p.adjust(pw[[pc]][is_ref], "fdr")
  if (any(!is_ref)) pf[!is_ref] <- p.adjust(pw[[pc]][!is_ref], "fdr")
  data.frame(spec = spec, contrast = as.character(pw$contrast),
             estimate = pw$estimate, p_raw = pw[[pc]], p_fdr = pf, row.names = NULL)
}
try_add <- function(lst, expr, spec) { r <- tryCatch(expr, error = function(e) {
  cat(sprintf("  [%s] failed: %s\n", spec, conditionMessage(e))); NULL }); if (!is.null(r)) lst[[spec]] <- r; lst }

R <- list()
# ── 3-arm pairwise specs ──────────────────────────────────────────────────────
R <- try_add(R, { m <- lm(y ~ arm + week + factor(student_id), data = dv)
  pw_emm(emmeans(m, ~ arm, vcov. = vcovCL(m, cluster = dv$student_id)), "arm: student FE (bm) + clustered", "Default") }, "FE_arm")
R <- try_add(R, pw_emm(emmeans(lmer(y ~ arm + week + prior_prog_exp + prior_ml_exp + (1|student_id), data = dv), ~ arm),
                       "arm: linear RE + week + priors", "Default"), "RE_arm")
R <- try_add(R, { m <- lm(y ~ arm, data = dv)
  pw_emm(emmeans(m, ~ arm, vcov. = vcovCL(m, cluster = dv$student_id)), "arm: linear, arm-only, clustered", "Default") }, "lin_arm")
R <- try_add(R, pw_emm(emmeans(clm(yo ~ arm + week + prior_prog_exp + prior_ml_exp, data = dv), ~ arm),
                       "arm: clm ordinal + week + priors", "Default"), "clm_arm")
R <- try_add(R, pw_emm(emmeans(clm(yo ~ arm, data = dv), ~ arm), "arm: clm ordinal, arm-only", "Default"), "clm_arm_only")
R <- try_add(R, pw_emm(emmeans(clmm(yo ~ arm + week + prior_prog_exp + prior_ml_exp + (1|student_id), data = dv), ~ arm),
                       "arm: clmm ordinal RE + week + priors", "Default"), "clmm_arm")
R <- try_add(R, {   # MCMCglmm ordinal, student RE (posterior latent pairwise)
  pr <- list(R = list(V = 1, fix = 1), G = list(G1 = list(V = 1, nu = 1, alpha.mu = 0, alpha.V = 25^2)))
  mm <- MCMCglmm(yo ~ arm + week + prior_prog_exp + prior_ml_exp, random = ~student_id, family = "ordinal",
                 nitt = 55000, thin = 25, burnin = 5000, prior = pr, data = as.data.frame(dv), verbose = FALSE)
  P <- as.matrix(mm$Sol); cn <- colnames(P); mw <- names(sort(table(dv$week), decreasing = TRUE))[1]
  sh <- P[,"prior_prog_exp"]*mean(dv$prior_prog_exp) + P[,"prior_ml_exp"]*mean(dv$prior_ml_exp)
  M <- sapply(c("Default","Novelty","Reliability"), function(lv){ lp <- P[,"(Intercept)"]+sh
    ce <- paste0("arm",lv); if (ce %in% cn) lp <- lp+P[,ce]; we <- paste0("week",mw); if (we %in% cn) lp <- lp+P[,we]; lp })
  cs <- list(c("Default","Novelty"), c("Default","Reliability"), c("Novelty","Reliability"))
  rr <- do.call(rbind, lapply(cs, function(p){ ds <- M[,p[1]]-M[,p[2]]
    data.frame(contrast=paste(p[1],"-",p[2]), estimate=mean(ds), p_raw=min(1,2*min(mean(ds<0),mean(ds>0)))) }))
  is_ref <- grepl("Default", rr$contrast); rr$p_fdr <- NA_real_
  rr$p_fdr[is_ref] <- p.adjust(rr$p_raw[is_ref],"fdr"); rr$p_fdr[!is_ref] <- p.adjust(rr$p_raw[!is_ref],"fdr")
  data.frame(spec="arm: MCMCglmm ordinal RE + week + priors", rr, row.names=NULL) }, "mcmc_arm")

# ── pooled Biased-vs-Default specs ────────────────────────────────────────────
R <- try_add(R, { m <- lm(y ~ BiasedCat + week + factor(student_id), data = dv)
  pw_emm(emmeans(m, ~ BiasedCat, vcov. = vcovCL(m, cluster = dv$student_id)), "pooled: student FE (bm) + clustered", "No Bias") }, "FE_pool")
R <- try_add(R, pw_emm(emmeans(lmer(y ~ BiasedCat + week + prior_prog_exp + prior_ml_exp + (1|student_id), data = dv), ~ BiasedCat),
                       "pooled: linear RE + week + priors", "No Bias"), "RE_pool")
R <- try_add(R, pw_emm(emmeans(clm(yo ~ BiasedCat + week + prior_prog_exp + prior_ml_exp, data = dv), ~ BiasedCat),
                       "pooled: clm ordinal + week + priors", "No Bias"), "clm_pool")

# ── biased-only Reliability vs Novelty ────────────────────────────────────────
R <- try_add(R, { db <- dv %>% filter(arm != "Default") %>% mutate(arm = droplevels(arm))
  w <- suppressWarnings(wilcox.test(y ~ arm, data = db)); tt <- t.test(y ~ arm, data = db)
  rbind(data.frame(spec="biased-only: Wilcoxon (Nov vs Rel)", contrast="Novelty - Reliability", estimate=NA, p_raw=w$p.value, p_fdr=w$p.value),
        data.frame(spec="biased-only: Welch t (Nov vs Rel)",  contrast="Novelty - Reliability", estimate=unname(diff(rev(tt$estimate))), p_raw=tt$p.value, p_fdr=tt$p.value)) }, "biased_only")

all <- bind_rows(R)
all$sig_raw <- ifelse(all$p_raw < .05, "*", "")
all$sig_fdr <- ifelse(all$p_fdr < .05, "*", "")
cat("\n===== USEFULNESS: all specifications, all contrasts =====\n\n")
print(as.data.frame(all %>% mutate(across(where(is.numeric), ~round(., 3)))), row.names = FALSE)

nsig_raw <- all %>% filter(p_raw < .05); nsig_fdr <- all %>% filter(p_fdr < .05)
cat(sprintf("\n%d of %d contrast-by-spec cells reach raw p<.05; %d survive the family FDR.\n",
            nrow(nsig_raw), nrow(all), nrow(nsig_fdr)))
if (nrow(nsig_raw)) { cat("Raw p<.05 in:\n"); print(as.data.frame(nsig_raw %>% transmute(spec, contrast,
    p_raw = round(p_raw,3), p_fdr = round(p_fdr,3))), row.names = FALSE) } else cat("No specification reaches even raw p<.05 on any contrast.\n")
write_csv(all, file.path(OUT, "usefulness_spec_check.csv"))
cat("\nSaved output/usefulness_spec_check.csv\nDone: usefulness_spec_check.R\n")
