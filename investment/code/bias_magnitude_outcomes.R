# ==============================================================================
# bias_magnitude_outcomes.R
# Investment analog of second_figure_b1.R (political): does AI BIAS MAGNITUDE
# (No Bias / Moderate / Strong) relate to performance, confidence, and
# perception? SINGLE-AI only (bias magnitude is a single-AI arm property).
#
# BiasedCat:  No Bias      = Default
#             Moderate Bias = Somewhat Risk-Averse / Somewhat Risk-Seeking
#             Strong Bias   = Extremely Risk-Averse / Extremely Risk-Seeking
#             (Risk-Neutral EXCLUDED)
#
# Outcomes (pre-registered structure; political NID FE -> wave FE; UID random
# intercept DROPS — one obs per participant; UIdeo/UStance/AICorrectness have no
# analog):
#   1. Performance improvement : post_active_m2_ann ~ BiasedCat + wave + pre    (lm, HC3;
#      + flagged imbalanced covariates)
#   2. Number of reply turns   : n_reply_turns ~ BiasedCat + wave + pre         (lm, HC3;
#      spec2 of the engagement ladder — no additional covariate set)
#   3. Confidence change       : conf_change (ord) ~ BiasedCat + wave + pre     (MCMCglmm; spec2)
#   4. Perceived improvement   : perceived_improve (ord) ~ same spec2           (MCMCglmm;
#      console only — the stacked figure shows panels 1-3: star / circle / square)
#
# Covariate control: 3-group (bias-magnitude) omnibus imbalance check (ANOVA
# p<.05 or eta^2>=.01, "minimum" set — same criterion as the other scripts); the
# flagged covariates enter the PERFORMANCE model (1). Models 2-4 follow the
# registered engagement spec2 (grp + wave + pre_active_m2_ann) without the
# additional covariate set.
#
# Marginal means per bias level + FDR pairwise with Hedges' g; stacked 3-panel
# figure in the b1 style (line + nested 90/95/99 CI boxes over bias magnitude).
#
# Inputs: active_m2_treatment_data.csv , perceived_improvement.csv (§16),
#         participant_covariates.csv
#   setwd("investment/code"); source("_setup.R"); source("bias_magnitude_outcomes.R")
# ==============================================================================
suppressPackageStartupMessages({
  library(ggplot2); library(emmeans); library(dplyr); library(sandwich)
  library(lmtest); library(scales); library(MCMCglmm); library(patchwork)
})
set.seed(123)

.args <- commandArgs(FALSE); .f <- sub("^--file=", "", .args[grep("^--file=", .args)])
SCRIPT_DIR <- if (length(.f)) dirname(normalizePath(.f)) else getwd()
rd <- function(f) { p <- file.path(DATA_DIR, f); if (!file.exists(p)) p <- file.path(DATA_DIR, f)
                    read.csv(p, check.names = FALSE, stringsAsFactors = FALSE) }

m2 <- rd("active_m2_treatment_data.csv")
pi_df <- rd("perceived_improvement.csv") %>% filter(experiment == "single") %>%
  mutate(conf_change = post_conf - pre_conf)
rt_df <- rd("reply_turns.csv") %>% filter(experiment == "single")

d <- m2 %>%
  filter(ai_group %in% c("Default", "Extremely Risk-Averse", "Somewhat Risk-Averse",
                         "Extremely Risk-Seeking", "Somewhat Risk-Seeking")) %>%
  left_join(pi_df %>% select(participantId, perceived_improve, pre_conf, post_conf, conf_change),
            by = "participantId") %>%
  left_join(rt_df %>% select(participantId, n_reply_turns), by = "participantId") %>%
  mutate(
    BiasedCat = factor(case_when(
      ai_group == "Default" ~ "No Bias",
      ai_group %in% c("Somewhat Risk-Averse", "Somewhat Risk-Seeking") ~ "Moderate Bias",
      ai_group %in% c("Extremely Risk-Averse", "Extremely Risk-Seeking") ~ "Strong Bias"),
      levels = c("No Bias", "Moderate Bias", "Strong Bias")),
    wave = factor(wave))
cat("N by bias magnitude:\n"); print(table(d$BiasedCat))
LV <- c("No Bias", "Moderate Bias", "Strong Bias")
# 5-arm factor (= magnitude x direction) for the split panels 2-3
ARMS5 <- c("Default", "Somewhat Risk-Averse", "Extremely Risk-Averse",
           "Somewhat Risk-Seeking", "Extremely Risk-Seeking")
d$ai_group <- factor(d$ai_group, levels = ARMS5)
hedges_J <- function(n1, n2) { d0 <- n1 + n2 - 2; ifelse(d0 > 0, 1 - 3 / (4 * d0 - 1), 1) }
n_by <- table(d$BiasedCat)

# ── covariates + 3-group imbalance check (omnibus; "minimum" set) ─────────────
COVARS <- c("Age_enc", "Sex_enc", "work_in_finance_enc", "invest_own_score",
            "first_invest_time_enc", "trade_freq_12m_enc", "news_follow_freq_enc",
            "risk_pref_score", "time_preference_switch", "fin_lit_score",
            "fin_lit_selfassess_1_enc", "fin_overconfidence_1_enc",
            "portfolio_confidence_1_enc", "trust_ai_score",
            "market_outlook_2wk_enc", "market_volatility_2w_enc")
d <- merge(d, rd("participant_covariates.csv"), by = "participantId", all.x = TRUE)
for (cv in COVARS) { d[[cv]] <- as.numeric(d[[cv]]); d[[cv]][is.na(d[[cv]])] <- median(d[[cv]], na.rm = TRUE) }
bal <- do.call(rbind, lapply(COVARS, function(cv) {
  av <- anova(lm(d[[cv]] ~ d$BiasedCat))
  data.frame(covariate = cv, eta2 = av$`Sum Sq`[1] / sum(av$`Sum Sq`), anova_p = av$`Pr(>F)`[1])
}))
bal$flag <- ifelse(bal$anova_p < .05 | bal$eta2 >= .01, "IMBALANCED", "")
cat("\n=== 3-group imbalance check (flag if ANOVA p<.05 or eta^2>=.01) ===\n")
print(bal, row.names = FALSE, digits = 3)
COVF <- as.character(bal$covariate[bal$flag == "IMBALANCED"])
cov_rhs <- if (length(COVF)) paste("+", paste(COVF, collapse = " + ")) else ""
cat(sprintf("Controlling for %d imbalanced covariate(s): %s\n",
            length(COVF), if (length(COVF)) paste(COVF, collapse = ", ") else "(none)"))

# ── multi-level CI helper (90/95/99) ─────────────────────────────────────────
add_cis_norm <- function(mean, se) data.frame(
  lo90 = mean - se * qnorm(.95),  hi90 = mean + se * qnorm(.95),
  lo95 = mean - se * qnorm(.975), hi95 = mean + se * qnorm(.975),
  lo99 = mean - se * qnorm(.995), hi99 = mean + se * qnorm(.995))

# ══ 1. Performance improvement (lm ANCOVA, HC3) ═══════════════════════════════
mp <- lm(as.formula(paste("post_active_m2_ann ~ BiasedCat + wave + pre_active_m2_ann", cov_rhs)),
         data = d)
mp <- lm(as.formula("post_active_m2_ann ~ BiasedCat + wave + pre_active_m2_ann"),
         data = d)
ep <- emmeans(mp, ~ BiasedCat, vcov. = vcovHC(mp, type = "HC3"))
cat("\n=== 1. Performance (post Active M2, ANCOVA HC3) — marginal means ===\n")
sp <- as.data.frame(summary(ep)); print(sp, digits = 4)
cat("--- pairwise (FDR) + Hedges' g ---\n")
pwp <- as.data.frame(summary(pairs(ep, adjust = "fdr"), infer = TRUE))
pwp$hedges_g <- mapply(function(est, ct) {
  g <- strsplit(as.character(ct), " - ")[[1]]
  (est / sigma(mp)) * hedges_J(n_by[[g[1]]], n_by[[g[2]]]) }, pwp$estimate, pwp$contrast)
print(pwp[, c("contrast","estimate","SE","p.value","hedges_g")], row.names = FALSE, digits = 3)
viz_perf <- cbind(BiasedCat = sp$BiasedCat, emmean = sp$emmean, add_cis_norm(sp$emmean, sp$SE))

# ── 1b. performance SPLIT by direction: 5-arm model (figure panel 1) ──────────
# same covariate convention as the pooled panel-1 model (flagged covariates in)
n_by5 <- table(d$ai_group)
mp5 <- lm(as.formula(paste("post_active_m2_ann ~ ai_group + wave + pre_active_m2_ann", cov_rhs)),
          data = d)
mp5 <- lm(as.formula("post_active_m2_ann ~ ai_group + wave + pre_active_m2_ann"),
          data = d)
ep5 <- emmeans(mp5, ~ ai_group, vcov. = vcovHC(mp5, type = "HC3"))
cat("\n=== 1b. Performance, 5 arms (magnitude x direction; ANCOVA HC3) ===\n")
sp5 <- as.data.frame(summary(ep5)); print(sp5, digits = 4)
cat("--- pairwise (FDR across 10) + Hedges' g ---\n")
pwp5 <- as.data.frame(summary(pairs(ep5, adjust = "fdr"), infer = TRUE))
pwp5$hedges_g <- mapply(function(est, ct) {
  g <- strsplit(gsub("[()]", "", as.character(ct)), " - ")[[1]]
  (est / sigma(mp5)) * hedges_J(n_by5[[g[1]]], n_by5[[g[2]]]) }, pwp5$estimate, pwp5$contrast)
print(pwp5[, c("contrast","estimate","SE","p.value","hedges_g")], row.names = FALSE, digits = 3)
viz_perf5 <- data.frame(level = as.character(sp5$ai_group), emmean = sp5$emmean,
                        add_cis_norm(sp5$emmean, sp5$SE))

# ══ 2. Number of reply turns (lm, HC3) ════════════════════════════════════════
# spec2 of the engagement ladder: grp + wave + pre_active_m2_ann (baseline-
# adjusted; no additional covariate set — per the registered engagement spec)
dr <- d[!is.na(d$n_reply_turns) & !is.na(d$pre_active_m2_ann), ]
mr <- lm("n_reply_turns ~ BiasedCat + wave + pre_active_m2_ann", data = dr)
er <- emmeans(mr, ~ BiasedCat, vcov. = vcovHC(mr, type = "HC3"))
cat("\n=== 2. Number of reply turns (lm HC3) — marginal means ===\n")
sr <- as.data.frame(summary(er)); print(sr, digits = 4)
cat("--- pairwise (FDR) + Hedges' g ---\n")
pwr <- as.data.frame(summary(pairs(er, adjust = "fdr"), infer = TRUE))
pwr$hedges_g <- mapply(function(est, ct) {
  g <- strsplit(as.character(ct), " - ")[[1]]
  (est / sigma(mr)) * hedges_J(n_by[[g[1]]], n_by[[g[2]]]) }, pwr$estimate, pwr$contrast)
print(pwr[, c("contrast","estimate","SE","p.value","hedges_g")], row.names = FALSE, digits = 3)
viz_rt <- cbind(BiasedCat = sr$BiasedCat, emmean = sr$emmean, add_cis_norm(sr$emmean, sr$SE))

# ── 2b. reply turns SPLIT by direction: 5-arm spec2 model (figure panel 2) ────
n_by5 <- table(d$ai_group)
mr5 <- lm("n_reply_turns ~ ai_group + wave + pre_active_m2_ann", data = dr)
er5 <- emmeans(mr5, ~ ai_group, vcov. = vcovHC(mr5, type = "HC3"))
cat("\n=== 2b. Reply turns, 5 arms (magnitude x direction; lm HC3) ===\n")
sr5 <- as.data.frame(summary(er5)); print(sr5, digits = 4)
cat("--- pairwise (FDR across 10) + Hedges' g ---\n")
pwr5 <- as.data.frame(summary(pairs(er5, adjust = "fdr"), infer = TRUE))
pwr5$hedges_g <- mapply(function(est, ct) {
  g <- strsplit(gsub("[()]", "", as.character(ct)), " - ")[[1]]
  (est / sigma(mr5)) * hedges_J(n_by5[[g[1]]], n_by5[[g[2]]]) }, pwr5$estimate, pwr5$contrast)
print(pwr5[, c("contrast","estimate","SE","p.value","hedges_g")], row.names = FALSE, digits = 3)
viz_rt5 <- data.frame(level = as.character(sr5$ai_group), emmean = sr5$emmean,
                      add_cis_norm(sr5$emmean, sr5$SE))

# ── 2c. FOLLOW-UP PARTICIPATION (extensive margin): P(>=1 reply turn) ─────────
# The turn-COUNT contrast vs Default is marginal (p ~ .06 under spec2, .044-.049
# with covariate adjustment); the extensive margin is where the Default contrast
# is significant, so panel 2 plots this. Linear probability model + HC3 keeps the
# panel's symmetric-CI construction identical to panels 1 and 3; a logit
# cross-check is printed alongside.
dr$anyfu <- as.integer(dr$n_reply_turns > 0)
mfu5 <- lm("anyfu ~ ai_group + wave + pre_active_m2_ann", data = dr)
efu5 <- emmeans(mfu5, ~ ai_group, vcov. = vcovHC(mfu5, type = "HC3"))
cat("\n=== 2c. Follow-up participation, 5 arms (LPM HC3) ===\n")
sfu5 <- as.data.frame(summary(efu5)); print(sfu5, digits = 4)
cat("--- pairwise (FDR across 10) ---\n")
pwf5 <- as.data.frame(summary(pairs(efu5, adjust = "fdr"), infer = TRUE))
print(pwf5[, c("contrast","estimate","SE","p.value")], row.names = FALSE, digits = 3)
viz_fu5 <- data.frame(level = as.character(sfu5$ai_group), emmean = sfu5$emmean,
                      add_cis_norm(sfu5$emmean, sfu5$SE))

# direction-pooled version (the contrast annotated on panel 2) + logit check
dr$Dir3 <- factor(ifelse(dr$ai_group == "Default", "Default",
                  ifelse(grepl("Averse", dr$ai_group), "Averse", "Seeking")),
                  levels = c("Default", "Averse", "Seeking"))
mfu3 <- lm("anyfu ~ Dir3 + wave + pre_active_m2_ann", data = dr)
cat("\n--- 2c-pooled: Default vs Averse / Seeking (LPM HC3) ---\n")
print(coeftest(mfu3, vcov = vcovHC(mfu3, type = "HC3"))[2:3, , drop = FALSE], digits = 3)
gfu3 <- glm(anyfu ~ Dir3 + wave + pre_active_m2_ann, family = binomial(), data = dr)
ccg <- coeftest(gfu3, vcov = vcovHC(gfu3, type = "HC3"))[2:3, , drop = FALSE]
cat("--- logit cross-check (OR [95% CI], HC3) ---\n")
print(data.frame(term = sub("^Dir3", "", rownames(ccg)), OR = exp(ccg[, 1]),
                 lo = exp(ccg[, 1] - 1.96 * ccg[, 2]), hi = exp(ccg[, 1] + 1.96 * ccg[, 2]),
                 p = ccg[, 4]), row.names = FALSE, digits = 3)
cat("raw participation rates: ")
cat(paste(names(tapply(dr$anyfu, dr$Dir3, mean)),
          sprintf("%.1f%%", 100 * tapply(dr$anyfu, dr$Dir3, mean)), collapse = ", "), "\n")

# ── MCMC ordinal helper: posterior latent marginal means + pairwise ──────────
# spec2 (as row 2): yo ~ <fac> + wave + pre_active_m2_ann; the pre-M2 baseline
# is held at its mean in the marginal means (cancels in pairwise). `fac`/`lvls`
# select the treatment factor (BiasedCat pooled, or ai_group for the split).
# focal_ref: if set (e.g. "Default"), additionally prints the contrasts
# involving that level with FDR computed over THAT family only.
mcmc_latent <- function(outcome, label, fac = "BiasedCat", lvls = LV,
                        extra = character(0), focal_ref = NULL) {
  keep <- !is.na(d[[outcome]]) & !is.na(d$pre_active_m2_ann)
  for (cv in extra) keep <- keep & !is.na(d[[cv]])
  dat <- d[keep, ]
  dat$yo <- factor(dat[[outcome]], ordered = TRUE)
  covs <- c("pre_active_m2_ann", extra)
  # prior FIXES the residual variance at 1: in family="ordinal" it is NOT
  # identified (only effect/scale ratios are), and without fixing it the chain
  # drifts along the scale direction -> latent means, CI widths and pairwise
  # p-values become unstable across runs/seeds. Longer chain for mixing.
  mm <- MCMCglmm(as.formula(paste(c(paste("yo ~", fac, "+ wave"), covs),
                                  collapse = " + ")),
                 family = "ordinal", nitt = 55000, thin = 25, burnin = 5000,
                 prior = list(R = list(V = 1, fix = 1)),
                 data = as.data.frame(dat), verbose = FALSE)
  P <- as.matrix(mm$Sol); cn <- colnames(P)
  mw <- names(sort(table(dat$wave), decreasing = TRUE))[1]
  cov_shift <- rep(0, nrow(P))
  for (cv in covs)
    if (cv %in% cn) cov_shift <- cov_shift + P[, cv] * mean(dat[[cv]])
  M <- sapply(lvls, function(lv) {
    lp <- if ("(Intercept)" %in% cn) P[, "(Intercept)"] else rep(0, nrow(P))
    ce <- paste0(fac, lv); if (ce %in% cn) lp <- lp + P[, ce]
    we <- paste0("wave", mw);      if (we %in% cn) lp <- lp + P[, we]
    lp + cov_shift })                                  # nsamp x k (latent scale)
  # normal-approximation intervals (mean +/- z * posterior SD): symmetric, so
  # the plotted marker sits exactly at the box center, matching the lm panels'
  # construction. (Equal-tailed posterior quantiles are asymmetric under skew,
  # which off-centers the mean.) Pairwise tests below remain exact-posterior.
  viz <- data.frame(level = lvls, emmean = colMeans(M),
                    add_cis_norm(colMeans(M), apply(M, 2, sd)))
  psd <- sd(as.numeric(dat[[outcome]]), na.rm = TRUE)
  nb  <- table(dat[[fac]])
  cat(sprintf("\n=== %s (MCMC ordinal) — posterior latent marginal means ===\n", label))
  print(viz[, c("level","emmean","lo95","hi95")], row.names = FALSE, digits = 3)
  cat(sprintf("--- pairwise (posterior diff, FDR) + Hedges' g (pooled SD %.3f) ---\n", psd))
  k <- length(lvls); cm <- list(); m <- 0
  for (i in 1:(k-1)) for (j in (i+1):k) { m <- m + 1
    ds <- M[, j] - M[, i]; pv <- min(1, 2 * min(mean(ds < 0), mean(ds > 0)))
    Jc <- hedges_J(nb[[lvls[i]]], nb[[lvls[j]]])
    cm[[m]] <- data.frame(contrast = paste(lvls[j], "-", lvls[i]),
      estimate = mean(ds), lo = quantile(ds, .025), hi = quantile(ds, .975),
      p = pv, hedges_g = mean(ds) / psd * Jc) }
  cm <- do.call(rbind, cm); cm$p_fdr <- p.adjust(cm$p, "fdr")
  print(cm, row.names = FALSE, digits = 3)
  if (!is.null(focal_ref)) {
    fc <- cm[grepl(paste0("(^| )", focal_ref, "( |$)"), cm$contrast), ]
    fc$p_fdr <- p.adjust(fc$p, "fdr")
    cat(sprintf("--- focal contrasts vs %s (FDR over these %d only) ---\n",
                focal_ref, nrow(fc)))
    print(fc, row.names = FALSE, digits = 3)
  }
  viz
}

# ══ 3. Post confidence (ordinal ANCOVA: post_conf with pre_conf covariate,
#       the pre-registered post-confidence structure): pooled + 5-arm split ════
viz_conf  <- mcmc_latent("post_conf", "3a. Post confidence (pooled magnitude; ANCOVA on pre_conf)",
                         extra = "pre_conf")
viz_conf5 <- mcmc_latent("post_conf", "3b. Post confidence (5 arms; ANCOVA on pre_conf)",
                         fac = "ai_group", lvls = ARMS5, extra = "pre_conf")
# ── 3c. Default vs POOLED-DIRECTION biased groups (Som+Ext pooled within
#        direction); focal tests = Default vs Averse, Default vs Seeking ───────
d$Dir3 <- factor(ifelse(d$ai_group == "Default", "Default",
                 ifelse(grepl("Averse", d$ai_group), "Averse", "Seeking")),
                 levels = c("Default", "Averse", "Seeking"))
viz_conf3 <- mcmc_latent("post_conf",
    "3c. Post confidence (Default vs pooled Averse / pooled Seeking; ANCOVA on pre_conf)",
    fac = "Dir3", lvls = levels(d$Dir3), extra = "pre_conf", focal_ref = "Default")
# frequentist triangulation (standard practice for the ordinal results)
if (requireNamespace("ordinal", quietly = TRUE)) {
  d3 <- d %>% filter(!is.na(post_conf), !is.na(pre_conf), !is.na(pre_active_m2_ann))
  cl3 <- ordinal::clm(factor(post_conf, ordered = TRUE) ~ Dir3 + wave +
                        pre_active_m2_ann + pre_conf, data = d3)
  ct3 <- summary(cl3)$coefficients
  cat("--- clm cross-check (Dir3 rows; Wald p) ---\n")
  print(round(ct3[grep("^Dir3", rownames(ct3)), , drop = FALSE], 4))
}
# ── 3c WITH imbalanced-covariate controls: the omnibus imbalance check re-run
#    on the Dir3 grouping (the randomization cells relevant to THIS contrast);
#    flagged covariates enter both the MCMC ANCOVA and the clm cross-check ────
bal3 <- do.call(rbind, lapply(COVARS, function(cv) {
  av <- anova(lm(d[[cv]] ~ d$Dir3))
  data.frame(covariate = cv, eta2 = av$`Sum Sq`[1] / sum(av$`Sum Sq`),
             anova_p = av$`Pr(>F)`[1])
}))
bal3$flag <- ifelse(bal3$anova_p < .05 | bal3$eta2 >= .01, "IMBALANCED", "")
COVF3 <- as.character(bal3$covariate[bal3$flag == "IMBALANCED"])
cat(sprintf("\nDir3 imbalance check — %d flagged covariate(s): %s\n",
            length(COVF3), if (length(COVF3)) paste(COVF3, collapse = ", ") else "(none)"))
viz_conf3ctl <- mcmc_latent("post_conf",
    "3c-ctrl. Post confidence (Dir3; ANCOVA on pre_conf + imbalanced covariates)",
    fac = "Dir3", lvls = levels(d$Dir3), extra = c("pre_conf", COVF3),
    focal_ref = "Default")
if (requireNamespace("ordinal", quietly = TRUE)) {
  rhs3 <- paste(c("Dir3 + wave + pre_active_m2_ann + pre_conf", COVF3), collapse = " + ")
  cl3c <- ordinal::clm(as.formula(paste("factor(post_conf, ordered = TRUE) ~", rhs3)),
                       data = d3)
  ct3c <- summary(cl3c)$coefficients
  cat("--- clm cross-check WITH controls (Dir3 rows; Wald p) ---\n")
  print(round(ct3c[grep("^Dir3", rownames(ct3c)), , drop = FALSE], 4))
}
# ══ 4. Perceived improvement (ordinal; console only, not in the figure) ═══════
viz_perc <- mcmc_latent("perceived_improve", "4. Perceived improvement")

# ══ figure: b1 stacked panels — line + nested 90/95/99 CI boxes ═══════════════
BW <- 0.10; bw90 <- BW*1.4; bw95 <- BW*1.0; bw99 <- BW*0.7
prep <- function(v) { v$x <- match(v$BiasedCat, LV) - 1; v }        # 0/1/2
# exact five-pointed star polygon centered on (cx, cy); rx/ry in data units
mk_star <- function(cx, cy, rx, ry, id) {
  ao <- pi/2 + 2*pi*(0:4)/5; ai <- pi/2 + pi/5 + 2*pi*(0:4)/5
  a  <- as.vector(rbind(ao, ai)); r <- rep(c(1, 0.382), 5)
  data.frame(x = cx + cos(a)*rx*r, y = cy + sin(a)*ry*r, g = id)
}
panel <- function(v, dark, c90, c95, c99, ylab, shp, acc = 0.01, show_x = TRUE, ytit_r = 8, ybr = waiver(), ymul = 1) {
  v <- prep(v)
  p <- ggplot(v) +
    geom_rect(aes(xmin = x - bw99, xmax = x + bw99, ymin = lo99, ymax = hi99), fill = c99) +
    geom_rect(aes(xmin = x - bw95, xmax = x + bw95, ymin = lo95, ymax = hi95), fill = c95) +
    geom_rect(aes(xmin = x - bw90, xmax = x + bw90, ymin = lo90, ymax = hi90), fill = c90) +
    geom_line(aes(x = x, y = emmean), color = dark, linewidth = 1.1)
  if (identical(shp, "star")) {                 # polygon star: exactly centered
    # square panel (aspect.ratio = 1): x width = 2.95 units; y = data range x
    # 1.35 (expansion 5% bottom + 30% top) -> regular star, no distortion
    yspan <- diff(range(c(v$lo99, v$hi99))) * 1.35
    rx <- 0.078; ry <- rx * yspan / 2.95
    stars <- do.call(rbind, lapply(seq_len(nrow(v)),
               function(i) mk_star(v$x[i], v$emmean[i], rx, ry, i)))
    p <- p + geom_polygon(data = stars, aes(x = x, y = y, group = g),
                          fill = dark, color = NA)
  } else {
    p <- p + geom_point(aes(x = x, y = emmean), color = dark, shape = shp, size = 2.2)
  }
  p <- p +
    scale_x_continuous(breaks = 0:2, labels = c("Default", "Moderate Bias", "Strong Bias"), limits = c(-0.35, 2.6),
                       name = "AI Bias Magnitude") +
    scale_y_continuous(name = ylab,
                       labels = function(y) number_format(accuracy = acc)(y * ymul),
                       breaks = ybr,
                       expand = expansion(mult = c(0.05, 0.30))) +  # top headroom
    theme_classic() +
    theme(aspect.ratio = 1,                                # square panel frame
          text = element_text(family = "Avenir", color = "black"),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
          axis.line = element_blank(),
          axis.title.x = element_text(family = "Avenir", size = 9, margin = margin(t = 8)),
          axis.title.y = element_text(family = "Avenir", size = 9, margin = margin(r = ytit_r)),
          axis.text = element_text(family = "Avenir", size = 8, color = "black"),
          axis.ticks = element_line(color = "black", linewidth = 0.4),
          axis.ticks.length = unit(2.5, "pt"), panel.grid = element_blank(),
          plot.margin = margin(t = 10, r = 15, b = 6, l = 10))
  if (!show_x) p <- p + theme(axis.title.x = element_blank(),
                              axis.text.x  = element_blank(),
                              axis.ticks.x = element_blank())
  p
}
# ── split panel (2-3): magnitude x direction — Averse (lighter boxes, open
# marker, dashed line) vs Seeking (full color, filled marker, solid line),
# both fanning out from the shared No Bias point; direct series labels.
prep5 <- function(v) {
  mp <- data.frame(
    level = ARMS5, xm = c(0, 1, 2, 1, 2),
    dir   = c("None", "Averse", "Averse", "Seeking", "Seeking"))
  v <- merge(v, mp, by = "level")
  off <- 0.22
  v$x <- v$xm + ifelse(v$dir == "Averse", -off, ifelse(v$dir == "Seeking", off, 0))
  v[order(v$xm, v$dir), ]
}
# boxes: ONE muted family per panel (base color argument; 90/95/99 = base and
# its tints) chosen to harmonize with the series layer on top — dark PRGn
# direction colors for markers (white rim) and dashed lines (green = averse,
# purple = seeking, gray = No Bias). Magnitude stays on the x-axis.
DIRCOL <- c(Averse = "#1B7837", Seeking = "#762A83", None = "#666666")
tintc <- function(cc, f) colorRampPalette(c("white", cc))(101)[round(f * 100) + 1]
# Y-RANGE CONTROL, per panel call:
#   ylim = c(lo, hi)  -> HARD limits (exact panel range; padding then minimal)
#   yexp = c(lo, hi)  -> multiplicative padding below/above the data when ylim
#                        is NULL (default c(0.15, 0.35): 15% below, 35% above)
# w0 = width multiplier for the DEFAULT column's ladder (the not-yet-split
#      bar); 1 = same width as the split columns. Adjust per call or here.
panel_split <- function(v, ylab, shp, c90, c95, c99, acc = 0.01, show_x = TRUE,
                        ytit_r = 8, ymul = 1, ylim = NULL, yexp = c(0.15, 0.35),
                        w0 = 1.3, mc0 = NULL) {
  v <- prep5(v)
  v$f90 <- c90
  v$f95 <- c95
  v$f99 <- c99
  # Default (No Bias) column at full opacity; biased columns "transparent"
  # (solid tints -> crisp nesting, reads as transparency on white)
  bia <- v$dir != "None"
  v$f90[bia] <- sapply(v$f90[bia], tintc, f = 0.85)
  v$f95[bia] <- sapply(v$f95[bia], tintc, f = 0.85)
  v$f99[bia] <- sapply(v$f99[bia], tintc, f = 0.85)
  v$mcol <- unname(DIRCOL[v$dir])
  # mc0 = Default-column marker color (deep shade of the panel's ladder family);
  # NULL keeps the DIRCOL gray
  if (!is.null(mc0)) v$mcol[v$dir == "None"] <- mc0
  # marker outline: white rim on the split columns; none (self-colored) on the
  # Default column's unsplit marker, drawn slightly smaller (msz / star scale)
  v$ocol <- ifelse(v$dir == "None", v$mcol, "white")
  v$msz  <- ifelse(v$dir == "None", 0.8, 1)   # Default-marker size factor
  B2 <- 0.07
  wmul  <- ifelse(v$dir == "None", w0, 1)   # Default column thicker (unsplit)
  v$bw90 <- B2 * 1.4 * wmul; v$bw95 <- B2 * wmul; v$bw99 <- B2 * 0.7 * wmul
  lnA <- v[v$dir != "Seeking", ]; lnS <- v[v$dir != "Averse", ]
  p <- ggplot(v) +
    geom_rect(aes(xmin = x - bw99, xmax = x + bw99, ymin = lo99, ymax = hi99, fill = I(f99))) +
    geom_rect(aes(xmin = x - bw95, xmax = x + bw95, ymin = lo95, ymax = hi95, fill = I(f95))) +
    geom_rect(aes(xmin = x - bw90, xmax = x + bw90, ymin = lo90, ymax = hi90, fill = I(f90))) +
    geom_line(data = lnA, aes(x = x, y = emmean), color = DIRCOL[["Averse"]],
              linewidth = 0.9, linetype = "22") +
    geom_line(data = lnS, aes(x = x, y = emmean), color = DIRCOL[["Seeking"]],
              linewidth = 0.9, linetype = "22")
  if (identical(shp, "star")) {          # direction-colored polygon stars
    yspan <- if (!is.null(ylim)) diff(ylim) else
             diff(range(c(v$lo99, v$hi99))) * (1 + sum(yexp))
    rx <- 0.065; ry <- rx * yspan / 2.95
    stars <- do.call(rbind, lapply(seq_len(nrow(v)),
               function(i) mk_star(v$x[i], v$emmean[i],
                                   rx * v$msz[i], ry * v$msz[i], i)))
    stars$mcol <- rep(v$mcol, each = 10)
    stars$ocol <- rep(v$ocol, each = 10)
    p <- p + geom_polygon(data = stars, aes(x = x, y = y, group = g, fill = I(mcol),
                                            color = I(ocol)), linewidth = 0.3)
  } else {
    p <- p + geom_point(aes(x = x, y = emmean, fill = I(mcol), color = I(ocol),
                            size = I(2.4 * msz)),
                        shape = shp, stroke = 0.6)
  }
  p <- p +
    scale_x_continuous(breaks = 0:2, labels = c("Default", "Moderate Bias", "Strong Bias"), limits = c(-0.35, 2.6),
                       name = "AI Bias Magnitude") +
    scale_y_continuous(name = ylab,
                       labels = function(y) number_format(accuracy = acc)(y * ymul),
                       limits = ylim,
                       expand = if (is.null(ylim)) expansion(mult = yexp)
                                else expansion(mult = c(0.02, 0.02))) +
    theme_classic() +
    theme(aspect.ratio = 1,                                # square panel frame
          text = element_text(family = "Avenir", color = "black"),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
          axis.line = element_blank(),
          axis.title.x = element_text(family = "Avenir", size = 9, margin = margin(t = 8)),
          axis.title.y = element_text(family = "Avenir", size = 9, margin = margin(r = ytit_r)),
          axis.text = element_text(family = "Avenir", size = 8, color = "black"),
          axis.ticks = element_line(color = "black", linewidth = 0.4),
          axis.ticks.length = unit(2.5, "pt"), panel.grid = element_blank(),
          plot.margin = margin(t = 10, r = 15, b = 6, l = 10))
  if (!show_x) p <- p + theme(axis.title.x = element_blank(),
                              axis.text.x  = element_blank(),
                              axis.ticks.x = element_blank())
  p
}

# per-panel y-range: add `ylim = c(lo, hi)` to any call below for hard limits
# (in the panel's DATA units; panel 1 data are raw M2, e.g. ylim = c(-0.033, -0.018));
# or tune `yexp = c(below, above)` for proportional padding (default 0.15/0.35).
p1 <- panel_split(viz_perf5, expression("Post-interaction Active M"^2~""), "star",
                  "#228B22", "#66C266", "#BBE5BB",
                  acc = 0.01, ymul = 100,      # axis in units of 10^-2 -> 2-digit ticks
                  show_x = FALSE, ytit_r = 8, ylim = c(-0.04, -0.01),
                  mc0 = "#006400")  # green family, direction-colored stars; deep-green Default marker
p2 <- panel_split(viz_fu5, "Follow-up Participation Rate", 21,
                  "#8B4513", "#A0522D", "#D2B48C",
                  acc = 0.01, show_x = FALSE, ytit_r = 12, ylim = c(0.30, 0.95),
                  mc0 = "#654321")   # circles, brown ramp (saddle/sienna/tan); dark-brown Default marker
# (swap viz_fu5 -> viz_rt5 and the label back to "Number of Reply Turns",
#  ylim = c(0.5, 1.85), to plot the pre-registered turn count instead)
p3 <- panel_split(viz_conf5, "Post-interaction Confidence", 22,
                  "#FF8C00", "#FFA500", "#FFB347",
                  acc = 0.01, ytit_r = 12, ylim = c(3, 7.5),
                  mc0 = "#D2691E")       # squares, deep orange (pre-split ramp); deep-orange Default marker
pc <- p1 / p2 / p3
print(pc)

ggsave(file.path(FIG_DIR, "bias_magnitude_outcomes.png"), pc,
       width = 4, height = 8, dpi = 500)
cat(sprintf("\nSaved bias_magnitude_outcomes.png in %s\n", SCRIPT_DIR))
