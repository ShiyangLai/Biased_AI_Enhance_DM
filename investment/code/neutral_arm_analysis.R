# ==============================================================================
# neutral_arm_analysis.R
# Brings the RISK-NEUTRAL arm back into the single-AI analysis. Every other
# script in this project excludes it (the pre-registered treatment ladder
# compares Default against the four biased arms); here it is a fourth group, so
# the two "unbiased" conditions can be contrasted directly:
#
#   Averse  = Somewhat + Extremely Risk-Averse   (biased, defensive direction)
#   Default = default Gemini persona             (unbiased, unlabelled)
#   Neutralized = Risk-Neutral persona               (unbiased, EXPLICITLY labelled)
#   Seeking = Somewhat + Extremely Risk-Seeking  (biased, aggressive direction)
#
# The Default-vs-Neutralized contrast is the interesting one: both give unbiased
# advice, but only Neutralized announces a risk stance, so any difference isolates
# the effect of DECLARING a stance from the effect of the stance's content
# (compare plot_active_m2_priming.R, where the label is manipulated with the
# content held at default).
#
# Model (pre-registered structure): post_active_m2_ann ~ grp + wave + pre, HC3;
# emmeans marginal means, FDR pairwise with Hedges' g; per-wave versions drop
# the wave FE. A 4-group covariate imbalance check is printed, and a
# covariate-adjusted spec is reported alongside the primary one.
#
# Inputs:  active_m2_treatment_data.csv , participant_covariates.csv,
#          reply_turns.csv (§17), perceived_improvement.csv (§16)
# Outputs: neutral_arm_active_m2.png        (4-group ladder)
#          neutral_arm_active_m2_by_wave.png (per-wave panels)
#   setwd("investment/code"); source("_setup.R"); source("neutral_arm_analysis.R")
# ==============================================================================
suppressPackageStartupMessages({
  library(ggplot2); library(emmeans); library(dplyr); library(sandwich)
  library(lmtest); library(scales)
})

.args <- commandArgs(FALSE); .f <- sub("^--file=", "", .args[grep("^--file=", .args)])
SCRIPT_DIR <- if (length(.f)) dirname(normalizePath(.f)) else getwd()
rd <- function(f) { p <- file.path(DATA_DIR, f); if (!file.exists(p)) p <- file.path(DATA_DIR, f)
                    read.csv(p, check.names = FALSE, stringsAsFactors = FALSE) }

GRP <- c("Averse", "Default", "Neutralized", "Seeking")
hedges_J <- function(n1, n2) { d0 <- n1 + n2 - 2; ifelse(d0 > 0, 1 - 3 / (4 * d0 - 1), 1) }

d <- rd("active_m2_treatment_data.csv") %>%
  mutate(grp = case_when(
    ai_group %in% c("Somewhat Risk-Averse", "Extremely Risk-Averse")   ~ "Averse",
    ai_group == "Default"                                             ~ "Default",
    ai_group == "Risk-Neutral"                                        ~ "Neutralized",
    ai_group %in% c("Somewhat Risk-Seeking", "Extremely Risk-Seeking") ~ "Seeking",
    TRUE ~ NA_character_)) %>%
  filter(!is.na(grp), !is.na(pre_active_m2_ann), !is.na(post_active_m2_ann)) %>%
  mutate(grp = factor(grp, levels = GRP), wave = factor(wave))
cat("N by group:\n"); print(table(d$grp))
cat("\nraw pre/post Active M2 by group:\n")
print(d %>% group_by(grp) %>%
        summarise(n = n(), pre = mean(pre_active_m2_ann), post = mean(post_active_m2_ann),
                  delta = mean(post_active_m2_ann - pre_active_m2_ann), .groups = "drop") %>%
        as.data.frame(), digits = 3, row.names = FALSE)

# ── 4-group covariate imbalance check ────────────────────────────────────────
COVARS <- c("Age_enc", "Sex_enc", "work_in_finance_enc", "invest_own_score",
            "first_invest_time_enc", "trade_freq_12m_enc", "news_follow_freq_enc",
            "risk_pref_score", "time_preference_switch", "fin_lit_score",
            "fin_lit_selfassess_1_enc", "fin_overconfidence_1_enc",
            "portfolio_confidence_1_enc", "trust_ai_score",
            "market_outlook_2wk_enc", "market_volatility_2w_enc")
d <- merge(d, rd("participant_covariates.csv"), by = "participantId", all.x = TRUE)
for (cv in COVARS) { d[[cv]] <- as.numeric(d[[cv]]); d[[cv]][is.na(d[[cv]])] <- median(d[[cv]], na.rm = TRUE) }
bal <- do.call(rbind, lapply(COVARS, function(cv) {
  av <- anova(lm(d[[cv]] ~ d$grp))
  data.frame(covariate = cv, eta2 = av$`Sum Sq`[1] / sum(av$`Sum Sq`), anova_p = av$`Pr(>F)`[1]) }))
bal$flag <- ifelse(bal$anova_p < .05 | bal$eta2 >= .01, "IMBALANCED", "")
cat("\n=== 4-group imbalance check (flag if ANOVA p<.05 or eta^2>=.01) ===\n")
print(bal[order(bal$anova_p), ], row.names = FALSE, digits = 3)
COVF <- as.character(bal$covariate[bal$flag == "IMBALANCED"])
cov_rhs <- if (length(COVF)) paste("+", paste(COVF, collapse = " + ")) else ""
cat(sprintf("Flagged: %d -> %s\n", length(COVF),
            if (length(COVF)) paste(COVF, collapse = ", ") else "(none)"))

# ── primary model + pairwise ─────────────────────────────────────────────────
fit_report <- function(rhs, label, data = d) {
  m <- lm(as.formula(paste("post_active_m2_ann ~ grp +", rhs)), data = data)
  V <- vcovHC(m, type = "HC3"); em <- emmeans(m, ~ grp, vcov. = V)
  cat(sprintf("\n=== %s ===\n", label))
  es <- as.data.frame(summary(em)); es$emmean_pct <- es$emmean * 100
  print(es[, c("grp", "emmean", "SE", "emmean_pct")], row.names = FALSE, digits = 4)
  pw <- as.data.frame(summary(pairs(em, adjust = "fdr"), infer = TRUE))
  nb <- table(data$grp)
  pw$hedges_g <- mapply(function(est, ct) {
    g <- strsplit(as.character(ct), " - ")[[1]]
    (est / sigma(m)) * hedges_J(nb[[g[1]]], nb[[g[2]]]) }, pw$estimate, pw$contrast)
  pw[, c("estimate", "lower.CL", "upper.CL")] <- pw[, c("estimate", "lower.CL", "upper.CL")] * 100
  cat("--- pairwise (FDR across 6), estimates in percentage units ---\n")
  print(pw[, c("contrast", "estimate", "lower.CL", "upper.CL", "hedges_g", "p.value")],
        row.names = FALSE, digits = 3)
  invisible(list(m = m, em = em, es = as.data.frame(summary(em))))
}
main <- fit_report("wave + pre_active_m2_ann", "PRIMARY: post ~ grp + wave + pre (HC3)")
if (length(COVF))
  invisible(fit_report(paste("wave + pre_active_m2_ann", cov_rhs),
                       "ROBUSTNESS: + imbalanced covariates"))

# focal: the two unbiased arms
cat("\n=== FOCAL: Default vs Neutralized (both unbiased; label-only difference) ===\n")
ct <- as.data.frame(summary(contrast(main$em, list("Neutralized - Default" = c(0, -1, 1, 0))),
                            infer = TRUE))
cat(sprintf("  Delta = %+.4f (%.2f%%), 95%% CI [%+.4f, %+.4f], p = %.3f\n",
            ct$estimate, ct$estimate * 100, ct$lower.CL, ct$upper.CL, ct$p.value))

# ── per-wave ─────────────────────────────────────────────────────────────────
waves <- sort(unique(as.character(d$wave)))
pd_wave <- do.call(rbind, lapply(waves, function(w) {
  dw <- droplevels(d[d$wave == w, ])
  mw <- lm(post_active_m2_ann ~ grp + pre_active_m2_ann, data = dw)
  ew <- emmeans(mw, ~ grp, vcov. = vcovHC(mw, type = "HC3"))
  cn <- contrast(ew, method = "trt.vs.ctrl", ref = "Default")
  cc <- as.data.frame(summary(cn, adjust = "none", infer = c(TRUE, TRUE)))
  nb <- table(dw$grp)
  cc$hedges_g <- sapply(seq_len(nrow(cc)), function(i) {
    g <- strsplit(gsub("[()]", "", as.character(cc$contrast[i])), " - ")[[1]]
    (cc$estimate[i] / sigma(mw)) * hedges_J(nb[[g[1]]], nb[[g[2]]]) })
  cc$wave <- w
  es <- as.data.frame(summary(ew)); es$wave <- w
  attr(es, "ct") <- cc; es }))
# ALL 6 pairwise per wave (not just vs Default), so the Neutralized-vs-Averse and
# Neutralized-vs-Seeking comparisons are available alongside the Default contrasts.
ct_wave <- do.call(rbind, lapply(waves, function(w) {
  dw <- droplevels(d[d$wave == w, ])
  mw <- lm(post_active_m2_ann ~ grp + pre_active_m2_ann, data = dw)
  ew <- emmeans(mw, ~ grp, vcov. = vcovHC(mw, type = "HC3"))
  cc <- as.data.frame(summary(pairs(ew, adjust = "none"), infer = c(TRUE, TRUE)))
  nb <- table(dw$grp)
  cc$hedges_g <- sapply(seq_len(nrow(cc)), function(i) {
    g <- strsplit(gsub("[()]", "", as.character(cc$contrast[i])), " - ")[[1]]
    (cc$estimate[i] / sigma(mw)) * hedges_J(nb[[g[1]]], nb[[g[2]]]) })
  cc$wave <- w; cc }))
ct_wave$p_fdr <- p.adjust(ct_wave$p.value, "fdr")
ct_wave[, c("estimate", "lower.CL", "upper.CL")] <-
  ct_wave[, c("estimate", "lower.CL", "upper.CL")] * 100
cat("\n=== per-wave pairwise contrasts (HC3; estimates in %; FDR across 18) ===\n")
print(ct_wave[, c("wave", "contrast", "estimate", "lower.CL", "upper.CL",
                  "hedges_g", "p.value", "p_fdr")], row.names = FALSE, digits = 3)

# ── ladder figures (plot_active_m2_treatment.R conventions) ──────────────────
YORDER <- c("Averse" = 4, "Default" = 3, "Neutralized" = 2, "Seeking" = 1)
grp_colors <- c("Averse" = "#1B7837", "Default" = "#BABABA",
                "Neutralized" = "#666666", "Seeking" = "#762A83")
lab_top2bottom <- names(sort(YORDER, decreasing = TRUE))

add_ci <- function(es) es %>%
  mutate(label = as.character(grp), y_position = YORDER[label],
         CI_90_lower = emmean - SE*qnorm(0.95),  CI_90_upper = emmean + SE*qnorm(0.95),
         CI_95_lower = emmean - SE*qnorm(0.975), CI_95_upper = emmean + SE*qnorm(0.975),
         CI_99_lower = emmean - SE*qnorm(0.995), CI_99_upper = emmean + SE*qnorm(0.995))

nature_theme <- theme_classic() +
  theme(
    text = element_text(family = "Avenir", size = 8),
    axis.title = element_text(family = "Avenir", size = 9),
    axis.text = element_text(family = "Avenir", size = 8, color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.ticks.length = unit(0.15, "cm"),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 10))

ladder <- function() list(
  geom_errorbarh(aes(xmin = CI_99_lower, xmax = CI_99_upper, color = label),
                 height = 0.25, linewidth = 1.2, alpha = 0.3),
  geom_errorbarh(aes(xmin = CI_95_lower, xmax = CI_95_upper, color = label),
                 height = 0.20, linewidth = 0.9, alpha = 0.5),
  geom_errorbarh(aes(xmin = CI_90_lower, xmax = CI_90_upper, color = label),
                 height = 0.15, linewidth = 0.7, alpha = 0.8),
  geom_point(aes(x = emmean, color = label), size = 3, alpha = 0.9),
  scale_color_manual(values = grp_colors, breaks = lab_top2bottom),
  scale_y_continuous(breaks = 4:1, labels = lab_top2bottom,
                     expand = expansion(add = c(0.5, 0.5))),
  labs(x = expression("Post-interaction Active M"^2), y = NULL))

# XEXP = proportional x padding (left, right) for later annotation
XEXP <- c(0.25, 1)
pd <- add_ci(main$es)
p <- ggplot(pd, aes(y = y_position)) + ladder() +
  scale_x_continuous(labels = label_number(accuracy = 0.001),
                     expand = expansion(mult = XEXP)) +
  guides(color = guide_legend(nrow = 1)) + nature_theme +
  theme(legend.position = "none",   # legend removed; restore with "bottom"
        axis.text.y = element_text(angle = 90, hjust = 0.5))
print(p)
ggsave(file.path(FIG_DIR, "neutral_arm_active_m2.png"), p,
       width = 5, height = 4.4, dpi = 500)

pdw <- add_ci(pd_wave)
p_w <- ggplot(pdw, aes(y = y_position)) + ladder() +
  facet_wrap(~ wave, nrow = 1,
             labeller = as_labeller(function(x) gsub("wave", "Wave ", x))) +
  scale_x_continuous(labels = label_number(accuracy = 0.01),
                     n.breaks = 4, expand = expansion(mult = XEXP)) +
  guides(color = guide_legend(nrow = 1)) + nature_theme +
  theme(legend.position = "none",   # legend removed; restore with "bottom"
        strip.background = element_blank(),
        strip.text = element_blank(),   # panel titles removed (label in manuscript);
                                        # restore: element_text(family="Avenir", size=8)
        panel.spacing = unit(1.2, "lines"),
        axis.text.y = element_text(angle = 90, hjust = 0.5))
print(p_w)
ggsave(file.path(FIG_DIR, "neutral_arm_active_m2_by_wave.png"), p_w,
       width = 6.5, height = 4.4, dpi = 500)

# ══ BULL-COUNTERFACTUAL OVERLAY (waves 2-3) ═══════════════════════════════════
# Same construction as active_m2_treatment_bull_overlay.R, extended to the
# 4-group split. Portfolios are FIXED at interaction time and re-scored on the
# BULL tercile of 15-row (calendar-day) windows from the pinned pre-study history
# (counterfactual_regime_m2.py); the model is the per-wave spec on the
# bucket-conditional outcome, post_m2_bull ~ grp + pre_m2_bull (HC3), fitted on
# all four groups within the wave so Default stays the WITHIN-REGIME benchmark.
#
# LIMITATION (asymmetric behavioural bounding): the counterfactual varies only
# the market draw, holding decisions at their realized (defensive-context)
# level. Users' context-sensitive resistance to seeking advice is therefore
# baked in, while whether AVERSE advice would be resisted symmetrically under
# genuine bull sentiment is unidentifiable here. Portfolio counterfactual, not
# behavioural counterfactual.
cf <- rd("counterfactual_regime_m2.csv") %>%
  select(participantId, pre_m2_bull, post_m2_bull)
db <- d %>% left_join(cf, by = "participantId")
BW <- c("wave2", "wave3")

emm_bull <- function(w) {
  dw <- db %>% filter(wave == w, !is.na(post_m2_bull), !is.na(pre_m2_bull)) %>% droplevels()
  mw <- lm(post_m2_bull ~ grp + pre_m2_bull, data = dw)
  ew <- emmeans(mw, ~ grp, vcov. = vcovHC(mw, type = "HC3"))
  cc <- as.data.frame(summary(pairs(ew, adjust = "none"), infer = c(TRUE, TRUE)))
  nb <- table(dw$grp)
  cc$hedges_g <- sapply(seq_len(nrow(cc)), function(i) {
    g <- strsplit(gsub("[()]", "", as.character(cc$contrast[i])), " - ")[[1]]
    (cc$estimate[i] / sigma(mw)) * hedges_J(nb[[g[1]]], nb[[g[2]]]) })
  cc$wave <- w
  es <- as.data.frame(summary(ew)); es$wave <- w
  list(es = es, ct = cc)
}
bull <- lapply(BW, emm_bull); names(bull) <- BW
ct_bull <- do.call(rbind, lapply(bull, `[[`, "ct"))
ct_bull$p_fdr <- p.adjust(ct_bull$p.value, "fdr")
ct_bull[, c("estimate", "lower.CL", "upper.CL")] <-
  ct_bull[, c("estimate", "lower.CL", "upper.CL")] * 100
cat("\n=== BULL counterfactual, per-wave pairwise (HC3; %; FDR across 12) ===\n")
print(ct_bull[, c("wave", "contrast", "estimate", "lower.CL", "upper.CL",
                  "hedges_g", "p.value", "p_fdr")], row.names = FALSE, digits = 3)

# PLOT only the non-averse rows: the averse counterfactual is the one most
# exposed to the asymmetric-bounding caveat (no portfolios were formed under
# bull sentiment, so its bull losses are likely overstated). Its contrasts are
# still printed above; set BULL_SHOW <- GRP to draw all four.
BULL_SHOW <- c("Default", "Neutralized", "Seeking")
pd_bull <- add_ci(do.call(rbind, lapply(bull, `[[`, "es"))) %>%
  filter(label %in% BULL_SHOW) %>%
  mutate(y_position = y_position - 0.30)      # ghost row just below the realized one

# realized = filled circle + solid whiskers; counterfactual = hollow diamond,
# half-transparent, whiskers kept solid so the (narrow) CIs stay legible
lad <- function(dat, a, a_w = a, marker = "circle") { sl <- (marker == "circle"); list(
  geom_errorbarh(data = dat, aes(xmin = CI_99_lower, xmax = CI_99_upper, color = label),
                 height = 0.22, linewidth = 1.2, alpha = 0.3 * a_w, show.legend = sl),
  geom_errorbarh(data = dat, aes(xmin = CI_95_lower, xmax = CI_95_upper, color = label),
                 height = 0.18, linewidth = 0.9, alpha = 0.5 * a_w, show.legend = sl),
  geom_errorbarh(data = dat, aes(xmin = CI_90_lower, xmax = CI_90_upper, color = label),
                 height = 0.13, linewidth = 0.7, alpha = 0.8 * a_w, show.legend = sl),
  if (sl) geom_point(data = dat, aes(x = emmean, color = label), size = 2.8, alpha = 0.9 * a)
  else    geom_point(data = dat, aes(x = emmean, color = label), shape = 23, fill = "white",
                     size = 1.7, stroke = 0.9, alpha = 0.95 * a, show.legend = FALSE)) }

p_b <- ggplot(mapping = aes(y = y_position)) +
  lad(pdw, a = 1) +
  lad(pd_bull, a = 0.65, a_w = 1, marker = "diamond") +
  facet_wrap(~ wave, nrow = 1,
             labeller = as_labeller(function(x) gsub("wave", "Wave ", x))) +
  scale_color_manual(values = grp_colors, breaks = lab_top2bottom) +
  scale_y_continuous(breaks = 4:1, labels = lab_top2bottom,
                     expand = expansion(add = c(0.5, 0.6))) +
  scale_x_continuous(labels = label_number(accuracy = 0.01), n.breaks = 4,
                     expand = expansion(mult = XEXP)) +
  labs(x = expression("Post-interaction Active M"^2), y = NULL) +
  guides(color = guide_legend(nrow = 1)) + nature_theme +
  theme(legend.position = "none",   # legend removed; restore with "bottom"
        strip.background = element_blank(),
        strip.text = element_blank(),   # panel titles removed (label in manuscript);
                                        # restore: element_text(family="Avenir", size=8)
        panel.spacing = unit(1., "lines"))
print(p_b)
ggsave(file.path(FIG_DIR, "neutral_arm_bull_overlay.png"), p_b,
       width = 4, height = 3, dpi = 500)
cat(sprintf("\nSaved neutral_arm_active_m2{,_by_wave}.png and neutral_arm_bull_overlay.png in %s\n",
            SCRIPT_DIR))
cat("Hollow diamonds (waves 2-3) = the same portfolios under the BULL\n")
cat("counterfactual; post-hoc/exploratory, portfolios fixed at interaction time.\n")

# ══ CROSS-PARTICIPANT PORTFOLIO DIVERSITY, 4 groups ═══════════════════════════
# portfolio_structure.R with the Risk-Neutral arm added as a fourth group.
# Measure: cross-participant dispersion = mean pairwise L1 distance among a
# group's row-normalized 25-asset portfolios; effect = POST - PRE (<0 = the
# group's portfolios converged on one another = homogenization).
# Inference: bootstrap CI on the change (B_BOOT); paired pre/post permutation
# vs ZERO and pairwise between-group permutation, both BH-FDR, at B_PERM.
# p = (count+1)/(B+1), so B_PERM sets the reportable floor (1e-4 here).
B_BOOT <- 1000; B_PERM <- 10000
set.seed(1)

pf <- rd("participant_portfolios.csv")
PREW <- paste0("pre_w_", ASSETS <- c(
  "SHY","IEF","LQD","GLD","VNQ","SPY","XLF","XLE","XLI","XLP","XLU","AAPL","MSFT",
  "AMZN","GOOGL","NVDA","TSLA","SHOP","SNOW","PLTR","DKNG","RIVN","CRSP","BTC","ETH"))
POSTW <- paste0("post_w_", ASSETS)
pf$grp <- with(pf, ifelse(ai_group %in% c("Extremely Risk-Averse","Somewhat Risk-Averse"), "Averse",
                   ifelse(ai_group %in% c("Extremely Risk-Seeking","Somewhat Risk-Seeking"), "Seeking",
                   ifelse(ai_group == "Default", "Default",
                   ifelse(ai_group == "Risk-Neutral", "Neutralized", NA_character_)))))
pf <- pf[!is.na(pf$grp), ]
norm_rows <- function(M) { M[is.na(M)] <- 0; M[M < 0] <- 0; rs <- rowSums(M)
                           M[rs <= 0, ] <- NA_real_; M / rs }
Wpre <- norm_rows(as.matrix(pf[, PREW])); Wpost <- norm_rows(as.matrix(pf[, POSTW]))
ok <- is.finite(rowSums(Wpre)) & is.finite(rowSums(Wpost))
pf <- pf[ok, ]; Wpre <- Wpre[ok, ]; Wpost <- Wpost[ok, ]
# precomputed distance matrices: subset dispersion is then a submatrix mean
# (EXACT: sum(D[i,i]) counts each pair twice + zero diagonal, so /(k(k-1)))
DPRE <- as.matrix(dist(Wpre, "manhattan")); DPOST <- as.matrix(dist(Wpost, "manhattan"))
dsp  <- function(D, i) { k <- length(i); if (k < 2) return(NA_real_); sum(D[i, i]) / (k*(k-1)) }
del  <- function(i) dsp(DPOST, i) - dsp(DPRE, i)
gi <- lapply(GRP, function(g) which(pf$grp == g)); names(gi) <- GRP

rz <- data.frame(grp = GRP, n = sapply(gi, length),
                 pre_disp = sapply(gi, function(i) dsp(DPRE, i)),
                 post_disp = sapply(gi, function(i) dsp(DPOST, i)),
                 d_change = sapply(gi, del))
bci <- sapply(gi, function(i) quantile(replicate(B_BOOT, {
  s <- sample(i, length(i), TRUE); del(s) }), c(.025, .975), na.rm = TRUE))
rz$lo <- bci[1, ]; rz$hi <- bci[2, ]

perm0 <- function(g) { i <- gi[[g]]; n <- length(i); obs <- del(i); cnt <- 0
  for (b in seq_len(B_PERM)) { sw <- runif(n) < 0.5
    Pp <- Wpost[i, , drop = FALSE]; Pr <- Wpre[i, , drop = FALSE]
    t2 <- Pp[sw, , drop = FALSE]; Pp[sw, ] <- Pr[sw, ]; Pr[sw, ] <- t2
    dl <- mean(dist(Pp, "manhattan")) - mean(dist(Pr, "manhattan"))
    if (!is.na(dl) && abs(dl) >= abs(obs)) cnt <- cnt + 1 }
  (cnt + 1) / (B_PERM + 1) }
rz$p_vs_zero <- sapply(GRP, perm0); rz$p_vs_zero_fdr <- p.adjust(rz$p_vs_zero, "fdr")
cat("\n=== Cross-participant dispersion by group (d_change = POST - PRE) ===\n")
print(rz, row.names = FALSE, digits = 3)

perm_pair <- function(g1, g2) { obs <- del(gi[[g1]]) - del(gi[[g2]])
  ab <- c(gi[[g1]], gi[[g2]]); labs <- pf$grp[ab]; cnt <- 0
  for (b in seq_len(B_PERM)) { pl <- sample(labs)
    dl <- del(ab[pl == g1]) - del(ab[pl == g2])
    if (!is.na(dl) && abs(dl) >= abs(obs)) cnt <- cnt + 1 }
  c(diff = obs, p = (cnt + 1) / (B_PERM + 1)) }
PZ <- combn(GRP, 2, simplify = FALSE)
pwz <- do.call(rbind, lapply(PZ, function(q) { v <- perm_pair(q[1], q[2])
  data.frame(comparison = paste(q[1], "vs", q[2]),
             diff_d_change = unname(v["diff"]), perm_p = unname(v["p"])) }))
pwz$p_fdr <- p.adjust(pwz$perm_p, "fdr")
cat("\n=== pairwise between-group d_change (permutation; BH-FDR across 6) ===\n")
print(pwz, row.names = FALSE, digits = 3)

# ── bar figure (portfolio_structure.R style, Neutralized added) ──────────────────
# Palette follows the 3-group figure (Tableau greens/mauve) with the two
# unbiased arms as light (Default) and dark (Neutralized) grey, matching the ladders.
# HORIZONTAL bars: groups on y (first level at the TOP, hence rev()), the
# measure on x. Value labels sit just beyond the far end of each interval.
rz$grp <- factor(rz$grp, levels = rev(GRP))
zcol <- c("Averse" = "#59A14F", "Default" = "#BAB0AC",
          "Neutralized" = "#79706E", "Seeking" = "#B07AA1")
offz <- 0.06 * diff(range(c(rz$lo, rz$hi, 0), na.rm = TRUE))
rz$lab_x  <- ifelse(rz$d_change >= 0, rz$hi + offz, rz$lo - offz)
rz$lab_hj <- ifelse(rz$d_change >= 0, 0, 1)

pz <- ggplot(rz, aes(y = grp, x = d_change, fill = grp)) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.4) +
  geom_col(alpha = 0.85, width = 0.6) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.15, linewidth = 0.7, alpha = 0.9) +
  geom_point(color = "black", size = 3, shape = 18) +
  geom_text(aes(x = lab_x, label = sprintf("%+.3f", d_change), hjust = lab_hj),
            size = 4, family = "Avenir") +
  scale_fill_manual(values = zcol) +
  scale_x_continuous(labels = label_number(accuracy = 0.01)) +
  labs(y = NULL, x = "Change in Cross-participant\n Portfolio Diversity") +
  nature_theme +
  theme(legend.position = "none",
        text = element_text(family = "Avenir", size = 12),
        axis.text = element_text(family = "Avenir", size = 12, color = "black"),
        axis.text.y = element_text(family = "Avenir", size = 12, color = "black"),
        axis.title.x = element_text(family = "Avenir", size = 12, margin = margin(t = 6))) +
  xlim(-0.7, 0.03)          # room left of the bars for significance brackets
print(pz)
ggsave(file.path(FIG_DIR, "neutral_arm_diversity.png"), pz,
       width = 3.3, height = 4., dpi = 500)
cat(sprintf("\nSaved neutral_arm_diversity.png in %s\n", SCRIPT_DIR))

# ══ FOLLOW-UP PARTICIPATION, 4 groups ═════════════════════════════════════════
# Extensive margin of engagement: P(participant sent >= 1 follow-up), the
# measure plotted in panel 2 of bias_magnitude_outcomes.R. Linear probability
# model + HC3 so the ladder's symmetric CI construction matches the performance
# panel; a logit cross-check is printed alongside.
dfu <- d %>%
  left_join(rd("reply_turns.csv") %>% filter(experiment == "single") %>%
              select(participantId, n_reply_turns), by = "participantId") %>%
  filter(!is.na(n_reply_turns)) %>%
  mutate(anyfu = as.integer(n_reply_turns > 0))
cat(sprintf("\n=== Follow-up participation, 4 groups (n = %d) ===\nraw rates: %s\n",
            nrow(dfu), paste(names(tapply(dfu$anyfu, dfu$grp, mean)),
              sprintf("%.1f%%", 100 * tapply(dfu$anyfu, dfu$grp, mean)), collapse = " | ")))
mfu <- lm(anyfu ~ grp + wave + pre_active_m2_ann, data = dfu)
Vfu <- vcovHC(mfu, type = "HC3"); emfu <- emmeans(mfu, ~ grp, vcov. = Vfu)
esfu <- as.data.frame(summary(emfu))
print(esfu[, c("grp", "emmean", "SE")], row.names = FALSE, digits = 3)
pwfu <- as.data.frame(summary(pairs(emfu, adjust = "fdr"), infer = TRUE))
nbf <- table(dfu$grp)
pwfu$hedges_g <- mapply(function(est, ct) {
  g <- strsplit(as.character(ct), " - ")[[1]]
  (est / sigma(mfu)) * hedges_J(nbf[[g[1]]], nbf[[g[2]]]) }, pwfu$estimate, pwfu$contrast)
cat("--- pairwise (LPM, HC3; FDR across 6) ---\n")
print(pwfu[, c("contrast", "estimate", "lower.CL", "upper.CL", "hedges_g", "p.value")],
      row.names = FALSE, digits = 3)
gfu <- glm(anyfu ~ grp + wave + pre_active_m2_ann, family = binomial(), data = dfu)
ccf <- coeftest(gfu, vcov = vcovHC(gfu, type = "HC3")); rf <- grep("^grp", rownames(ccf))
cat("--- logit cross-check (OR vs Averse = reference, HC3) ---\n")
print(data.frame(term = sub("^grp", "", rownames(ccf)[rf]), OR = exp(ccf[rf, 1]),
                 lo = exp(ccf[rf, 1] - 1.96 * ccf[rf, 2]),
                 hi = exp(ccf[rf, 1] + 1.96 * ccf[rf, 2]), p = ccf[rf, 4]),
      row.names = FALSE, digits = 3)

pdfu <- add_ci(esfu)
p_fu <- ggplot(pdfu, aes(y = y_position)) + ladder() +
  scale_x_continuous(labels = label_number(accuracy = 0.01),
                     expand = expansion(mult = XEXP)) +
  labs(x = "Follow-up Participation Rate") +
  guides(color = guide_legend(nrow = 1)) + nature_theme +
  theme(legend.position = "none",
        axis.text.y = element_text(family = "Avenir", size = 7.6, color = "black"))
print(p_fu)
ggsave(file.path(FIG_DIR, "neutral_arm_participation.png"), p_fu,
       width = 2.3, height = 2.8, dpi = 500)
cat(sprintf("\nSaved neutral_arm_participation.png in %s\n", SCRIPT_DIR))

# ══ AI-ADVICE QUALITY, 4 groups (and controlling for it) ══════════════════════
# Each assistant's OWN recommended portfolio (GPT-extracted, ai_only_by_wave.csv)
# scored on the SAME participant window as the realized outcome. Two questions:
#   (a) how good was the Neutralized assistant's advice relative to the others?
#   (b) do the arm effects survive conditioning on advice quality? If they
#       vanish, the arm effect operated THROUGH what was recommended rather than
#       through anything the participant did with it.
# CAVEAT: ai_m2 is measured post-treatment and is a consequence of the arm, so
# conditioning on it is a mediation-style decomposition, not a causal control;
# read it as "how much of the arm effect is carried by advice content".
ret <- rd("daily_returns.csv"); ret$Date <- as.Date(ret$Date)
aio <- rd("ai_only_by_wave.csv"); aio$eval_start <- as.Date(aio$eval_start)
AWC <- paste0("ai_w_", ASSETS)
active_m2 <- function(w, R) {
  s <- sum(w, na.rm = TRUE); if (!is.finite(s) || s <= 0) return(NA_real_)
  w <- ifelse(is.na(w), 0, w) / s
  rp <- as.numeric(R %*% w); rb <- R[, "SPY"]; a <- rp - rb
  sa <- sd(a); sm <- sd(rb)
  if (is.na(sa) || sa == 0 || sm == 0) return(NA_real_)
  (mean(a) / sa) * sm * sqrt(252) }
aio$ai_m2 <- NA_real_
for (i in seq_len(nrow(aio))) {
  Ri <- as.matrix(ret[ret$Date >= aio$eval_start[i] &
                      ret$Date <= (aio$eval_start[i] + 14), ASSETS, drop = FALSE])
  if (nrow(Ri) >= 3) aio$ai_m2[i] <- active_m2(as.numeric(aio[i, AWC]), Ri) }
da <- d %>% left_join(aio %>% select(participantId, ai_m2), by = "participantId") %>%
  filter(!is.na(ai_m2))
cat(sprintf("\n=== AI-ADVICE quality by group (n = %d) ===\nraw mean ai_m2 (%%): %s\n",
            nrow(da), paste(names(tapply(da$ai_m2, da$grp, mean)),
              sprintf("%.2f", 100 * tapply(da$ai_m2, da$grp, mean)), collapse = " | ")))
ma <- lm(ai_m2 ~ grp + wave + pre_active_m2_ann, data = da)
ema <- emmeans(ma, ~ grp, vcov. = vcovHC(ma, type = "HC3"))
esa <- as.data.frame(summary(ema)); esa$emmean_pct <- esa$emmean * 100
print(esa[, c("grp", "emmean", "SE", "emmean_pct")], row.names = FALSE, digits = 4)
pwa <- as.data.frame(summary(pairs(ema, adjust = "fdr"), infer = TRUE))
nba <- table(da$grp)
pwa$hedges_g <- mapply(function(est, ct) {
  g <- strsplit(as.character(ct), " - ")[[1]]
  (est / sigma(ma)) * hedges_J(nba[[g[1]]], nba[[g[2]]]) }, pwa$estimate, pwa$contrast)
pwa[, c("estimate", "lower.CL", "upper.CL")] <- pwa[, c("estimate", "lower.CL", "upper.CL")] * 100
cat("--- pairwise advice quality (FDR across 6; % units) ---\n")
print(pwa[, c("contrast", "estimate", "lower.CL", "upper.CL", "hedges_g", "p.value")],
      row.names = FALSE, digits = 3)

cat("\n=== ARM EFFECTS on participant performance, with vs without ai_m2 ===\n")
for (nm in c("without ai_m2" = "grp + wave + pre_active_m2_ann",
             "with ai_m2"    = "grp + wave + pre_active_m2_ann + ai_m2")) {
  mz <- lm(as.formula(paste("post_active_m2_ann ~", nm)), data = da)
  ez <- emmeans(mz, ~ grp, vcov. = vcovHC(mz, type = "HC3"))
  cz <- as.data.frame(summary(contrast(ez, "trt.vs.ctrl", ref = "Default"),
                              adjust = "fdr", infer = TRUE))
  cz[, c("estimate", "lower.CL", "upper.CL")] <- cz[, c("estimate", "lower.CL", "upper.CL")] * 100
  cat(sprintf("--- %s (vs Default; %% units; FDR across 3) ---\n",
              names(which(c("grp + wave + pre_active_m2_ann",
                            "grp + wave + pre_active_m2_ann + ai_m2") == nm))))
  print(cz[, c("contrast", "estimate", "lower.CL", "upper.CL", "p.value")],
        row.names = FALSE, digits = 3) }

# ══ POST-INTERACTION CONFIDENCE, 4 groups ═════════════════════════════════════
# ANCOVA: the outcome is POST-interaction confidence with PRE-confidence as a
# covariate (post ~ grp + wave + pre_active_m2_ann + pre_conf), the registered
# structure used in bias_magnitude_outcomes.R -- not a change score.
# Primary = ordinal MCMC on the latent scale (residual variance FIXED; family=
# "ordinal" does not identify it, and leaving it free makes estimates drift
# across runs). OLS is printed as an interpretable cross-check and is what the
# ladder plots.
suppressPackageStartupMessages(library(MCMCglmm))
dc <- d %>%
  left_join(rd("perceived_improvement.csv") %>% filter(experiment == "single") %>%
              select(participantId, pre_conf, post_conf), by = "participantId") %>%
  filter(!is.na(post_conf), !is.na(pre_conf))
cat(sprintf("\n=== Post-interaction confidence, 4 groups (n = %d) ===\nraw mean post_conf: %s\n",
            nrow(dc), paste(names(tapply(dc$post_conf, dc$grp, mean)),
              sprintf("%.3f", tapply(dc$post_conf, dc$grp, mean)), collapse = " | ")))

# --- OLS ANCOVA (HC3): interpretable units, plotted ---
mcf <- lm(post_conf ~ grp + wave + pre_active_m2_ann + pre_conf, data = dc)
emcf <- emmeans(mcf, ~ grp, vcov. = vcovHC(mcf, type = "HC3"))
escf <- as.data.frame(summary(emcf))
print(escf[, c("grp", "emmean", "SE")], row.names = FALSE, digits = 3)
pwcf <- as.data.frame(summary(pairs(emcf, adjust = "fdr"), infer = TRUE))
nbc <- table(dc$grp)
pwcf$hedges_g <- mapply(function(est, ct) {
  g <- strsplit(as.character(ct), " - ")[[1]]
  (est / sigma(mcf)) * hedges_J(nbc[[g[1]]], nbc[[g[2]]]) }, pwcf$estimate, pwcf$contrast)
cat("--- pairwise, OLS ANCOVA (FDR across 6) ---\n")
print(pwcf[, c("contrast", "estimate", "lower.CL", "upper.CL", "hedges_g", "p.value")],
      row.names = FALSE, digits = 3)

# --- ordinal MCMC latent scale (primary) ---
dc$yo <- factor(dc$post_conf, ordered = TRUE)
set.seed(123)
mmc <- MCMCglmm(yo ~ grp + wave + pre_active_m2_ann + pre_conf, family = "ordinal",
                nitt = 55000, thin = 25, burnin = 5000,
                prior = list(R = list(V = 1, fix = 1)),
                data = as.data.frame(dc), verbose = FALSE)
Pc <- as.matrix(mmc$Sol); Sc <- sd(dc$post_conf)
gc4 <- function(nm) if (nm == GRP[1]) rep(0, nrow(Pc)) else Pc[, paste0("grp", nm)]
cmb <- combn(GRP, 2, simplify = FALSE)
resc <- do.call(rbind, lapply(cmb, function(q) {
  ct <- gc4(q[1]) - gc4(q[2])
  data.frame(contrast = paste(q[1], "-", q[2]), estimate = mean(ct),
             lo = quantile(ct, .025), hi = quantile(ct, .975),
             hedges_g = mean(ct) / Sc * hedges_J(nbc[[q[1]]], nbc[[q[2]]]),
             p = min(1, 2 * min(mean(ct < 0), mean(ct > 0)))) }))
resc$p_fdr <- p.adjust(resc$p, "fdr")
cat("--- pairwise, ordinal MCMC latent (FDR across 6) ---\n")
print(resc, row.names = FALSE, digits = 3)

pdcf <- add_ci(escf)
p_cf <- ggplot(pdcf, aes(y = y_position)) + ladder() +
  scale_x_continuous(labels = label_number(accuracy = 0.01),
                     expand = expansion(mult = XEXP)) +
  labs(x = "Post-interaction Confidence") +
  guides(color = guide_legend(nrow = 1)) + nature_theme +
  theme(legend.position = "none")
print(p_cf)
ggsave(file.path(FIG_DIR, "neutral_arm_confidence.png"), p_cf,
       width = 2.3, height = 2.8, dpi = 500)
cat(sprintf("\nSaved neutral_arm_confidence.png in %s\n", SCRIPT_DIR))

# ══ PERCEIVED IMPROVEMENT (survey item), 4 groups ═════════════════════════════
# The direct self-report item, kept as a SECONDARY measure: the paper's
# "perceived improvement" axis is operationalized as the confidence delta above
# (SI §X). Same estimators as the confidence section.
dpi4 <- d %>%
  left_join(rd("perceived_improvement.csv") %>% filter(experiment == "single") %>%
              select(participantId, perceived_improve), by = "participantId") %>%
  filter(!is.na(perceived_improve))
cat(sprintf("\n=== Perceived improvement item, 4 groups (n = %d) ===\nraw mean: %s\n",
            nrow(dpi4), paste(names(tapply(dpi4$perceived_improve, dpi4$grp, mean)),
              sprintf("%.3f", tapply(dpi4$perceived_improve, dpi4$grp, mean)), collapse = " | ")))
mpi <- lm(perceived_improve ~ grp + wave + pre_active_m2_ann, data = dpi4)
empi <- emmeans(mpi, ~ grp, vcov. = vcovHC(mpi, type = "HC3"))
print(as.data.frame(summary(empi))[, c("grp", "emmean", "SE")], row.names = FALSE, digits = 3)
pwpi <- as.data.frame(summary(pairs(empi, adjust = "fdr"), infer = TRUE))
nbp <- table(dpi4$grp)
pwpi$hedges_g <- mapply(function(est, ct) {
  g <- strsplit(as.character(ct), " - ")[[1]]
  (est / sigma(mpi)) * hedges_J(nbp[[g[1]]], nbp[[g[2]]]) }, pwpi$estimate, pwpi$contrast)
cat("--- pairwise, OLS HC3 (FDR across 6) ---\n")
print(pwpi[, c("contrast", "estimate", "lower.CL", "upper.CL", "hedges_g", "p.value")],
      row.names = FALSE, digits = 3)

dpi4$yo <- factor(dpi4$perceived_improve, ordered = TRUE)
set.seed(123)
mmp <- MCMCglmm(yo ~ grp + wave + pre_active_m2_ann, family = "ordinal",
                nitt = 55000, thin = 25, burnin = 5000,
                prior = list(R = list(V = 1, fix = 1)),
                data = as.data.frame(dpi4), verbose = FALSE)
Pp4 <- as.matrix(mmp$Sol); Sp4 <- sd(dpi4$perceived_improve)
gp4 <- function(nm) if (nm == GRP[1]) rep(0, nrow(Pp4)) else Pp4[, paste0("grp", nm)]
resp <- do.call(rbind, lapply(combn(GRP, 2, simplify = FALSE), function(q) {
  ct <- gp4(q[1]) - gp4(q[2])
  data.frame(contrast = paste(q[1], "-", q[2]), estimate = mean(ct),
             lo = quantile(ct, .025), hi = quantile(ct, .975),
             hedges_g = mean(ct) / Sp4 * hedges_J(nbp[[q[1]]], nbp[[q[2]]]),
             p = min(1, 2 * min(mean(ct < 0), mean(ct > 0)))) }))
resp$p_fdr <- p.adjust(resp$p, "fdr")
cat("--- pairwise, ordinal MCMC latent (FDR across 6) ---\n")
print(resp, row.names = FALSE, digits = 3)
