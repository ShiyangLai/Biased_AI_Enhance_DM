# ==============================================================================
# diversity_single_vs_dual.R
# CROSS-PARTICIPANT portfolio diversity (homogenization) across the 5
# experimental conditions (single + dual). Same measure as portfolio_structure.R,
# extended from the 3 single-AI bias groups to the full single-vs-dual design.
#
# Conditions: Single AI Default / Single AI Biased (Som+Ext Averse+Seeking) /
#   Dual AI Default / Dual AI Opposition / Dual AI Balanced.
#
# Measure: cross-participant dispersion = mean pairwise L1 distance among a
#   condition's row-normalized 25-asset portfolios. Treatment effect = POST - PRE
#   (>0 diversified; <0 homogenized/converged).
# Inference: bootstrap CI on the change; paired pre/post permutation vs ZERO
#   (BH-FDR); pairwise between-condition permutation (BH-FDR). p = (count+1)/(B+1).
#
# Covariate robustness: since dispersion is a GROUP-level statistic (no per-row
#   regression), covariates are handled by REWEIGHTING participants (inverse-
#   propensity / balancing weights) so every condition shares a common covariate
#   distribution, then computing the SAME dispersion measure on the (unchanged)
#   weights. Weights = p(G=g)/p(G=g|X) from a multinomial propensity on the
#   imbalanced covariates (min set: 5-condition omnibus ANOVA p<.05 or eta^2>=.01),
#   stabilized + winsorized; this makes each group's weighted covariate means
#   equal the pooled means. Reported as robustness; the raw analysis is primary.
#   (Arms are randomized within each experiment, so this mainly affects the
#   cross-experiment contrasts.)
#
# CAVEAT: single-AI vs dual-AI conditions are separate samples/experiments; the
#   clean comparisons are within-experiment. 9 participants appear in both, but
#   each condition draws from exactly one experiment, so no de-dup is needed here.
# Inputs: participant_portfolios.csv , dual_portfolios.csv ,
#         participant_covariates.csv, dual_covariates.csv
#   setwd("investment/code"); source("_setup.R"); source("diversity_single_vs_dual.R")
# ==============================================================================
suppressPackageStartupMessages({ library(ggplot2); library(dplyr); library(scales) })

.args <- commandArgs(FALSE); .f <- sub("^--file=", "", .args[grep("^--file=", .args)])
SCRIPT_DIR <- if (length(.f)) dirname(normalizePath(.f)) else getwd()
rd <- function(f) { p <- file.path(DATA_DIR, f); if (!file.exists(p)) p <- file.path(DATA_DIR, f)
                    read.csv(p, check.names = FALSE, stringsAsFactors = FALSE) }

ASSETS <- c("SHY","IEF","LQD","GLD","VNQ","SPY","XLF","XLE","XLI","XLP","XLU",
            "AAPL","MSFT","AMZN","GOOGL","NVDA","TSLA","SHOP","SNOW","PLTR","DKNG",
            "RIVN","CRSP","BTC","ETH")
PREW <- paste0("pre_w_", ASSETS); POSTW <- paste0("post_w_", ASSETS)

sp <- rd("participant_portfolios.csv"); stopifnot(all(c(PREW, POSTW) %in% names(sp)))
dp <- rd("dual_portfolios.csv");        stopifnot(all(c(PREW, POSTW) %in% names(dp)))
scov <- rd("participant_covariates.csv"); dcov <- rd("dual_covariates.csv")
COVARS <- c("Age_enc", "Sex_enc", "work_in_finance_enc", "invest_own_score",
            "first_invest_time_enc", "trade_freq_12m_enc", "news_follow_freq_enc",
            "risk_pref_score", "time_preference_switch", "fin_lit_score",
            "fin_lit_selfassess_1_enc", "fin_overconfidence_1_enc",
            "portfolio_confidence_1_enc", "trust_ai_score",
            "market_outlook_2wk_enc", "market_volatility_2w_enc")

# ── assemble the 5 conditions (merge covariates per experiment) ───────────────
sp$cond <- with(sp, ifelse(ai_group == "Default", "Single AI Default",
                    ifelse(ai_group %in% c("Extremely Risk-Averse","Somewhat Risk-Averse",
                                           "Extremely Risk-Seeking","Somewhat Risk-Seeking"),
                           "Single AI Biased", NA_character_)))
dp$cond <- with(dp, ifelse(dual_condition == "dual_nonbiased",  "Dual AI Default",
                    ifelse(dual_condition == "dual_opposition", "Dual AI Opposition",
                    ifelse(dual_condition == "dual_balanced",   "Dual AI Balanced", NA_character_))))
sp <- merge(sp, scov, by = "participantId", all.x = TRUE)
dp <- merge(dp, dcov, by = "participantId", all.x = TRUE)
keep <- c("cond", PREW, POSTW, COVARS)
d <- bind_rows(sp[, keep], dp[, keep])
d <- d[!is.na(d$cond), ]

GROUPS <- c("Single AI Default", "Single AI Biased",
            "Dual AI Default", "Dual AI Opposition", "Dual AI Balanced")

norm_rows <- function(M) { M[is.na(M)] <- 0; M[M < 0] <- 0; rs <- rowSums(M); M[rs <= 0, ] <- NA_real_; M / rs }
Wpre  <- norm_rows(as.matrix(d[, PREW]))
Wpost <- norm_rows(as.matrix(d[, POSTW]))
ok <- is.finite(rowSums(Wpre)) & is.finite(rowSums(Wpost))
d <- d[ok, ]; Wpre <- Wpre[ok, ]; Wpost <- Wpost[ok, ]

disp  <- function(M, idx) if (length(idx) < 2) NA_real_ else mean(dist(M[idx, , drop = FALSE], method = "manhattan"))
delta <- function(idx) disp(Wpost, idx) - disp(Wpre, idx)
gidx <- lapply(GROUPS, function(g) which(d$cond == g)); names(gidx) <- GROUPS

# Precomputed full pairwise-distance matrices: dispersion of any subset is then a
# submatrix mean rather than a fresh dist() call. EXACT (not an approximation):
# sum(D[idx, idx]) counts every pair twice and the zero diagonal once, so
# sum/(k(k-1)) == mean(dist(.)). Used inside the permutation loops, which is what
# makes B_PERM = 10,000 feasible.
DPRE  <- as.matrix(dist(Wpre,  method = "manhattan"))
DPOST <- as.matrix(dist(Wpost, method = "manhattan"))
disp_fast  <- function(D, idx) { k <- length(idx); if (k < 2) return(NA_real_)
                                 sum(D[idx, idx]) / (k * (k - 1)) }
delta_fast <- function(idx) disp_fast(DPOST, idx) - disp_fast(DPRE, idx)

# ── point estimates ──────────────────────────────────────────────────────────
res <- data.frame(cond = GROUPS,
  n         = sapply(gidx, length),
  pre_disp  = sapply(gidx, function(ix) disp(Wpre, ix)),
  post_disp = sapply(gidx, function(ix) disp(Wpost, ix)),
  d_change  = sapply(gidx, delta))

# ── bootstrap CI on the change (resample participants within condition) ──────
# Replicate counts. Permutation p = (count+1)/(B+1) is FLOORED at 1/(B+1), so
# B_PERM fixes the reportable resolution of the PRIMARY tests: at B = 1,000 the
# smallest attainable p is 0.001 and the smallest BH-adjusted value 0.0017, i.e.
# "p < 0.001" is unclaimable; B = 10,000 lifts that floor to 1e-4.
# The IPW robustness section keeps 1,000 (runtime; it needs no <0.001 resolution).
set.seed(1)
B_BOOT <- 1000     # bootstrap CIs
B_PERM <- 10000    # primary permutation tests (plotted / reported)
B_ROB  <- 1000     # covariate-balanced robustness permutations
boot_ci <- function(ix) {
  bs <- replicate(B_BOOT, { s <- sample(ix, length(ix), replace = TRUE)
    mean(dist(Wpost[s, , drop = FALSE], "manhattan")) - mean(dist(Wpre[s, , drop = FALSE], "manhattan")) })
  quantile(bs, c(.025, .975), na.rm = TRUE)
}
ci <- sapply(gidx, boot_ci); res$lo <- ci[1, ]; res$hi <- ci[2, ]

# ── change vs ZERO: paired pre/post permutation (BH-FDR across 5) ────────────
perm_p0 <- function(g) {
  ix <- gidx[[g]]; n <- length(ix); if (n < 2) return(NA_real_)
  obs <- delta(ix); cnt <- 0
  for (b in seq_len(B_PERM)) {
    sw <- runif(n) < 0.5
    Pp <- Wpost[ix, , drop = FALSE]; Pr <- Wpre[ix, , drop = FALSE]
    t2 <- Pp[sw, , drop = FALSE]; Pp[sw, ] <- Pr[sw, ]; Pr[sw, ] <- t2
    dl <- mean(dist(Pp, "manhattan")) - mean(dist(Pr, "manhattan"))
    if (!is.na(dl) && abs(dl) >= abs(obs)) cnt <- cnt + 1
  }
  (cnt + 1) / (B_PERM + 1)
}
res$perm_p_vs_zero <- sapply(GROUPS, perm_p0)
res$p_vs_zero_fdr  <- p.adjust(res$perm_p_vs_zero, "fdr")

cat("=== Cross-participant dispersion (mean pairwise L1) by condition ===\n")
cat("   d_change = POST - PRE (>0 diversified, <0 homogenized); perm p vs 0 (+FDR/5)\n")
print(res, row.names = FALSE, digits = 3)

# ── pairwise between-condition permutation tests (BH-FDR across 10) ──────────
perm_pair <- function(g1, g2) {
  obs <- delta(gidx[[g1]]) - delta(gidx[[g2]])
  ab <- c(gidx[[g1]], gidx[[g2]]); labs <- d$cond[ab]; cnt <- 0
  for (b in seq_len(B_PERM)) {
    pl <- sample(labs); dl <- delta_fast(ab[pl == g1]) - delta_fast(ab[pl == g2])
    if (!is.na(dl) && abs(dl) >= abs(obs)) cnt <- cnt + 1
  }
  c(diff = obs, p = (cnt + 1) / (B_PERM + 1))
}
PAIRS <- combn(GROUPS, 2, simplify = FALSE)
pw <- do.call(rbind, lapply(PAIRS, function(pr) {
  v <- perm_pair(pr[1], pr[2])
  data.frame(comparison = paste(pr[1], "vs", pr[2]),
             diff_d_change = unname(v["diff"]), perm_p = unname(v["p"]))
}))
pw$p_fdr <- p.adjust(pw$perm_p, "fdr")
cat("\n=== pairwise between-condition d_change (permutation; BH-FDR across 10) ===\n")
print(pw, row.names = FALSE, digits = 3)
cat("NOTE: single-AI vs dual-AI comparisons are cross-experiment (separate samples).\n")

# ══ COVARIATE ROBUSTNESS: dispersion on covariate-residualized weights ════════
# 5-condition imbalance check (omnibus: ANOVA p<.05 or eta^2>=.01; "minimum"
# set, same criterion as active_m2_single_vs_dual.R). Flagged covariates ->
# residualize each asset weight (pre & post) on them, recompute d_change + tests.
for (cv in COVARS) { d[[cv]] <- as.numeric(d[[cv]]); d[[cv]][is.na(d[[cv]])] <- median(d[[cv]], na.rm = TRUE) }
condf <- factor(d$cond, levels = GROUPS)
bal <- do.call(rbind, lapply(COVARS, function(cv) {
  av <- anova(lm(d[[cv]] ~ condf))
  data.frame(covariate = cv, eta2 = av$`Sum Sq`[1] / sum(av$`Sum Sq`), anova_p = av$`Pr(>F)`[1])
}))
bal$flag <- ifelse(bal$anova_p < .05 | bal$eta2 >= .01, "IMBALANCED", "")
cat("\n=== 5-condition imbalance check (omnibus: flag if ANOVA p<.05 or eta^2>=.01) ===\n")
print(bal, row.names = FALSE, digits = 3)
COVF <- as.character(bal$covariate[bal$flag == "IMBALANCED"])
cat(sprintf("Balancing on %d imbalanced covariates: %s\n",
            length(COVF), paste(COVF, collapse = ", ")))

# ── stabilized IPW / balancing weights to the pooled covariate distribution ──
# v_i = p(G=g_i) / p(G=g_i | X_i); makes each group's weighted covariate means
# equal the pooled means. Winsorized at [1st, 99th] pct for stability.
suppressPackageStartupMessages(library(nnet))
condf <- factor(d$cond, levels = GROUPS)
pm  <- multinom(condf ~ ., data = data.frame(condf, d[, COVF, drop = FALSE]),
                trace = FALSE, maxit = 500)
P   <- predict(pm, type = "probs")                    # n x 5
p_gx <- P[cbind(seq_len(nrow(P)), as.integer(condf))] # own-group propensity
p_g  <- as.numeric(prop.table(table(condf))[as.integer(condf)])
v <- p_g / p_gx
v <- pmin(pmax(v, quantile(v, .01)), quantile(v, .99))
v <- v / mean(v)
cat(sprintf("balancing weights: range [%.2f, %.2f], mean 1.00\n", min(v), max(v)))
cat("weighted covariate means by group (should be ~equal across rows):\n")
print(sapply(COVF, function(cv) tapply(d[[cv]] * v, condf, sum) / tapply(v, condf, sum)),
      digits = 3)

# weighted dispersion: sum_{i<j} v_i v_j d_ij / sum_{i<j} v_i v_j
disp_w <- function(M, idx, v) {
  if (length(idx) < 2) return(NA_real_)
  D <- as.matrix(dist(M[idx, , drop = FALSE], "manhattan")); vv <- outer(v[idx], v[idx])
  ut <- upper.tri(D); sum(D[ut] * vv[ut]) / sum(vv[ut])
}
deltaW <- function(idx) disp_w(Wpost, idx, v) - disp_w(Wpre, idx, v)
resW <- data.frame(cond = GROUPS, d_change_wt = sapply(gidx, deltaW))

# vs-zero: weighted paired pre/post permutation (weights are person-level -> swap-invariant)
perm0W <- function(g) { ix <- gidx[[g]]; n <- length(ix); if (n < 2) return(NA_real_)
  obs <- deltaW(ix); cnt <- 0
  for (b in seq_len(B_ROB)) { sw <- runif(n) < 0.5
    Pp <- Wpost[ix, , drop = FALSE]; Pr <- Wpre[ix, , drop = FALSE]
    t2 <- Pp[sw, , drop = FALSE]; Pp[sw, ] <- Pr[sw, ]; Pr[sw, ] <- t2
    vg <- v[ix]; vv <- outer(vg, vg); ut <- upper.tri(vv)
    Dp <- as.matrix(dist(Pp, "manhattan")); Dr <- as.matrix(dist(Pr, "manhattan"))
    dl <- sum(Dp[ut]*vv[ut])/sum(vv[ut]) - sum(Dr[ut]*vv[ut])/sum(vv[ut])
    if (!is.na(dl) && abs(dl) >= abs(obs)) cnt <- cnt + 1 }
  (cnt + 1) / (B_ROB + 1) }
resW$perm_p_vs_zero <- sapply(GROUPS, perm0W)
resW$p_vs_zero_fdr  <- p.adjust(resW$perm_p_vs_zero, "fdr")
cat("\n=== covariate-BALANCED d_change vs 0 (IPW-weighted dispersion) ===\n")
print(resW, row.names = FALSE, digits = 3)

# pairwise: weighted permutation (shuffle labels; person-level weights fixed)
pairW <- function(g1, g2) { obs <- deltaW(gidx[[g1]]) - deltaW(gidx[[g2]])
  ab <- c(gidx[[g1]], gidx[[g2]]); labs <- d$cond[ab]; cnt <- 0
  for (b in seq_len(B_ROB)) { pl <- sample(labs)
    dl <- deltaW(ab[pl == g1]) - deltaW(ab[pl == g2])
    if (!is.na(dl) && abs(dl) >= abs(obs)) cnt <- cnt + 1 }
  c(diff = obs, p = (cnt + 1) / (B_ROB + 1)) }
pwW <- do.call(rbind, lapply(PAIRS, function(pr) { z <- pairW(pr[1], pr[2])
  data.frame(comparison = paste(pr[1], "vs", pr[2]),
             diff_wt = unname(z["diff"]), perm_p = unname(z["p"])) }))
pwW$p_fdr <- p.adjust(pwW$perm_p, "fdr")
cat("\n=== covariate-BALANCED pairwise d_change (IPW-weighted; BH-FDR across 10) ===\n")
print(pwW, row.names = FALSE, digits = 3)

# ── figure: d_change bars (teal value-gradient), HORIZONTAL ──────────────────
res$cond <- factor(res$cond, levels = GROUPS)
tramp <- colorRampPalette(c("#CDEAE6", "#7FC9C2", "#3E9E97", "#0E5F5A"))(100)
nv <- (res$d_change - min(res$d_change)) / diff(range(res$d_change))   # value gradient
res$fill_color <- tramp[pmax(1, round(nv * 99) + 1)]
res$formal_label <- factor(gsub(" AI ", " AI\n", as.character(res$cond)),
                           levels = rev(gsub(" AI ", " AI\n", GROUPS)))
off <- 0.04 * diff(range(c(res$lo, res$hi, 0), na.rm = TRUE))
res$lab_x <- ifelse(res$d_change >= 0, res$hi + off, res$lo - off)
res$lab_hj <- ifelse(res$d_change >= 0, 0, 1)

p <- ggplot(res, aes(y = formal_label, x = d_change)) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.7) +
  geom_col(aes(fill = fill_color), color = "black", width = 0.65, linewidth = 0) +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0.3, linewidth = 0.5) +
  geom_text(aes(x = lab_x * 1.1, label = sprintf("%.3f", d_change), hjust = lab_hj),
            family = "Avenir", size = 3) +
  geom_point(color = "black", size = 3, shape = 18) +
  scale_fill_identity() +
  scale_x_continuous(labels = number_format(accuracy = 0.01),
                     expand = expansion(mult = c(0.10, 0.12))) +
  labs(y = "Investment Experimental Condition", x = "Change in Cross-participant Portfolio Diversity") +
  xlim(-0.6, 0.3) +
  theme_classic() +
  theme(
    text = element_text(family = "Avenir", color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.line = element_blank(),
    axis.text.x = element_text(family = "Avenir", size = 9, color = "black"),
    axis.text.y = element_text(family = "Avenir", size = 9, color = "black", angle = 90, hjust = 0.5),
    axis.title.x = element_text(family = "Avenir", size = 12, color = "black",
                                margin = margin(t = 12)),
    axis.title.y = element_text(family = "Avenir", size = 12, color = "black",
                                margin = margin(r = 12)),
    axis.ticks = element_line(color = "black", linewidth = 0.4),
    axis.ticks.length = unit(3, "pt"),
    panel.grid = element_blank(),
    plot.margin = margin(t = 15, r = 40, b = 15, l = 15),
    legend.position = "none"
  )
print(p)
ggsave(file.path(FIG_DIR, "diversity_single_vs_dual.png"), p,
       width = 4, height = 4.5, dpi = 500)
cat(sprintf("\nSaved diversity_single_vs_dual.png in %s\n", SCRIPT_DIR))
