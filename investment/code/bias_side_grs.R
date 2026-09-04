# ==============================================================================
# bias_side_grs.R
# Same design as bias_side_performance.R, with the SECOND primary outcome:
# GRS F-statistic (ex-ante mean-variance efficiency; LOWER = closer to the
# efficient frontier = BETTER). Within the biased single-AI arms:
#   BiasSide = "Same" (echo-chamber: AI risk direction matches the participant's
#              own risk lean) vs "Opposite" (opposition), lean = median split of
#              risk_pref_score.
#
# KEY PREDICTION (triangulation story): GRS is EX-ANTE — it does not depend on
# the realized 14-day window — so unlike Active M2 the BiasSide x lean pattern
# should be REGIME-STABLE across waves: whoever ends up more risk-seeking is
# further from the frontier in every wave. Wave-scaling on M2 + wave-stability
# on GRS together identify the risk-exposure mechanism.
#
# Model (pre-registered structure): post_grs_F_1y ~ BiasSide + wave + pre_grs_F_1y (HC3),
# plus BiasSide x lean interaction; per-wave versions drop the wave FE.
# Figures identical in style to bias_side_performance.R (4-cell grouped bars,
# Averse lean = green / Seeking lean = purple; per-wave facets).
#
# GRS windows: primary = 1y ending at STUDY_START (pinned, ex-ante); 3y and 5y
# estimation windows are run as a REGIME-SENSITIVITY check on the frontier
# (mu-hat from short windows inherits that window's regime). Additional outcome:
# regime-averaged Active M2 (bootstrap over multi-regime history; higher=better).
# Inputs: grs_data.csv , participant_covariates.csv
#   setwd("investment/code"); source("_setup.R"); source("bias_side_grs.R")
# ==============================================================================
suppressPackageStartupMessages({
  library(ggplot2); library(emmeans); library(dplyr); library(sandwich)
  library(lmtest); library(scales)
})

.args <- commandArgs(FALSE); .f <- sub("^--file=", "", .args[grep("^--file=", .args)])
SCRIPT_DIR <- if (length(.f)) dirname(normalizePath(.f)) else getwd()
rd <- function(f) { p <- file.path(DATA_DIR, f); if (!file.exists(p)) p <- file.path(DATA_DIR, f)
                    read.csv(p, check.names = FALSE, stringsAsFactors = FALSE) }

grs <- rd("grs_data.csv")
cov <- rd("participant_covariates.csv")

d <- grs %>%
  filter(ai_group %in% c("Extremely Risk-Averse", "Somewhat Risk-Averse",
                         "Extremely Risk-Seeking", "Somewhat Risk-Seeking")) %>%   # biased only
  left_join(cov %>% select(participantId, risk_pref_score), by = "participantId") %>%
  filter(!is.na(risk_pref_score), !is.na(post_grs_F_1y), !is.na(pre_grs_F_1y))

thr <- median(d$risk_pref_score, na.rm = TRUE)
d <- d %>% mutate(
  lean   = ifelse(risk_pref_score >= thr, "Seeking", "Averse"),
  ai_dir = ifelse(grepl("Averse", ai_group), "Averse", "Seeking"),
  BiasSide = factor(ifelse(ai_dir == lean, "Same", "Opposite"),
                    levels = c("Same", "Opposite")),
  lean = factor(lean, levels = c("Averse", "Seeking")),
  wave = factor(wave))
cat(sprintf("N (biased arms, risk_pref median split at %.3f): %d\n", thr, nrow(d)))
cat("\nBiasSide x lean cell sizes:\n"); print(table(d$lean, d$BiasSide))

hedges_J <- function(n1, n2) { d0 <- n1 + n2 - 2; ifelse(d0 > 0, 1 - 3 / (4 * d0 - 1), 1) }

# ── main model: post_grs_F_1y ~ BiasSide + wave + pre_grs_F_1y (HC3) ────────────────
m <- lm(post_grs_F_1y ~ BiasSide + wave + pre_grs_F_1y, data = d)
V <- vcovHC(m, type = "HC3")
cat("\n=== OLS (HC3): post_grs_F_1y ~ BiasSide + wave + pre_grs_F_1y ===\n")
cat("    (LOWER GRS F = closer to efficient frontier = BETTER)\n")
print(coeftest(m, vcov = V))
emm <- emmeans(m, ~ BiasSide, vcov. = V)
cat("\n=== adjusted mean post GRS F by bias side ===\n")
es <- as.data.frame(summary(emm)); print(es, digits = 4)
pw <- as.data.frame(summary(pairs(emm, adjust = "none"), infer = TRUE))
nb <- table(d$BiasSide)
pw$hedges_g <- (pw$estimate / sigma(m)) * hedges_J(nb[["Same"]], nb[["Opposite"]])
cat("--- Same vs Opposite contrast + Hedges' g ---\n")
print(pw[, c("contrast","estimate","SE","p.value","hedges_g")], row.names = FALSE, digits = 3)

# ── BiasSide x lean interaction ───────────────────────────────────────────────
mi <- lm(post_grs_F_1y ~ BiasSide * lean + wave + pre_grs_F_1y, data = d)
Vi <- vcovHC(mi, type = "HC3")
cat("\n=== interaction: BiasSide x lean (HC3) ===\n")
ci <- coeftest(mi, vcov = Vi); print(ci[grep("BiasSide|lean", rownames(ci)), , drop = FALSE])
emmi <- emmeans(mi, ~ BiasSide | lean, vcov. = Vi)
cat("\n--- Same - Opposite within each lean (FDR) ---\n")
print(summary(contrast(emmi, "pairwise", by = "lean", adjust = "fdr"), infer = TRUE), digits = 3)

# ── figure: 4 cells — bias side x lean (Averse green / Seeking purple) ────────
pd <- as.data.frame(summary(emmeans(mi, ~ BiasSide * lean, vcov. = Vi))) %>%
  rename(estimate = emmean, lo = lower.CL, hi = upper.CL) %>%
  mutate(BiasSide = factor(as.character(BiasSide), levels = c("Same", "Opposite")),
         lean = factor(as.character(lean), levels = c("Averse", "Seeking")))
lean_col <- c("Averse" = "#59A14F", "Seeking" = "#B07AA1")
ypad <- 0.15 * (max(pd$hi) - min(pd$lo)); dodge <- position_dodge(width = 0.7)
p <- ggplot(pd, aes(x = BiasSide, y = estimate, fill = lean)) +
  geom_col(position = dodge, color = "black", width = 0.62, linewidth = 0) +
  geom_errorbar(aes(ymin = lo, ymax = hi), position = dodge, width = 0.18, linewidth = 0.6) +
  geom_text(aes(y = hi + ypad * 0.45, label = sprintf("%.3f", estimate)),
            position = dodge, size = 3, family = "Avenir") +
  scale_fill_manual(values = lean_col, name = "Participant\nrisk lean") +
  scale_x_discrete(name = "AI Bias Direction",
                   labels = c("Same" = "Echo-Chamber Bias", "Opposite" = "Opposition Bias")) +
  scale_y_continuous(name = "Post-interaction GRS F (lower = better)",
                     labels = number_format(accuracy = 0.01)) +
  coord_cartesian(ylim = c(min(pd$lo) - ypad, max(pd$hi) + ypad)) +
  theme_classic() +
  theme(
    text = element_text(family = "Avenir"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.line = element_blank(),
    axis.title.x = element_text(family = "Avenir", size = 12, margin = margin(t = 12)),
    axis.title.y = element_text(family = "Avenir", size = 12, color = "black", margin = margin(r = 12)),
    axis.text = element_text(family = "Avenir", size = 10, color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 0.4),
    axis.ticks.length = unit(3, "pt"),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.title = element_text(family = "Avenir", size = 10),
    legend.text = element_text(family = "Avenir", size = 9),
    plot.margin = margin(t = 15, r = 15, b = 15, l = 15)
  )
print(p)
ggsave(file.path(FIG_DIR, "bias_side_grs.png"), p, width = 5.8, height = 4, dpi = 500)
cat(sprintf("\nSaved bias_side_grs.png in %s\n", SCRIPT_DIR))

# ══ per-wave: regime-STABILITY test (prediction: same sign in every wave) ═════
waves <- sort(unique(as.character(d$wave)))
pd_w <- do.call(rbind, lapply(waves, function(w) {
  dw <- droplevels(d[d$wave == w, ])
  mw <- lm(post_grs_F_1y ~ BiasSide * lean + pre_grs_F_1y, data = dw)
  Vw <- vcovHC(mw, type = "HC3")
  cat(sprintf("\n=== %s (n=%d) — BiasSide x lean (HC3) ===\n", w, nrow(dw)))
  ct <- coeftest(mw, vcov = Vw)
  print(ct[grep("BiasSide|lean", rownames(ct)), , drop = FALSE])
  ew <- emmeans(mw, ~ BiasSide * lean, vcov. = Vw)
  cat("-- Same - Opposite within lean (FDR) --\n")
  print(summary(contrast(ew, "pairwise", by = "lean", adjust = "fdr"), infer = TRUE), digits = 3)
  es <- as.data.frame(summary(ew)); es$wave <- w; es
}))
pd_w <- pd_w %>% rename(estimate = emmean, lo = lower.CL, hi = upper.CL) %>%
  mutate(BiasSide = factor(as.character(BiasSide), levels = c("Same", "Opposite")),
         lean = factor(as.character(lean), levels = c("Averse", "Seeking")))

ypadw <- 0.10 * (max(pd_w$hi) - min(pd_w$lo))
pw_fig <- ggplot(pd_w, aes(x = BiasSide, y = estimate, fill = lean)) +
  geom_col(position = dodge, color = "black", width = 0.62, linewidth = 0) +
  geom_errorbar(aes(ymin = lo, ymax = hi), position = dodge, width = 0.18, linewidth = 0.5) +
  facet_wrap(~ wave, nrow = 1,
             labeller = as_labeller(function(x) gsub("wave", "Wave ", x))) +
  scale_fill_manual(values = lean_col, name = "Participant\nrisk lean") +
  scale_x_discrete(name = "AI Bias Direction",
                   labels = c("Same" = "Echo-\nChamber", "Opposite" = "Opposition")) +
  scale_y_continuous(name = "Post-interaction GRS F",
                     labels = number_format(accuracy = 0.01)) +
  coord_cartesian(ylim = c(min(pd_w$lo) - ypadw, max(pd_w$hi) + ypadw)) +
  theme_classic() +
  theme(
    text = element_text(family = "Avenir"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.line = element_blank(),
    axis.title.x = element_text(family = "Avenir", size = 11, margin = margin(t = 10)),
    axis.title.y = element_text(family = "Avenir", size = 11, color = "black", margin = margin(r = 10)),
    axis.text = element_text(family = "Avenir", size = 8, color = "black"),
    strip.background = element_rect(fill = "grey92", color = "black", linewidth = 0.5),
    strip.text = element_text(family = "Avenir", size = 10, face = "bold"),
    axis.ticks = element_line(color = "black", linewidth = 0.4),
    axis.ticks.length = unit(3, "pt"),
    panel.grid = element_blank(), panel.spacing = unit(0.8, "lines"),
    legend.position = "right",
    legend.title = element_text(family = "Avenir", size = 10),
    legend.text = element_text(family = "Avenir", size = 9),
    plot.margin = margin(t = 15, r = 15, b = 15, l = 15)
  )
print(pw_fig)
ggsave(file.path(FIG_DIR, "bias_side_grs_by_wave.png"), pw_fig,
       width = 9, height = 3.6, dpi = 500)
cat(sprintf("\nSaved bias_side_grs_by_wave.png in %s\n", SCRIPT_DIR))

# ══ SENSITIVITY: GRS under longer estimation windows (3y, 5y) ═════════════════
# mu-hat from a 1y window inherits that year's regime; if the BiasSide x lean
# interaction is a frontier-regime artifact it should attenuate/flip under
# longer, multi-regime estimation windows.
for (wy in c("3y", "5y")) {
  fml <- sprintf("post_grs_F_%s ~ BiasSide * lean + wave + pre_grs_F_%s", wy, wy)
  ms <- lm(as.formula(fml), data = d)
  Vs <- vcovHC(ms, type = "HC3")
  cat(sprintf("\n=== GRS %s estimation window — BiasSide x lean (HC3) ===\n", wy))
  cs <- coeftest(ms, vcov = Vs)
  print(cs[grep("BiasSide|lean", rownames(cs)), , drop = FALSE])
  esw <- emmeans(ms, ~ BiasSide | lean, vcov. = Vs)
  cat("-- Same - Opposite within lean (FDR) --\n")
  print(summary(contrast(esw, "pairwise", by = "lean", adjust = "fdr"), infer = TRUE), digits = 3)
}

# ══ ADDITIONAL OUTCOME: regime-averaged Active M2 (bootstrap; HIGHER = better) ═
# Expected Active M2 over K=200 random 10-day windows from the multi-regime
# pre-study history (§6.7 convention) — the regime-neutral performance measure.
dr <- d %>% filter(!is.na(post_ra_m2), !is.na(pre_ra_m2))
mr <- lm(post_ra_m2 ~ BiasSide * lean + wave + pre_ra_m2, data = dr)
Vr <- vcovHC(mr, type = "HC3")
cat(sprintf("\n=== regime-averaged Active M2 (n=%d) — BiasSide x lean (HC3) ===\n", nrow(dr)))
cat("    (HIGHER = better; expected performance across regimes)\n")
cr <- coeftest(mr, vcov = Vr)
print(cr[grep("BiasSide|lean", rownames(cr)), , drop = FALSE])
emr <- emmeans(mr, ~ BiasSide | lean, vcov. = Vr)
cat("-- Same - Opposite within lean (FDR) --\n")
print(summary(contrast(emr, "pairwise", by = "lean", adjust = "fdr"), infer = TRUE), digits = 3)

# figure (same style; higher = better)
pdr <- as.data.frame(summary(emmeans(mr, ~ BiasSide * lean, vcov. = Vr))) %>%
  rename(estimate = emmean, lo = lower.CL, hi = upper.CL) %>%
  mutate(BiasSide = factor(as.character(BiasSide), levels = c("Same", "Opposite")),
         lean = factor(as.character(lean), levels = c("Averse", "Seeking")))
ypr <- 0.15 * (max(pdr$hi) - min(pdr$lo))
pr_fig <- ggplot(pdr, aes(x = BiasSide, y = estimate, fill = lean)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  geom_col(position = dodge, color = "black", width = 0.62, linewidth = 0) +
  geom_errorbar(aes(ymin = lo, ymax = hi), position = dodge, width = 0.18, linewidth = 0.6) +
  geom_text(aes(y = hi + ypr * 0.45, label = sprintf("%.4f", estimate)),
            position = dodge, size = 3, family = "Avenir") +
  scale_fill_manual(values = lean_col, name = "Participant\nrisk lean") +
  scale_x_discrete(name = "AI Bias Direction",
                   labels = c("Same" = "Echo-Chamber Bias", "Opposite" = "Opposition Bias")) +
  scale_y_continuous(name = expression("Regime-averaged Active M"^2*" (higher = better)"),
                     labels = number_format(accuracy = 0.001)) +
  coord_cartesian(ylim = c(min(pdr$lo, 0) - ypr, max(pdr$hi) + ypr)) +
  theme_classic() +
  theme(
    text = element_text(family = "Avenir"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.line = element_blank(),
    axis.title.x = element_text(family = "Avenir", size = 12, margin = margin(t = 12)),
    axis.title.y = element_text(family = "Avenir", size = 12, color = "black", margin = margin(r = 12)),
    axis.text = element_text(family = "Avenir", size = 10, color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 0.4),
    axis.ticks.length = unit(3, "pt"),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.title = element_text(family = "Avenir", size = 10),
    legend.text = element_text(family = "Avenir", size = 9),
    plot.margin = margin(t = 15, r = 15, b = 15, l = 15)
  )
print(pr_fig)
ggsave(file.path(FIG_DIR, "bias_side_ra_m2.png"), pr_fig, width = 5.8, height = 4, dpi = 500)
cat(sprintf("\nSaved bias_side_ra_m2.png in %s\n", SCRIPT_DIR))
