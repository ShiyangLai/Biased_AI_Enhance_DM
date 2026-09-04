# ==============================================================================
# bias_side_performance.R
# Investment analog of third_figure_c1.R (political): within the BIASED single-AI
# arms, does it matter whether the AI's bias is on the SAME side as the
# participant's own risk lean (echo-chamber) or the OPPOSITE side (opposition)?
#
# Political Same/Opposite = AI party vs user party. Investment analog = AI risk
# direction (averse/seeking) vs the participant's own risk lean:
#   participant lean : median split of risk_pref_score (higher = more seeking)
#   ai_dir           : Averse (Ext/Som Risk-Averse) or Seeking (Ext/Som Seeking)
#   BiasSide = "Same"     (echo-chamber)  if ai_dir == lean
#              "Opposite" (opposition)    otherwise
#   (Default + Risk-Neutral EXCLUDED — biased arms only, as in c1.)
#
# Outcome: post_active_m2_ann (realized performance).
# Model (pre-registered structure; political NID FE -> wave FE; UID clustering
# drops -> HC3, one obs/participant; UStance/UIdeo/AICorrectness no analog):
#   post_active_m2_ann ~ BiasSide + wave + pre_active_m2_ann   (HC3)
# Also: BiasSide x lean interaction (does echo/opposition differ for
# averse- vs seeking-leaning participants?), FDR + Hedges' g.
#
# Inputs: active_m2_treatment_data.csv , participant_covariates.csv
#   setwd("investment/code"); source("_setup.R"); source("bias_side_performance.R")
# ==============================================================================
suppressPackageStartupMessages({
  library(ggplot2); library(emmeans); library(dplyr); library(sandwich)
  library(lmtest); library(scales)
})

.args <- commandArgs(FALSE); .f <- sub("^--file=", "", .args[grep("^--file=", .args)])
SCRIPT_DIR <- if (length(.f)) dirname(normalizePath(.f)) else getwd()
rd <- function(f) { p <- file.path(DATA_DIR, f); if (!file.exists(p)) p <- file.path(DATA_DIR, f)
                    read.csv(p, check.names = FALSE, stringsAsFactors = FALSE) }

m2  <- rd("active_m2_treatment_data.csv")
cov <- rd("participant_covariates.csv")

d <- m2 %>%
  filter(ai_group %in% c("Extremely Risk-Averse", "Somewhat Risk-Averse",
                         "Extremely Risk-Seeking", "Somewhat Risk-Seeking")) %>%   # biased only
  left_join(cov %>% select(participantId, risk_pref_score), by = "participantId") %>%
  filter(!is.na(risk_pref_score), !is.na(post_active_m2_ann), !is.na(pre_active_m2_ann))

thr <- median(d$risk_pref_score, na.rm = TRUE)
d <- d %>% mutate(
  lean   = ifelse(risk_pref_score >= thr, "Seeking", "Averse"),          # participant risk lean
  ai_dir = ifelse(grepl("Averse", ai_group), "Averse", "Seeking"),        # AI bias direction
  BiasSide = factor(ifelse(ai_dir == lean, "Same", "Opposite"),
                    levels = c("Same", "Opposite")),
  lean = factor(lean, levels = c("Averse", "Seeking")),
  wave = factor(wave))
cat(sprintf("N (biased arms, risk_pref median split at %.3f): %d\n", thr, nrow(d)))
cat("\nBiasSide x lean cell sizes:\n"); print(table(d$lean, d$BiasSide))

hedges_J <- function(n1, n2) { d0 <- n1 + n2 - 2; ifelse(d0 > 0, 1 - 3 / (4 * d0 - 1), 1) }

# ── main model: post ~ BiasSide + wave + pre (HC3) ───────────────────────────
m <- lm(post_active_m2_ann ~ BiasSide + wave + pre_active_m2_ann, data = d)
V <- vcovHC(m, type = "HC3")
cat("\n=== OLS (HC3): post_active_m2_ann ~ BiasSide + wave + pre ===\n")
print(coeftest(m, vcov = V))
emm <- emmeans(m, ~ BiasSide, vcov. = V)
cat("\n=== adjusted mean post Active M2 by bias side ===\n")
es <- as.data.frame(summary(emm)); print(es, digits = 4)
pw <- as.data.frame(summary(pairs(emm, adjust = "none"), infer = TRUE))
nb <- table(d$BiasSide)
pw$hedges_g <- (pw$estimate / sigma(m)) * hedges_J(nb[["Same"]], nb[["Opposite"]])
cat("--- Same vs Opposite contrast + Hedges' g ---\n")
print(pw[, c("contrast","estimate","SE","p.value","hedges_g")], row.names = FALSE, digits = 3)

# ── BiasSide x lean interaction (does echo/opposition differ by lean?) ───────
mi <- lm(post_active_m2_ann ~ BiasSide * lean + wave + pre_active_m2_ann, data = d)
Vi <- vcovHC(mi, type = "HC3")
cat("\n=== interaction: BiasSide x lean (HC3) ===\n")
ci <- coeftest(mi, vcov = Vi); print(ci[grep("BiasSide|lean", rownames(ci)), , drop = FALSE])
emmi <- emmeans(mi, ~ BiasSide | lean, vcov. = Vi)
cat("\n--- Same - Opposite within each lean (FDR) ---\n")
print(summary(contrast(emmi, "pairwise", by = "lean", adjust = "fdr"), infer = TRUE), digits = 3)

# ── figure: 4 cells — bias side x participant lean (Averse green / Seeking purple) ─
# Grouped bars make the interaction (crossover) visible instead of hiding it in
# the main effect. Cell means from the interaction model (HC3).
pd <- as.data.frame(summary(emmeans(mi, ~ BiasSide * lean, vcov. = Vi))) %>%
  rename(estimate = emmean, lo = lower.CL, hi = upper.CL) %>%
  mutate(BiasSide = factor(as.character(BiasSide), levels = c("Same", "Opposite")),
         lean = factor(as.character(lean), levels = c("Averse", "Seeking")))
lean_col <- c("Averse" = "#59A14F", "Seeking" = "#B07AA1")
ypad <- 0.15 * (max(pd$hi) - min(pd$lo)); dodge <- position_dodge(width = 0.7)
p <- ggplot(pd, aes(x = BiasSide, y = estimate, fill = lean)) +
  geom_col(position = dodge, color = "black", width = 0.62, linewidth = 0) +
  geom_errorbar(aes(ymin = lo, ymax = hi), position = dodge, width = 0.18, linewidth = 0.6) +
  geom_text(aes(y = lo - ypad * 0.45, label = sprintf("%.3f", estimate)),
            position = dodge, size = 3, family = "Avenir") +
  scale_fill_manual(values = lean_col, name = "Participant\nrisk lean") +
  scale_x_discrete(name = "AI Bias Direction",
                   labels = c("Same" = "Echo-Chamber Bias", "Opposite" = "Opposition Bias")) +
  scale_y_continuous(name = expression("Post-interaction Active M"^2),
                     labels = number_format(accuracy = 0.001)) +
  coord_cartesian(ylim = c(min(pd$lo) - ypad, 0)) +
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
ggsave(file.path(FIG_DIR, "bias_side_performance.png"), p, width = 5.8, height = 4, dpi = 500)
cat(sprintf("\nSaved bias_side_performance.png in %s\n", SCRIPT_DIR))

# ══ per-wave: same 4-cell interaction WITHIN each wave (regime test) ══════════
# Within a wave the wave FE is constant, so it drops: post ~ BiasSide*lean + pre.
# Lean = the SAME global median split (person attribute, consistent across waves).
waves <- sort(unique(as.character(d$wave)))
pd_w <- do.call(rbind, lapply(waves, function(w) {
  dw <- droplevels(d[d$wave == w, ])
  mw <- lm(post_active_m2_ann ~ BiasSide * lean + pre_active_m2_ann, data = dw)
  Vw <- vcovHC(mw, type = "HC3")
  cat(sprintf("\n=== %s (n=%d) — BiasSide x lean (HC3) ===\n", w, nrow(dw)))
  print(table(dw$lean, dw$BiasSide))
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
  scale_y_continuous(name = expression("Post-interaction Active M"^2),
                     labels = number_format(accuracy = 0.001)) +
  coord_cartesian(ylim = c(min(pd_w$lo) - ypadw, 0)) +
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
ggsave(file.path(FIG_DIR, "bias_side_performance_by_wave.png"), pw_fig,
       width = 9, height = 3.6, dpi = 500)
cat(sprintf("\nSaved bias_side_performance_by_wave.png in %s\n", SCRIPT_DIR))
