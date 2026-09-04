# ==============================================================================
# counterfactual_bull_m2.R
# POST-HOC counterfactual: how would the arms have performed under a
# `reasonable` BULL market draw? (companion to counterfactual_regime_m2.py,
# which scores every participant's FIXED pre/post portfolio on all overlapping
# 15-row (calendar-day) windows of the pinned pre-study history and averages within
# regime buckets: Bear / Neutral / Bull = SPY-return terciles.)
#
# Valid counterfactual because portfolios are fixed at interaction time and the
# outcome window is an unforecastable buy-and-hold draw: performance is a
# deterministic function of (weights x return path), the path exogenous to all
# decisions. EXPLORATORY / post-hoc; addresses the regime-boundedness of the
# realized (defensive) 2026 windows.
#
# Model: PRE-REGISTERED structure on the bucket-conditional outcome:
#     post_m2_<bucket> ~ ai_group + wave + pre_m2_<bucket>      (HC3)
#   (5 arms, Risk-Neutral excluded, Default reference — exactly as
#    plot_active_m2_treatment.R). Figure: same emmeans ladder forest
#    (90/95/99% CIs, arm palette) as the main performance figure.
#
# Outputs: counterfactual_bull_m2_effect.png    (bull ladder, main-figure style)
#          counterfactual_bull_m2_by_wave.png   (per-wave bull panels, no labels)
#          counterfactual_m2_by_regime.png      (Bear | Neutral | Bull tercile
#                                                panels; bear = validation that
#                                                the counterfactual recovers the
#                                                realized defensive ordering)
# Inputs:  counterfactual_regime_m2.csv  (from counterfactual_regime_m2.py)
#   setwd("investment/code"); source("_setup.R"); source("counterfactual_bull_m2.R")
# ==============================================================================
suppressPackageStartupMessages({
  library(ggplot2); library(emmeans); library(dplyr); library(sandwich); library(scales)
})

.args <- commandArgs(FALSE); .f <- sub("^--file=", "", .args[grep("^--file=", .args)])
SCRIPT_DIR <- if (length(.f)) dirname(normalizePath(.f)) else getwd()
rd <- function(f) { p <- file.path(DATA_DIR, f); if (!file.exists(p)) p <- file.path(DATA_DIR, f)
                    read.csv(p, check.names = FALSE, stringsAsFactors = FALSE) }

ARMS <- c("Default", "Extremely Risk-Averse", "Somewhat Risk-Averse",
          "Somewhat Risk-Seeking", "Extremely Risk-Seeking")
df <- rd("counterfactual_regime_m2.csv") %>%
  filter(ai_group %in% ARMS) %>%
  mutate(ai_group = factor(ai_group, levels = ARMS),   # Default = reference
         wave = factor(wave))
cat("N by arm (single-AI, Risk-Neutral excluded):\n"); print(table(df$ai_group))

# ── pre-registered model per regime bucket (HC3 emmeans) ─────────────────────
fit_bucket <- function(bucket, data = df) {
  d <- data %>% rename(post = paste0("post_m2_", bucket),
                       pre  = paste0("pre_m2_",  bucket)) %>%
    filter(!is.na(post), !is.na(pre))
  m <- lm(post ~ ai_group + wave + pre, data = d)
  V <- vcovHC(m, type = "HC3")
  list(m = m, V = V, emm = emmeans(m, ~ ai_group, vcov. = V), n = nrow(d))
}

for (b in c("bear", "neutral", "bull", "all")) {
  fb <- fit_bucket(b)
  cat(sprintf("\n=== %-7s windows: post ~ ai_group + wave + pre (HC3), n = %d ===\n",
              toupper(b), fb$n))
  print(summary(fb$emm), digits = 4)
  cn <- contrast(fb$emm, method = "trt.vs.ctrl", ref = "Default")
  ct <- as.data.frame(summary(cn, adjust = "none"))
  ct$p_dunnett <- as.data.frame(summary(cn, adjust = "dunnettx"))$p.value
  ct$p_fdr     <- p.adjust(ct$p.value, "fdr")
  cat("--- arm vs Default ---\n")
  print(ct[, c("contrast", "estimate", "SE", "p.value", "p_dunnett", "p_fdr")],
        row.names = FALSE, digits = 3)
}

# sensitivity: alternative bull definitions (top quartile; SPY > +2%)
cat("\n=== SENSITIVITY: alternative bull definitions (arm vs Default, unadj p) ===\n")
for (b in c("bull_q4", "bull_gt2")) {
  fb <- fit_bucket(b)
  ct <- as.data.frame(summary(contrast(fb$emm, "trt.vs.ctrl", ref = "Default"),
                              adjust = "none"))
  cat(sprintf("-- %s (n = %d) --\n", b, fb$n))
  print(ct[, c("contrast", "estimate", "SE", "p.value")], row.names = FALSE, digits = 3)
}

# ── ladder forest (exact plot_active_m2_treatment.R style) ────────────────────
YORDER <- c("Extremely Risk-Averse" = 5, "Somewhat Risk-Averse" = 4,
            "Default" = 3, "Somewhat Risk-Seeking" = 2, "Extremely Risk-Seeking" = 1)
SHORT  <- c("Extremely Risk-Averse" = "Ext.\nAverse", "Somewhat Risk-Averse" = "Swt.\nAverse",
            "Default" = "Default",
            "Somewhat Risk-Seeking" = "Swt.\nSeeking", "Extremely Risk-Seeking" = "Ext.\nSeeking")
bias_colors <- c("Ext.\nAverse"="#1B7837","Swt.\nAverse"="#7FBF7B","Neutral"="#BABABA",
                 "Default"="#999999","Swt.\nSeeking"="#AF8DC3","Ext.\nSeeking"="#762A83")
lab_top2bottom <- unname(SHORT[names(sort(YORDER, decreasing = TRUE))])

ladder_pd <- function(emm_obj, extra = NULL) {
  as.data.frame(summary(emm_obj)) %>%
    mutate(arm = as.character(ai_group), y_position = YORDER[arm], label = SHORT[arm],
           CI_90_lower = emmean - SE*qnorm(0.95),  CI_90_upper = emmean + SE*qnorm(0.95),
           CI_95_lower = emmean - SE*qnorm(0.975), CI_95_upper = emmean + SE*qnorm(0.975),
           CI_99_lower = emmean - SE*qnorm(0.995), CI_99_upper = emmean + SE*qnorm(0.995)) %>%
    { if (is.null(extra)) . else mutate(., regime = extra) }
}

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
    plot.margin = margin(t = 20, r = 20, b = 20, l = 10)
  )

ladder_layers <- function() list(
  geom_errorbarh(aes(xmin = CI_99_lower, xmax = CI_99_upper, color = label),
                 height = 0.25, linewidth = 1.2, alpha = 0.3),
  geom_errorbarh(aes(xmin = CI_95_lower, xmax = CI_95_upper, color = label),
                 height = 0.20, linewidth = 0.9, alpha = 0.5),
  geom_errorbarh(aes(xmin = CI_90_lower, xmax = CI_90_upper, color = label),
                 height = 0.15, linewidth = 0.7, alpha = 0.8),
  geom_point(aes(x = emmean, color = label), size = 3, alpha = 0.9),
  scale_color_manual(values = bias_colors, breaks = lab_top2bottom),
  scale_y_continuous(breaks = 5:1, labels = lab_top2bottom,
                     expand = expansion(add = c(0.4, 0.4))),
  scale_x_continuous(labels = label_number(accuracy = 0.001)))

# BULL ladder (headline)
pd_bull <- ladder_pd(fit_bucket("bull")$emm)
p_bull <- ggplot(pd_bull, aes(y = y_position)) + ladder_layers() +
  guides(color = guide_legend(nrow = 2)) +
  labs(x = expression("Post-interaction Active M"^2*" (bull-market counterfactual)"),
       y = NULL) +
  nature_theme +
  theme(legend.position = "bottom", legend.justification = "center",
        legend.title = element_blank(), legend.text = element_text(size = 9),
        axis.text.y = element_text(angle = 90, hjust = 0.5))
print(p_bull)
ggsave(file.path(FIG_DIR, "counterfactual_bull_m2_effect.png"), p_bull,
       width = 5, height = 4.95, dpi = 500)

# PER-WAVE bull version — same model estimated WITHIN each wave (wave FE drops:
# post_m2_bull ~ ai_group + pre_m2_bull, HC3), three panels, no panel labels
# (mirrors plot_active_m2_treatment.R's per-wave figure).
waves <- sort(unique(as.character(df$wave)))
hedges_J <- function(n1, n2) { d0 <- n1 + n2 - 2; ifelse(d0 > 0, 1 - 3 / (4 * d0 - 1), 1) }
wave_fits <- lapply(waves, function(w) {
  dw <- df %>% filter(wave == w) %>%
    rename(post = post_m2_bull, pre = pre_m2_bull) %>%
    filter(!is.na(post), !is.na(pre)) %>% droplevels()
  mw <- lm(post ~ ai_group + pre, data = dw)
  ew <- emmeans(mw, ~ ai_group, vcov. = vcovHC(mw, type = "HC3"))
  cn <- contrast(ew, method = "trt.vs.ctrl", ref = "Default")
  ct <- as.data.frame(summary(cn, adjust = "none", infer = c(TRUE, TRUE)))
  ct$p_dunnett <- as.data.frame(summary(cn, adjust = "dunnettx"))$p.value
  nb <- table(dw$ai_group)
  ct$hedges_g <- sapply(seq_len(nrow(ct)), function(i) {
    gg <- strsplit(gsub("[()]", "", as.character(ct$contrast[i])), " - ")[[1]]
    (ct$estimate[i] / sigma(mw)) * hedges_J(nb[[gg[1]]], nb[[gg[2]]]) })
  ct$wave <- w; ct$n <- nrow(dw)
  out <- ladder_pd(ew); out$wave <- w
  list(pd = out, ct = ct)
})
pd_wave <- do.call(rbind, lapply(wave_fits, `[[`, "pd"))
ct_bull_wave <- do.call(rbind, lapply(wave_fits, `[[`, "ct"))
ct_bull_wave$p_fdr_all <- p.adjust(ct_bull_wave$p.value, "fdr")
cat("\n=== per-wave BULL-counterfactual contrasts vs Default (HC3;\n")
cat("    unadj 95% CI; hedges_g = est / residual SD x J; FDR across 12) ===\n")
print(ct_bull_wave[, c("wave", "contrast", "estimate", "lower.CL", "upper.CL",
                       "hedges_g", "p.value", "p_dunnett", "p_fdr_all")],
      row.names = FALSE, digits = 3)

p_wave <- ggplot(pd_wave, aes(y = y_position)) + ladder_layers() +
  scale_x_continuous(breaks = c(-0.02, 0, 0.02),        # sparse ticks: narrow panels
                     labels = label_number(accuracy = 0.01)) +
  facet_wrap(~ wave, nrow = 1) +
  guides(color = guide_legend(nrow = 1)) +
  labs(x = expression("Post-interaction Active M"^2*" (bull-market counterfactual)"),
       y = NULL) +
  nature_theme +
  theme(legend.position = "bottom", legend.justification = "center",
        legend.title = element_blank(), legend.text = element_text(size = 9),
        strip.background = element_blank(),
        strip.text = element_blank(),
        panel.spacing = unit(1.2, "lines"),
        axis.text.y = element_text(angle = 90, hjust = 0.5))
print(p_wave)
ggsave(file.path(FIG_DIR, "counterfactual_bull_m2_by_wave.png"), p_wave,
       width = 5.5, height = 4.95, dpi = 500)

# ── REGIME-PANEL figure: Bear | Neutral | Bull tercile ladders ────────────────
# Same pre-registered model per bucket (full sample, wave FE); SHARED x-axis
# (same units across panels — the level shift across regimes is informative).
# XEXP_REG = proportional x padding (left, right) per panel — room for
# hand-added significance marks; adjust to taste.
XEXP_REG <- c(0.80, 0.80)
BUCKETS <- c(bear = "Bear windows", neutral = "Neutral windows", bull = "Bull windows")
pd_reg <- do.call(rbind, lapply(names(BUCKETS), function(b) {
  out <- ladder_pd(fit_bucket(b)$emm); out$regime <- BUCKETS[b]; out }))
pd_reg$regime <- factor(pd_reg$regime, levels = unname(BUCKETS))

p_reg <- ggplot(pd_reg, aes(y = y_position)) + ladder_layers() +
  scale_x_continuous(labels = label_number(accuracy = 0.01), n.breaks = 4,
                     expand = expansion(mult = XEXP_REG)) +
  facet_wrap(~ regime, nrow = 1) +
  guides(color = guide_legend(nrow = 1)) +
  labs(x = expression("Post-interaction Active M"^2*" (counterfactual by market regime)"),
       y = NULL) +
  nature_theme +
  theme(
    legend.position = "bottom", legend.justification = "center",
    legend.title = element_blank(), legend.text = element_text(size = 9),
    strip.background = element_blank(),
    strip.text = element_text(family = "Avenir", size = 8),  # regime labels; blank to self-label
    panel.spacing = unit(1.2, "lines"),
    axis.text.y = element_text(angle = 90, hjust = 0.5),
    axis.text.x = element_text(vjust = 0.5),
    legend.box.margin = margin(t = -3)
  )

print(p_reg)
ggsave(file.path(FIG_DIR, "counterfactual_m2_by_regime.png"), p_reg,
       width = 5.5, height = 4.95, dpi = 500)
cat(sprintf("\nSaved counterfactual_bull_m2_effect.png, counterfactual_bull_m2_by_wave.png,\nand counterfactual_m2_by_regime.png in %s\n",
            SCRIPT_DIR))
cat("NOTE: post-hoc / exploratory. Bull = top tercile of SPY 15-row (calendar-day) window\n")
cat("returns over the pinned pre-study history (2021-11 -> 2026-05-04); portfolios\n")
cat("fixed at interaction time; window draw exogenous to decisions.\n")
