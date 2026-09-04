# ==============================================================================
# plot_grs_treatment.R
# Treatment-effect ladder figure for the GRS F-statistic (Single AI, 5 arms),
# style identical to plot_active_m2_treatment.R's per-wave figure — but the
# panels are FRONTIER REGIMES, not waves:
#
#   GRS never sees the realized outcome window (it scores the held portfolio
#   against a frontier estimated on PRE-STUDY history), so waves cannot carry
#   regime for GRS; the regime lives in the frontier's estimation window.
#   Panels (grs_regime_extremes.csv from grs_regime_extremes.py;
#   all windows end before STUDY_START -> ex-ante):
#     Full history   : the entire 25-asset common panel, 2021-11 -> 2026-05-04
#                      (RIVN's IPO bounds it to ~4.5y; a 20y frontier is
#                      impossible without dropping assets participants hold)
#     Pre-study year : the 1y window ending at study start (2025-05 -> 2026-05,
#                      SPY +28.1% — NOT a "defensive year" by index direction;
#                      the defensive-favoring regime is the realized STUDY
#                      windows, not this frontier year)
#     Bull year      : the 1y window with the HIGHEST SPY return (2023-10 ->
#                      2024-10, +43.3%) — the bull-regime counterfactual
#   The most-BEARISH year (2022, SPY -19.7%) is in the csv but EXCLUDED from
#   the figure: 95.5% of portfolios have negative Sharpe under that frontier,
#   so the sign-blind F rewards the biggest losers (see caveat below). The
#   three plotted panels are artifact-clean (negative-Sharpe share <= 0.2%).
#
# Model per panel (pre-registered structure, full sample, wave FE retained):
#   post_grs_F_<w> ~ ai_group + wave + pre_grs_F_<w>     (HC3)
# LOWER GRS F = closer to the mean-variance frontier = BETTER.
# x scales are free: F units differ mechanically across frontier windows.
#
# CAVEAT (sign-symmetry artifact): GRS F uses the SQUARED portfolio Sharpe, so
# it cannot tell mu_p from -mu_p. Under the BEAR-year frontier ~95% of
# portfolios have NEGATIVE Sharpe, and arms that lost MORE (seeking) get
# larger squared Sharpe -> deceptively SMALL F ("efficient"). The companion
# signed-Sharpe ANCOVA printed below diagnoses this panel-by-panel; read the
# bear-year panel through that table, not the F ordering alone.
#
# Inputs: grs_regime_extremes.csv
# Output: grs_treatment_by_regime.png
#   setwd("investment/code"); source("_setup.R"); source("plot_grs_treatment.R")
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

df <- rd("grs_regime_extremes.csv") %>%
  filter(ai_group %in% ARMS) %>%
  mutate(ai_group = factor(ai_group, levels = ARMS), wave = factor(wave))

# panel definitions: column suffix -> display label (left-to-right order)
REGIMES <- c(full   = "Full history (2021–26)",
             def1y  = "Pre-study year (SPY +28.1%)",
             bull1y = "Bull year (SPY +43.3%)")

emm_by_arm <- function(suffix, stem = "grs_F") {
  dd <- df %>%
    rename(post = all_of(paste0("post_", stem, "_", suffix)),
           pre  = all_of(paste0("pre_",  stem, "_", suffix))) %>%
    filter(!is.na(post), !is.na(pre)) %>% droplevels()
  mo <- lm(post ~ ai_group + wave + pre, data = dd)
  V  <- sandwich::vcovHC(mo, type = "HC3")
  ew <- emmeans(mo, ~ ai_group, vcov. = V)
  es <- as.data.frame(summary(ew)); es$n <- nrow(dd)
  cn <- contrast(ew, method = "trt.vs.ctrl", ref = "Default")
  ct <- as.data.frame(summary(cn, adjust = "none"))
  ct$p_dunnett <- as.data.frame(summary(cn, adjust = "dunnettx"))$p.value
  list(emm = es, ct = ct)
}

fits <- lapply(names(REGIMES), emm_by_arm)
names(fits) <- names(REGIMES)

pd <- do.call(rbind, lapply(names(REGIMES), function(k) {
  es <- fits[[k]]$emm; es$regime <- REGIMES[k]; es }))
ct <- do.call(rbind, lapply(names(REGIMES), function(k) {
  cc <- fits[[k]]$ct; cc$regime <- REGIMES[k]; cc }))
names(ct)[names(ct) == "p.value"] <- "p_unadj"
ct$p_fdr_all <- p.adjust(ct$p_unadj, "fdr")

cat("=== emmeans (adjusted mean post GRS F per arm; HC3; LOWER = better) ===\n")
print(pd[, c("regime", "ai_group", "emmean", "SE", "n")], row.names = FALSE, digits = 4)
cat("\n=== contrasts vs Default (HC3; p_fdr_all = BH across all 12 tests) ===\n")
print(ct[, c("regime", "contrast", "estimate", "SE", "p_unadj", "p_dunnett", "p_fdr_all")],
      row.names = FALSE, digits = 3)

# ── companion: SIGNED-Sharpe ANCOVA (sign-artifact diagnostic; HIGHER=better) ──
sh <- lapply(names(REGIMES), function(k) emm_by_arm(k, stem = "sharpe"))
names(sh) <- names(REGIMES)
ct_sh <- do.call(rbind, lapply(names(REGIMES), function(k) {
  cc <- sh[[k]]$ct; cc$regime <- REGIMES[k]; cc }))
names(ct_sh)[names(ct_sh) == "p.value"] <- "p_unadj"
ct_sh$p_fdr_all <- p.adjust(ct_sh$p_unadj, "fdr")
cat("\n=== companion signed-Sharpe contrasts vs Default (HIGHER = better;\n")
cat("    use this to read the bear-year panel — GRS F is sign-blind) ===\n")
print(ct_sh[, c("regime", "contrast", "estimate", "SE", "p_unadj", "p_dunnett", "p_fdr_all")],
      row.names = FALSE, digits = 3)

# ── plot (per-wave figure conventions) ────────────────────────────────────────
YORDER <- c("Extremely Risk-Averse" = 5, "Somewhat Risk-Averse" = 4,
            "Default" = 3, "Somewhat Risk-Seeking" = 2, "Extremely Risk-Seeking" = 1)
SHORT  <- c("Extremely Risk-Averse" = "Ext.\nAverse", "Somewhat Risk-Averse" = "Swt.\nAverse",
            "Default" = "Default",
            "Somewhat Risk-Seeking" = "Swt.\nSeeking", "Extremely Risk-Seeking" = "Ext.\nSeeking")
bias_colors <- c("Ext.\nAverse"="#1B7837","Swt.\nAverse"="#7FBF7B",
                 "Default"="#999999","Swt.\nSeeking"="#AF8DC3","Ext.\nSeeking"="#762A83")
lab_top2bottom <- unname(SHORT[names(sort(YORDER, decreasing = TRUE))])

pd <- pd %>% mutate(
  arm        = as.character(ai_group),
  y_position = YORDER[arm],
  label      = SHORT[arm],
  regime     = factor(regime, levels = unname(REGIMES)),
  CI_90_lower = emmean - SE * qnorm(0.95),  CI_90_upper = emmean + SE * qnorm(0.95),
  CI_95_lower = emmean - SE * qnorm(0.975), CI_95_upper = emmean + SE * qnorm(0.975),
  CI_99_lower = emmean - SE * qnorm(0.995), CI_99_upper = emmean + SE * qnorm(0.995))

XEXP <- c(1, 1)   # x padding (left, right) per panel, as fraction of data range

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

p <- ggplot(pd, aes(y = y_position)) +
  geom_errorbarh(aes(xmin = CI_99_lower, xmax = CI_99_upper, color = label),
                 height = 0.25, linewidth = 1.2, alpha = 0.3) +
  geom_errorbarh(aes(xmin = CI_95_lower, xmax = CI_95_upper, color = label),
                 height = 0.20, linewidth = 0.9, alpha = 0.5) +
  geom_errorbarh(aes(xmin = CI_90_lower, xmax = CI_90_upper, color = label),
                 height = 0.15, linewidth = 0.7, alpha = 0.8) +
  geom_point(aes(x = emmean, color = label), size = 3, alpha = 0.9) +
  facet_wrap(~ regime, nrow = 1, scales = "free_x") +
  scale_color_manual(values = bias_colors, breaks = lab_top2bottom) +
  guides(color = guide_legend(nrow = 1)) +
  scale_y_continuous(breaks = 5:1, labels = lab_top2bottom,
                     expand = expansion(add = c(0.4, 0.4))) +
  # XEXP = proportional padding (left, right) inside every panel — room for
  # hand-added significance marks; raise/lower the two numbers to taste
  scale_x_continuous(labels = label_number(accuracy = 0.01),
                     n.breaks = 4,
                     expand = expansion(mult = XEXP)) +
  labs(x = "Post-interaction GRS F (lower = closer to the frontier)", y = NULL) +
  nature_theme +
  theme(
    legend.position = "bottom", legend.justification = "center",
    legend.title = element_blank(), legend.text = element_text(size = 9),
    strip.background = element_blank(),
    strip.text = element_text(family = "Avenir", size = 8),  # regime labels; blank to self-label
    panel.spacing = unit(1.2, "lines"),
    axis.text.y = element_text(angle = 90, hjust = 0.5)
  )
print(p)
ggsave(file.path(FIG_DIR, "grs_treatment_by_regime.png"), p,
       width = 5.5, height = 4.95, dpi = 500)
cat(sprintf("\nSaved grs_treatment_by_regime.png in %s\n", SCRIPT_DIR))
cat("NOTE: x scales are free across panels (F units differ mechanically with\n")
cat("the frontier estimation window); compare ARM ORDERING within panels, not\n")
cat("F levels across panels.\n")
