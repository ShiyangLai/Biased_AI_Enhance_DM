# ==============================================================================
# bias_side_counterfactual.R
# Echo-chamber vs opposition bias (single-AI biased arms, as
# bias_side_performance.R) evaluated on TWO outcome regimes:
#   REALIZED market  : post_active_m2_ann      (the actual defensive 2026 windows)
#   BULL counterfactual: post_m2_bull          (same fixed portfolios re-scored on
#                        the top-tercile SPY windows of the pinned pre-study
#                        history; from counterfactual_regime_m2.py)
#
#   BiasSide = Same (echo-chamber) / Opposite (opposition)  vs the participant's
#   risk lean (median split of risk_pref_score, computed on this sample).
#
# Model (per outcome, pre-registered structure as bias_side_performance.R):
#     post ~ BiasSide * lean + wave + pre        (HC3; pre matched to outcome)
#
# Figure: ONE horizontal grouped bar plot — y = bias side (2 groups), 4 dodged
# bars each: lean (fill: Averse green / Seeking purple) x market (alpha: solid =
# realized, semi-transparent = bull counterfactual). 95% CI whiskers.
#
# CAVEAT: bull outcome is post-hoc/exploratory (see counterfactual_bull_m2.R);
# the realized analysis is the primary one (bias_side_performance.R).
# Inputs: active_m2_treatment_data.csv , participant_covariates.csv,
#         counterfactual_regime_m2.csv (see code/upstream/)
#   setwd("investment/code"); source("_setup.R"); source("bias_side_counterfactual.R")
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
cf  <- rd("counterfactual_regime_m2.csv")

d <- m2 %>%
  filter(ai_group %in% c("Extremely Risk-Averse", "Somewhat Risk-Averse",
                         "Extremely Risk-Seeking", "Somewhat Risk-Seeking")) %>%   # biased only
  left_join(cov %>% select(participantId, risk_pref_score), by = "participantId") %>%
  left_join(cf  %>% select(participantId, pre_m2_bull, post_m2_bull), by = "participantId") %>%
  filter(!is.na(risk_pref_score),
         !is.na(post_active_m2_ann), !is.na(pre_active_m2_ann),
         !is.na(post_m2_bull),       !is.na(pre_m2_bull))

# ── Default-arm reference (pre-registered arm model on the 5-arm sample) ─────
# post ~ ai_group + wave + pre (HC3) on Default + 4 biased arms; the Default
# emmean per regime becomes a dashed benchmark line in the figure.
d5 <- m2 %>%
  filter(ai_group %in% c("Default", "Extremely Risk-Averse", "Somewhat Risk-Averse",
                         "Extremely Risk-Seeking", "Somewhat Risk-Seeking")) %>%
  left_join(cf %>% select(participantId, pre_m2_bull, post_m2_bull), by = "participantId") %>%
  filter(!is.na(post_active_m2_ann), !is.na(pre_active_m2_ann),
         !is.na(post_m2_bull), !is.na(pre_m2_bull)) %>%
  mutate(ai_group = factor(ai_group), wave = factor(wave))
def_ref <- function(post_col, pre_col) {
  dd <- d5 %>% rename(post = all_of(post_col), pre = all_of(pre_col))
  mo <- lm(post ~ ai_group + wave + pre, data = dd)
  es <- summary(emmeans(mo, ~ ai_group, vcov. = vcovHC(mo, type = "HC3")))
  es$emmean[es$ai_group == "Default"]
}
def_real <- def_ref("post_active_m2_ann", "pre_active_m2_ann")
def_bull <- def_ref("post_m2_bull",       "pre_m2_bull")
cat(sprintf("\nDefault-arm adjusted mean: realized %.4f | bull counterfactual %.4f\n",
            def_real, def_bull))

thr <- median(d$risk_pref_score, na.rm = TRUE)
d <- d %>% mutate(
  lean   = ifelse(risk_pref_score >= thr, "Seeking", "Averse"),
  ai_dir = ifelse(grepl("Averse", ai_group), "Averse", "Seeking"),
  BiasSide = factor(ifelse(ai_dir == lean, "Same", "Opposite"),
                    levels = c("Same", "Opposite")),
  lean = factor(lean, levels = c("Averse", "Seeking")),
  wave = factor(wave))
cat(sprintf("N (biased arms, both outcomes; risk_pref median split at %.3f): %d\n",
            thr, nrow(d)))
cat("\nBiasSide x lean cell sizes:\n"); print(table(d$lean, d$BiasSide))

# ── fit the bias-side model on each outcome regime ────────────────────────────
fit_side <- function(post_col, pre_col, label) {
  dd <- d %>% rename(post = all_of(post_col), pre = all_of(pre_col))
  mi <- lm(post ~ BiasSide * lean + wave + pre, data = dd)
  Vi <- vcovHC(mi, type = "HC3")
  cat(sprintf("\n=== %s: post ~ BiasSide * lean + wave + pre (HC3) ===\n", label))
  ci <- coeftest(mi, vcov = Vi)
  print(ci[grep("BiasSide|lean", rownames(ci)), , drop = FALSE])
  emmi <- emmeans(mi, ~ BiasSide | lean, vcov. = Vi)
  cat("--- Same - Opposite within each lean (FDR) ---\n")
  print(summary(contrast(emmi, "pairwise", by = "lean", adjust = "fdr"), infer = TRUE),
        digits = 3)
  as.data.frame(summary(emmeans(mi, ~ BiasSide * lean, vcov. = Vi))) %>%
    rename(estimate = emmean, lo = lower.CL, hi = upper.CL) %>%
    mutate(market = label)
}
pd <- bind_rows(
  fit_side("post_active_m2_ann", "pre_active_m2_ann", "Real"),
  fit_side("post_m2_bull",       "pre_m2_bull",       "Bull counterfactual")) %>%
  mutate(BiasSide = factor(as.character(BiasSide), levels = c("Opposite", "Same")),
         lean     = factor(as.character(lean), levels = c("Averse", "Seeking")),
         market   = factor(market, levels = c("Real", "Bull counterfactual")),
         grp      = interaction(lean, market, sep = " | "))

# ── figure: ONE horizontal grouped bar plot ───────────────────────────────────
# y = bias side; 4 dodged bars per group: lean (fill) x market (alpha).
lean_col <- c("Averse" = "#59A14F", "Seeking" = "#B07AA1")
# dodge order top->bottom within group: realized pair (Averse, Seeking) first,
# then counterfactual pair (Averse, Seeking)
pd$grp <- factor(pd$grp, levels = c("Seeking | Bull counterfactual",
                                    "Averse | Bull counterfactual",
                                    "Seeking | Real",
                                    "Averse | Real"))
dodge <- position_dodge(width = 0.82)
xpad  <- 0.1 * diff(range(c(pd$lo, pd$hi)))
# value labels beyond the CI whisker, on the bar's own side
pd <- pd %>% mutate(lab_x = ifelse(estimate >= 0, hi + xpad * 0.45, lo - xpad * 0.45),
                    lab_h = ifelse(estimate >= 0, 0, 1))

p <- ggplot(pd, aes(y = BiasSide, x = estimate, group = grp)) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.4) +
  geom_vline(xintercept = def_real,                        # Default-AI benchmark:
             linetype = "dashed", color = "grey70",        # solid-ish = realized,
             linewidth = 0.5) +                           # faint = bull counterfactual
  geom_vline(xintercept = def_bull,
             linetype = "dashed", color = "grey70", linewidth = 0.5, alpha = 0.4) +
  ggpattern::geom_col_pattern(                # diagonal stripes mark the
    aes(fill = lean, alpha = market,          # bull-counterfactual bars
        pattern = market),
    position = dodge, width = 0.72, color = NA,
    pattern_angle = 135, pattern_colour = NA, pattern_fill = "grey25",
    pattern_density = 0.12, pattern_spacing = 0.025, pattern_alpha = 0.55,
    pattern_key_scale_factor = 0.55) +
  ggpattern::scale_pattern_manual(values = c("Real" = "none",
                                             "Bull counterfactual" = "stripe"),
                                  name = NULL) +
  geom_errorbar(aes(xmin = lo, xmax = hi), position = dodge,
                width = 0.2, linewidth = 0.6, color = "black") +
  geom_text(aes(x = lab_x, label = round(estimate, 3), hjust = lab_h * 1.1),
            position = dodge, family = "Avenir", size = 4, color = "black") +
  scale_fill_manual(values = lean_col, name = "Participant\nrisk lean") +
  scale_alpha_manual(values = c("Real" = 1, "Bull counterfactual" = 0.35),
                     name = NULL) +
  scale_y_discrete(limits = c("Same", "Opposite"),   # Opposition Bias on top
                   labels = c("Opposite" = "Opposition Bias", "Same" = "Echo-Chamber Bias")) +
  # limits consolidated here (a separate xlim() call would replace this scale):
  # right edge per the chosen +/-0.045 framing; left extended so the outermost
  # value label is not clipped at the panel border.
  scale_x_continuous(labels = number_format(accuracy = 0.01),
                     breaks = c(-0.1, -0.08, -0.06, -0.04, -0.02, 0, 0.02, 0.04, 0.06, 0.08, 0.1),
                     limits = c(-0.1, 0.1)) +
  labs(y = "AI Bias Direction", x = expression("Post-interaction Active M"^2)) +
  guides(fill = "none", alpha = "none",
         pattern = guide_legend(override.aes = list(fill = "grey50", alpha = 1),
                                ncol = 1)) +
  theme_classic() +
  theme(
    text = element_text(family = "Avenir"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.line = element_blank(),
    axis.title.x = element_text(family = "Avenir", size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(family = "Avenir", size = 12, color = "black", margin = margin(r = 12)),
    axis.text.x = element_text(family = "Avenir", size = 10, color = "black"),
    axis.text.y = element_text(family = "Avenir", size = 10, color = "black", hjust = 0.5, angle = 90),
    axis.ticks = element_line(color = "black", linewidth = 0.4),
    axis.ticks.length = unit(3, "pt"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    plot.margin = margin(t = 15, r = 20, b = 15, l = 15),
    legend.position = c(0.97, 0.99),          # inside panel, upper-right corner
    legend.justification = c(1, 1),
    legend.background = element_blank(),
    legend.key.size = unit(12, "pt"),
    legend.title = element_blank(),
    legend.text = element_text(family = "Avenir", size = 11)
  )
print(p)

ggsave(file.path(FIG_DIR, "bias_side_real_vs_bull.png"), p,
       width = 5, height = 5, dpi = 500)
cat(sprintf("\nSaved bias_side_real_vs_bull.png in %s\n", SCRIPT_DIR))
cat("Solid = realized market; semi-transparent = bull counterfactual (post-hoc).\n")
