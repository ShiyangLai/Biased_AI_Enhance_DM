# ==============================================================================
# active_m2_treatment_bull_overlay.R
# Per-wave treatment ladder (as plot_active_m2_treatment.R, 3 panels) with the
# BULL-COUNTERFACTUAL Somewhat/Extremely Risk-Seeking estimates overlaid in
# HALF-TRANSPARENT color in the WAVE 2 and WAVE 3 panels (the defensive waves).
#
# Realized (per wave):  post_active_m2_ann ~ ai_group + pre_active_m2_ann  (HC3)
# Bull counterfactual:  post_m2_bull       ~ ai_group + pre_m2_bull        (HC3)
#   (bucket-conditional outcome from counterfactual_regime_m2.py; model fitted
#    on all 5 arms within the wave; the two seeking arms PLUS Default are drawn
#    -- Default as the within-regime benchmark for the seeking contrasts)
# Transparent ladders sit just below their realized counterparts, same colors.
#
# LIMITATION (asymmetric behavioral bounding): the counterfactual holds
# DECISIONS fixed and varies only the market draw. The realized (defensive-
# context) portfolios already embed users' context-sensitive resistance to
# seeking advice (bounded compliance), which damped the seeking arms' realized
# harm. Whether AVERSE advice would be symmetrically resisted in a TRUE bull
# market -- bounding its counterfactual underperformance the same way -- is a
# decision-time behavioral response that this simulation cannot reveal: the
# portfolios were never formed under bull-context sentiment.
#
# Output: active_m2_treatment_bull_overlay.png
# Inputs: active_m2_treatment_data.csv , counterfactual_regime_m2.csv
#   setwd("investment/code"); source("_setup.R"); source("active_m2_treatment_bull_overlay.R")
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
SEEK <- c("Somewhat Risk-Seeking", "Extremely Risk-Seeking")

df <- rd("active_m2_treatment_data.csv") %>%
  filter(ai_group %in% ARMS) %>%
  left_join(rd("counterfactual_regime_m2.csv") %>%
              select(participantId, pre_m2_bull, post_m2_bull),
            by = "participantId") %>%
  mutate(ai_group = factor(ai_group, levels = ARMS), wave = as.character(wave)) %>%
  filter(!is.na(post_active_m2_ann), !is.na(pre_active_m2_ann))

# ── per-wave emmeans (HC3), realized + bull-counterfactual ────────────────────
emm_wave <- function(data, post_col, pre_col) {
  dd <- data %>% rename(post = all_of(post_col), pre = all_of(pre_col)) %>%
    filter(!is.na(post), !is.na(pre)) %>% droplevels()
  mo <- lm(post ~ ai_group + pre, data = dd)
  as.data.frame(summary(emmeans(mo, ~ ai_group, vcov. = vcovHC(mo, type = "HC3"))))
}
waves <- sort(unique(df$wave))
pd_real <- do.call(rbind, lapply(waves, function(w) {
  es <- emm_wave(df[df$wave == w, ], "post_active_m2_ann", "pre_active_m2_ann")
  es$wave <- w; es }))
pd_bull <- do.call(rbind, lapply(c("wave2", "wave3"), function(w) {
  es <- emm_wave(df[df$wave == w, ], "post_m2_bull", "pre_m2_bull")
  es$wave <- w; es })) %>%
  filter(as.character(ai_group) %in% c("Default", SEEK))
# Default is included as the WITHIN-REGIME benchmark: comparing the bull-
# counterfactual seeking arms with the realized Default would conflate the
# regime shift with the treatment effect.

cat("=== realized per-wave emmeans (HC3) ===\n")
print(pd_real[, c("wave", "ai_group", "emmean", "SE")], row.names = FALSE, digits = 3)
cat("\n=== bull-counterfactual per-wave emmeans, seeking arms (HC3) ===\n")
print(pd_bull[, c("wave", "ai_group", "emmean", "SE")], row.names = FALSE, digits = 3)

# ── plot data (main-figure conventions) ───────────────────────────────────────
YORDER <- c("Extremely Risk-Averse" = 5, "Somewhat Risk-Averse" = 4,
            "Default" = 3, "Somewhat Risk-Seeking" = 2, "Extremely Risk-Seeking" = 1)
SHORT  <- c("Extremely Risk-Averse" = "Ext.\nAverse", "Somewhat Risk-Averse" = "Swt.\nAverse",
            "Default" = "Default",
            "Somewhat Risk-Seeking" = "Swt.\nSeeking", "Extremely Risk-Seeking" = "Ext.\nSeeking")
bias_colors <- c("Ext.\nAverse"="#1B7837","Swt.\nAverse"="#7FBF7B",
                 "Default"="#999999","Swt.\nSeeking"="#AF8DC3","Ext.\nSeeking"="#762A83")
lab_top2bottom <- unname(SHORT[names(sort(YORDER, decreasing = TRUE))])

add_ci <- function(es, y_off = 0) es %>%
  mutate(arm = as.character(ai_group), y_position = YORDER[arm] - y_off,
         label = SHORT[arm],
         CI_90_lower = emmean - SE*qnorm(0.95),  CI_90_upper = emmean + SE*qnorm(0.95),
         CI_95_lower = emmean - SE*qnorm(0.975), CI_95_upper = emmean + SE*qnorm(0.975),
         CI_99_lower = emmean - SE*qnorm(0.995), CI_99_upper = emmean + SE*qnorm(0.995))
pd_real <- add_ci(pd_real)
pd_bull <- add_ci(pd_bull, y_off = 0.42)      # transparent row just below realized

# realized = solid whiskers + filled circle; counterfactual = dashed whiskers +
# hollow diamond (white core, colored rim) + transparency: three cues, no legend
# a = point alpha multiplier; a_w = whisker alpha multiplier (kept high for the
# counterfactual so its narrow CIs stay visible next to the smaller marker).
# Whiskers are SOLID in both layers; the counterfactual is distinguished by the
# hollow diamond marker + transparency.
ladder <- function(data, a, a_w = a, marker = "circle") { sl <- (marker == "circle"); list(
  geom_errorbarh(data = data, aes(xmin = CI_99_lower, xmax = CI_99_upper, color = label),
                 height = 0.25, linewidth = 1.2, alpha = 0.3 * a_w, show.legend = sl),
  geom_errorbarh(data = data, aes(xmin = CI_95_lower, xmax = CI_95_upper, color = label),
                 height = 0.20, linewidth = 0.9, alpha = 0.5 * a_w, show.legend = sl),
  geom_errorbarh(data = data, aes(xmin = CI_90_lower, xmax = CI_90_upper, color = label),
                 height = 0.15, linewidth = 0.7, alpha = 0.8 * a_w, show.legend = sl),
  if (sl)
    geom_point(data = data, aes(x = emmean, color = label), size = 3, alpha = 0.9 * a)
  else
    geom_point(data = data, aes(x = emmean, color = label), shape = 23, fill = "white",
               size = 1.7, stroke = 0.9, alpha = 0.95 * a, show.legend = FALSE)) }

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

p <- ggplot(mapping = aes(y = y_position)) +
  ladder(pd_real, a = 1) +
  ladder(pd_bull, a = 0.65, a_w = 1, marker = "diamond") +  # ghost marker, solid CIs
  facet_wrap(~ wave, nrow = 1,
             labeller = as_labeller(function(x) gsub("wave", "Wave ", x))) +
  scale_color_manual(values = bias_colors, breaks = lab_top2bottom) +
  guides(color = guide_legend(nrow = 1)) +
  scale_y_continuous(breaks = 5:1, labels = lab_top2bottom,
                     expand = expansion(add = c(0.75, 0.4))) +
  scale_x_continuous(labels = label_number(accuracy = 0.001)) +
  labs(x = expression("Post-interaction Active M"^2), y = NULL) +
  nature_theme +
  xlim(-0.06, 0.06) +
  theme(
    legend.position = "bottom", legend.justification = "center",
    legend.title = element_blank(), legend.text = element_text(size = 9),
    strip.background = element_blank(),
    strip.text = element_blank(),
    panel.spacing = unit(1.2, "lines"),
    axis.text.y = element_text(angle = 90, hjust = 0.5, size = 8.8)
  )
print(p)
ggsave(file.path(FIG_DIR, "active_m2_treatment_bull_overlay.png"), p,
       width = 5, height = 4.95, dpi = 500)
cat(sprintf("\nSaved active_m2_treatment_bull_overlay.png in %s\n", SCRIPT_DIR))
cat("Half-transparent ladders (waves 2-3, below the seeking rows) = the same\n")
cat("arms under the BULL counterfactual (post-hoc; portfolios fixed).\n")
