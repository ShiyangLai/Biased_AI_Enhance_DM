# ==============================================================================
# ai_correctness_by_arm.R
# Five-arm breakdown of the AI-correctness density panel in second_figure_b2.R.
#
#   second_figure_b2.R          ->  this script
#   BiasedCat (Default/Mod/Strong)  ->  arm: Extreme Republican / Somewhat
#                                       Republican / Default / Somewhat Democrat
#                                       / Extreme Democrat (Neutral excluded)
#
# Magnitude pools the two bias DIRECTIONS, so a Republican-side and a
# Democrat-side effect of opposite sign can cancel within "Moderate" or
# "Strong". This splits them out, following the structure of
# notebooks/R/ai_advice_by_bias_magnitude.R (its 5-arm distribution section).
#
# Model spec is unchanged from second_figure_b2.R: AICorrectness ~ arm +
# as.factor(NID), UID-clustered SEs, emmeans with FDR-adjusted pairwise tests.
#
# Outputs: Images/ai_correctness_density_five_arm.png
# Run:  Rscript Code/ai_correctness_by_arm.R
# ==============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(ggplot2); library(scales)
  library(sandwich); library(lmtest); library(emmeans)
})

PROJ <- ".."
# Base data: single_ai_processed comes from preprcessing.R
if (!exists("single_ai_processed")) {
  cat("single_ai_processed not found - sourcing preprcessing.R\n")
  source("preprcessing.R")
}

# Raw AIStanceLabel values in the data; ARM_LABELS is how they are displayed
# ("Strong" is reported as "Extreme" throughout the manuscript).
ARMS <- c("Strong Republican", "Somewhat Republican", "Default",
          "Somewhat Democrat", "Strong Democrat")
ARM_LABELS <- c("Extreme Republican", "Somewhat Republican", "Default",
                "Somewhat Democrat", "Extreme Democrat")

# Neutral-treatment rows excluded, as elsewhere in the pipeline. Filtering on
# ARMS drops "Politically Neutral" explicitly rather than relying on an
# upstream object that may already be in the session.
d <- single_ai_processed %>%
  dplyr::filter(AIStanceLabel %in% ARMS, !is.na(AICorrectness)) %>%
  mutate(arm = factor(AIStanceLabel, levels = ARMS, labels = ARM_LABELS))

cat("N by arm (rows):\n"); print(table(d$arm))
cat("participants:", length(unique(d$UID)), "\n")

# ── model: same spec as second_figure_b2.R, five levels instead of three ──────
m <- lm(AICorrectness ~ arm + as.factor(NID), data = d)
V <- vcovCL(m, cluster = d$UID)
cat("\n=== AICorrectness ~ arm + NID (UID-clustered SEs) ===\n")
print(coeftest(m, vcov = V)[1:length(ARMS), , drop = FALSE])

emm <- emmeans(m, ~ arm, vcov. = V)
cat("\n=== MARGINAL MEANS BY ARM ===\n")
print(as.data.frame(summary(emm)), row.names = FALSE, digits = 4)

cat("\n=== PAIRWISE COMPARISONS (FDR across 10) ===\n")
pw <- as.data.frame(summary(pairs(emm, adjust = "fdr"), infer = TRUE))
print(pw[, c("contrast", "estimate", "lower.CL", "upper.CL", "t.ratio", "p.value")],
      row.names = FALSE, digits = 3)

cat("\n=== vs Default (FDR across 4) ===\n")
pwd <- as.data.frame(summary(
  contrast(emm, method = "trt.vs.ctrl", ref = which(ARM_LABELS == "Default"),
           adjust = "fdr"), infer = TRUE))
print(pwd[, c("contrast", "estimate", "lower.CL", "upper.CL", "p.value")],
      row.names = FALSE, digits = 3)

# ── raw distribution by arm ──────────────────────────────────────────────────
sstats <- d %>% group_by(arm) %>%
  summarise(n = n(), mean = mean(AICorrectness), sd = sd(AICorrectness),
            median = median(AICorrectness), .groups = "drop")
cat("\n=== AI correctness: raw distribution by arm ===\n")
print(as.data.frame(sstats), row.names = FALSE, digits = 3)

cat("--- Kolmogorov-Smirnov vs Default (FDR across 4) ---\n")
ks <- do.call(rbind, lapply(setdiff(ARM_LABELS, "Default"), function(g) {
  kt <- suppressWarnings(ks.test(d$AICorrectness[d$arm == g],
                                 d$AICorrectness[d$arm == "Default"]))
  data.frame(arm = g, D = unname(kt$statistic), p = kt$p.value)
}))
ks$p_fdr <- p.adjust(ks$p, "fdr")
print(ks, row.names = FALSE, digits = 3)

# ── figure: five densities, diverging red-grey-blue by arm ───────────────────
arm_colors <- c("Extreme Republican"  = "#B2182B",
                "Somewhat Republican" = "#EF8A62",
                "Default"             = "#999999",
                "Somewhat Democrat"   = "#67A9CF",
                "Extreme Democrat"    = "#2166AC")

base_theme <- theme_classic() +
  theme(
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.line = element_blank(),
    text = element_text(family = "Avenir", color = "black"),
    axis.title.x = element_text(family = "Avenir", size = 13.5, margin = margin(t = 12)),
    axis.title.y = element_text(family = "Avenir", size = 13.5, margin = margin(r = 10)),
    axis.text.x = element_text(family = "Avenir", size = 11.7, color = "black",
                               margin = margin(t = 4)),
    axis.text.y = element_text(family = "Avenir", size = 11.7, color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    axis.ticks.length = unit(3.5, "pt"),
    panel.grid = element_blank(),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10))

p_dist <- ggplot(d, aes(x = AICorrectness, color = arm, fill = arm)) +
  geom_density(alpha = 0.25, linewidth = 0.7) +
  geom_vline(data = sstats, aes(xintercept = mean, color = arm),
             linetype = "solid", linewidth = 0.9, alpha = 0.9) +
  scale_fill_manual(values = arm_colors, name = "") +
  scale_color_manual(values = arm_colors, name = "") +
  scale_x_continuous(name = "AI Fact-checking Correctness",
                     expand = expansion(mult = c(0, 0)),
                     labels = number_format(accuracy = 0.01)) +
  scale_y_continuous(name = "Probability Density",
                     expand = expansion(mult = c(0, 0.05)),
                     labels = number_format(accuracy = 0.1)) +
  base_theme +
  # base_theme sets legend.position = "none"; re-enable it inside the panel.
  # Extra right margin: x expansion is 0, so the "1.00" tick label sits half
  # outside the panel and is clipped by base_theme's 10 pt right margin.
  theme(plot.margin = margin(t = 10, r = 20, b = 10, l = 10),
        legend.position = c(0.02, 0.98),
        legend.justification = c(0, 1),
        # transparent, so the curves running under the legend stay visible
        legend.background = element_rect(fill = NA, color = NA),
        legend.margin = margin(4, 4, 4, 4),
        legend.key = element_rect(fill = NA, color = NA),
        legend.key.height = unit(0.42, "cm"),
        legend.text = element_text(family = "Avenir", size = 9.5))

if (interactive()) print(p_dist)

out <- file.path(PROJ, "figures", "ai_correctness_density_five_arm.png")
ragg::agg_png(out, width = 5.02, height = 4.3, units = "in", res = 500)
print(p_dist); dev.off()
cat("\nSaved:", out, "\n")
