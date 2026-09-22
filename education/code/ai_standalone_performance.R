# ==============================================================================
# ai_standalone_performance.R — Extended Data Fig. 6c: stand-alone task performance of the
# AI assistants by bias condition. Classroom analog of the AI-advice density
# panel in notebooks/R/ai_advice_by_bias_magnitude.R, whose styling this script
# follows exactly (geom_density + per-arm mean vlines, base_theme, inset legend,
# KS tests vs Default with FDR; 5.2 x 4.3 in at dpi 500).
#
#   investment                    ->  classroom
#   ai_m2 of the assistant's own  ->  stand-alone assignment grade: the three-
#     recommended portfolio           rater composite (two human graders +
#                                     Gemini, equal weight; Cronbach's alpha
#                                     0.770, against 0.613 for the humans alone)
#   arm = risk direction x        ->  arm = Novelty / Default / Neutralized /
#     magnitude, Default centre       Reliability; questioning and
#                                     neutralized excluded
#   one row per participant       ->  one row per (week, condition), 8-9 per arm
#
# SCALE. The two human passes graded disjoint weeks on different scales (A took
# weeks 1/2/7/8/9, B weeks 3/4/5/6, C all nine), so each rater series is
# standardised before averaging -- A's and B's weeks separately -- and the
# composite is then anchored back onto a 0-10-equivalent axis using the pooled
# rater mean and SD. The axis therefore reads like a grade but is not a grade any
# single rater assigned.
#
# CAVEAT THE CAPTION MUST CARRY: the investment panel draws hundreds of
# participants per arm; this one draws 8-9 course weeks per arm, so the curves
# are kernel densities over a handful of points and their width is set by the
# bandwidth rather than by sampling.
#
# Input  : ai_standalone_grades.csv (from the collate/merge step)
# Output : ai_standalone_performance.png
#   setwd("education/code"); source("_setup.R"); source("ai_standalone_performance.R")
# ==============================================================================
suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(scales)
})


ARMS <- c("Novelty", "Default", "Reliability")   # Neutralized excluded from this panel
IN <- DATA_DIR; OUT <- FIG_DIR
d <- read.csv(file.path(IN, "ai_standalone_grades.csv"),
              stringsAsFactors = FALSE) %>%
  mutate(cell = factor(dplyr::recode(condition, default = "Default",
                                     neutral = "Neutralized", novelty = "Novelty",
                                     reliability = "Reliability"), levels = ARMS))
# anchor the standardised composite onto a 0-10-equivalent axis
anch <- unlist(d[, c("humanAB", "humanC", "gemini")])
d$score <- pmin(pmax(mean(anch) + d$composite * sd(anch), 0), 10)
# the anchor above is computed on every rater and row, so excluding the
# neutralized arm below shifts no other arm's score
d <- d[!is.na(d$cell), ]

sstats <- d %>% group_by(cell) %>%
  summarise(n = n(), mean = mean(score), sd = sd(score), median = median(score),
            .groups = "drop")
cat("=== stand-alone grade composite: raw distribution by arm ===\n")
print(as.data.frame(sstats), row.names = FALSE, digits = 3)

cat("--- Kolmogorov-Smirnov vs Default (FDR across 2) ---\n")
ks <- do.call(rbind, lapply(setdiff(levels(d$cell), "Default"), function(g) {
  k <- suppressWarnings(ks.test(d$score[d$cell == g], d$score[d$cell == "Default"]))
  data.frame(arm = g, D = unname(k$statistic), p = k$p.value) }))
ks$p_fdr <- p.adjust(ks$p, "fdr"); print(ks, row.names = FALSE, digits = 3)

# ── styling ported verbatim from ai_advice_by_bias_magnitude.R ────────────────
arm_colors <- c("Novelty" = "#01665E", "Default" = "#BAB0AC",
                "Neutralized" = "#79706E", "Reliability" = "#8C510A")

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

p_dist <- ggplot(d, aes(x = score, color = cell, fill = cell)) +
  # adjust = 1.8 widens R's default nrd0 bandwidth. The reference panel drew
  # hundreds of participants per arm, where the default is smooth; at 8-9 course
  # weeks per arm it resolves individual observations into spurious humps.
  geom_density(alpha = 0.25, linewidth = 0.7, adjust = 1.8) +
  geom_vline(data = sstats, aes(xintercept = mean, color = cell),
             linetype = "solid", linewidth = 0.9, alpha = 0.9) +
  scale_fill_manual(values = arm_colors, name = "") +
  scale_color_manual(values = arm_colors, name = "") +
  scale_x_continuous(name = "AI Standalone Assignment Grade",
                     expand = expansion(mult = c(0, 0)),
                     labels = number_format(accuracy = 1)) +
  scale_y_continuous(name = "Probability Density",
                     expand = expansion(mult = c(0, 0.05)),
                     labels = number_format(accuracy = 0.05)) +
  base_theme +
  # reference anchors the legend's RIGHT edge at x = 0.42; here the LEFT edge is
  # pinned just inside the panel frame instead
  theme(legend.position = c(0.02, 0.98),
        legend.justification = c(0, 1),
        legend.background = element_rect(fill = "white", color = NA),
        legend.margin = margin(4, 4, 4, 4),
        legend.key = element_rect(fill = "white", color = NA),
        legend.key.height = unit(0.42, "cm"),
        legend.text = element_text(family = "Avenir", size = 9.5))
print(p_dist)
save_png(file.path(OUT, "ai_standalone_performance.png"), p_dist,
         width = 5.2, height = 4.3)
cat(sprintf("\nSaved ai_standalone_performance.png in %s\n", OUT))
cat("Done: ai_standalone_performance.R\n")
