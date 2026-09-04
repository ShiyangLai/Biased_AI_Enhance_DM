# ============================================================================
# Single vs Dual AI — 5-arm comparison (STANDALONE)
#
# Reproduces, from the exported `five_arm_single_dual.csv`, the two analyses
# and figures from this project:
#   PART A  Post-interaction performance   (mirrors forth_figure_d1.R)
#   PART B  Perceived improvement (ordinal) (mirrors forth_figure_d2.R)
#
# Each part: model(s) -> estimated means -> vertical bar plot -> pairwise
# comparisons (FDR-adjusted) with Hedges' g.
#
# Usage:
#   1. Run export_five_arm_data.R once (it writes ../data/five_arm_single_dual.csv),
#      or set `data_path` to your own copy.
#   2. source("five_arm_analysis_standalone.R")   # or Rscript
#
# Packages: dplyr readr ggplot2 scales sandwich lmtest lme4 lmerTest emmeans
#           performance ordinal MCMCglmm
# Note: family = "Avenir" is a macOS font; change it if unavailable.
# ============================================================================

library(dplyr)
library(readr)
library(ggplot2)
library(scales)
library(sandwich)
library(lmtest)
library(lme4)
library(lmerTest)
library(emmeans)
library(performance)
library(ordinal)
library(MCMCglmm)
set.seed(123)

# ---- Load & prepare -------------------------------------------------------
data_path <- "../data/five_arm_single_dual.csv"
# as.data.frame(): MCMCglmm indexes its response with base data.frame
# semantics, and a tibble returns a one-column tibble rather than a vector.
dat <- as.data.frame(read_csv(data_path, show_col_types = FALSE))

# 5-arm order (Default arms first = model reference is Single_AI_Non_Biased)
arm_levels <- c("Single_AI_Non_Biased", "Single_AI_Biased",
                "Dual_AI_Non_Biased", "Dual_AI_Opposition", "Dual_AI_Balanced")
dat$ExperimentType <- factor(dat$ExperimentType, levels = arm_levels)

# Display labels ("Non-Biased" shown as "Default")
formal_label_map <- c(
  Single_AI_Non_Biased = "Single AI\nDefault",
  Single_AI_Biased     = "Single AI\nBiased",
  Dual_AI_Non_Biased   = "Dual AI\nDefault",
  Dual_AI_Opposition   = "Dual AI\nOpposition",
  Dual_AI_Balanced     = "Dual AI\nBalanced"
)
label_levels <- unname(formal_label_map[arm_levels])

# Perceived improvement -> ordered factor (ordinal outcome)
dat$PerceivedImproveCode <- factor(dat$PerceivedImproveCode, ordered = TRUE)

# ---- Shared plotting helpers ----------------------------------------------
nature_theme_vertical <- theme_classic() +
  theme(
    text = element_text(family = "Avenir", color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.line = element_blank(),
    axis.text.x = element_text(family = "Avenir", size = 9, color = "black", margin = margin(t = 8)),
    axis.text.y = element_text(family = "Avenir", size = 9, color = "black"),
    axis.title.x = element_text(family = "Avenir", size = 12, color = "black", margin = margin(t = 12)),
    axis.title.y = element_text(family = "Avenir", size = 12, color = "black", margin = margin(r = 15)),
    axis.ticks = element_line(color = "black", linewidth = 0.4),
    axis.ticks.length = unit(3, "pt"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    plot.margin = margin(t = 15, r = 30, b = 15, l = 15),
    legend.position = "none"
  )

# Value-based fill: darker = higher, given a 4-stop light->dark ramp
make_value_fill <- function(values, ramp4) {
  rng <- range(values); norm <- (values - rng[1]) / (rng[2] - rng[1])
  b <- c(0, 0.33, 0.66, 1.0)
  sapply(norm, function(v) {
    if (v <= b[2])       colorRampPalette(ramp4[1:2])(100)[round(v / b[2] * 99) + 1]
    else if (v <= b[3])  colorRampPalette(ramp4[2:3])(100)[round((v - b[2]) / (b[3] - b[2]) * 99) + 1]
    else                 colorRampPalette(ramp4[3:4])(100)[round((v - b[3]) / (b[4] - b[3]) * 99) + 1]
  })
}

sig_stars <- function(p) cut(p, c(-Inf, .001, .01, .05, .1, Inf),
                             c("***", "**", "*", "†", "ns"), right = FALSE)

# Hedges' g for an emmeans-style contrast label "A - B"
hedges_from_contrast <- function(contrast_label, cohens_d, n_by) {
  g <- strsplit(as.character(contrast_label), " - ")[[1]]
  n1 <- as.numeric(n_by[trimws(g[1])]); n2 <- as.numeric(n_by[trimws(g[2])])
  cohens_d * (1 - 3 / (4 * (n1 + n2 - 2) - 1))
}

emm_options(lmer.df = "satterthwaite", lmerTest.limit = 20000, pbkrtest.limit = 20000)

# ============================================================================
# PART A — POST-INTERACTION PERFORMANCE  (mirrors forth_figure_d1.R)
# ============================================================================
cat("\n########## PART A: PERFORMANCE ##########\n")

## OLS + cluster-robust SE
perf_lm   <- lm(PostPerformance ~ ExperimentType + PrePerformance +
                  as.factor(NID) + as.factor(UStanceLabel) + UIdeo + AICorrectness,
                data = dat, na.action = na.omit)
perf_vcov <- vcovCL(perf_lm, cluster = dat$UID)
print(coeftest(perf_lm, vcov = perf_vcov))
cat("OLS adjusted R^2:", round(summary(perf_lm)$adj.r.squared, 4), "\n")

## Mixed effects
perf_lmer <- lmer(PostPerformance ~ ExperimentType + PrePerformance +
                    as.factor(NID) + as.factor(UStanceLabel) + (1 | UID),
                  data = dat, na.action = na.omit)
cat("Residual SD:", round(sigma(perf_lmer), 4), "\n")
print(performance::r2(perf_lmer))

## Estimated marginal means
perf_emm <- emmeans(perf_lmer, ~ ExperimentType)
perf_plot_data <- as.data.frame(perf_emm) %>%
  rename(estimate = emmean, lower_ci = lower.CL, upper_ci = upper.CL) %>%
  mutate(formal_label = factor(formal_label_map[as.character(ExperimentType)], levels = label_levels),
         fill_color   = make_value_fill(estimate, c("#BBE5BB", "#66C266", "#228B22", "#006400")))

## Vertical bar plot (green family, like d1)
yA <- range(c(perf_plot_data$lower_ci, perf_plot_data$upper_ci))
p_performance <- ggplot(perf_plot_data, aes(x = formal_label, y = estimate)) +
  geom_col(aes(fill = fill_color), color = "black", width = 0.65, linewidth = 0) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.3, linewidth = 0.5, color = "black") +
  geom_text(aes(y = upper_ci + diff(yA) * 0.05, label = round(estimate, 3)),
            vjust = 0, family = "Avenir", size = 3) +
  geom_point(size = 3, shape = 20, color = "black") +
  scale_fill_identity() +
  scale_y_continuous(labels = number_format(accuracy = 0.01), expand = c(0, 0)) +
  coord_cartesian(ylim = c(0.55, 0.75)) +   # widen if bars/CIs clip
  labs(x = "Experimental Condition", y = "Post-Interaction Performance") +
  nature_theme_vertical
print(p_performance)

## Pairwise (FDR) + Hedges' g
n_by <- table(dat$ExperimentType)
perf_pairs <- as.data.frame(pairs(perf_emm, adjust = "fdr", infer = TRUE)) %>%
  mutate(
    Cohens_d = estimate / sigma(perf_lmer),
    Hedges_g = mapply(hedges_from_contrast, contrast, Cohens_d, MoreArgs = list(n_by = n_by)),
    Significance = sig_stars(p.value)
  )
cat("\n=== PERFORMANCE: pairwise comparisons (FDR) + Hedges' g ===\n")
print(perf_pairs[, c("contrast", "estimate", "SE", "p.value", "Hedges_g", "Significance")],
      row.names = FALSE, digits = 3)

# ============================================================================
# PART B — PERCEIVED IMPROVEMENT (ordinal)  (mirrors forth_figure_d2.R)
# ============================================================================
cat("\n########## PART B: PERCEIVED IMPROVEMENT ##########\n")

# MCMCglmm hard-errors on NA in ANY fixed predictor, not just AICorrectness.
dat_pi <- dat[complete.cases(dat[, c("PerceivedImproveCode", "ExperimentType",
                                     "NID", "PrePerformance", "UIdeo",
                                     "AICorrectness", "UStanceLabel", "UID")]), ]

## Cumulative link model + cluster-robust SE
pi_clm <- clm(PerceivedImproveCode ~ ExperimentType + as.factor(NID) + PrePerformance +
                UStanceLabel + AICorrectness + UIdeo, data = dat_pi, na.action = na.omit)
print(coeftest(pi_clm, vcov = vcovCL(pi_clm, cluster = dat_pi$UID)))

## Bayesian ordinal model (MCMCglmm) for marginal means on the latent scale
# Seed immediately before the chain: the set.seed() at the top of the script is
# consumed by Part A, so the RNG state here would otherwise depend on what ran
# before it.
set.seed(123)
pi_mcmc <- MCMCglmm(PerceivedImproveCode ~ ExperimentType + as.factor(NID) +
                      PrePerformance + UIdeo + AICorrectness + as.factor(UStanceLabel),
                    random = ~ UID, family = "ordinal",
                    nitt = 25000, thin = 10, burnin = 5000, data = dat_pi)
print(summary(pi_mcmc))

## Reconstruct marginal means per arm from the posterior (intercept + treatment
## + PrePerformance at its mean + NID at its mode). The shared terms cancel in
## pairwise differences, so difference CIs are tighter than the per-arm CIs.
post <- pi_mcmc$Sol; cols <- colnames(post)
mean_pre <- mean(dat_pi$PrePerformance, na.rm = TRUE)
mode_nid <- names(sort(table(dat_pi$NID), decreasing = TRUE))[1]

marginal_draws <- function(arm) {
  lp <- if ("(Intercept)" %in% cols) post[, "(Intercept)"] else rep(0, nrow(post))
  if (arm != arm_levels[1]) { cn <- paste0("ExperimentType", arm); if (cn %in% cols) lp <- lp + post[, cn] }
  if ("PrePerformance" %in% cols) lp <- lp + post[, "PrePerformance"] * mean_pre
  nc <- paste0("as.factor(NID)", mode_nid); if (nc %in% cols) lp <- lp + post[, nc]
  lp
}
mm <- sapply(arm_levels, marginal_draws)          # draws x arms

pi_plot_data <- data.frame(
  ExperimentType = arm_levels,
  posterior_mean = colMeans(mm),
  lower_ci = apply(mm, 2, quantile, 0.025),
  upper_ci = apply(mm, 2, quantile, 0.975)
) %>% mutate(
  formal_label = factor(formal_label_map[ExperimentType], levels = label_levels),
  fill_color   = make_value_fill(posterior_mean, c("#D6C0E5", "#B88BD8", "#9370DB", "#4B0082"))
)

## Vertical bar plot (indigo family, like d2)
yB <- range(c(pi_plot_data$lower_ci, pi_plot_data$upper_ci))
p_perceived <- ggplot(pi_plot_data, aes(x = formal_label, y = posterior_mean)) +
  geom_col(aes(fill = fill_color), color = "black", width = 0.7, linewidth = 0) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.3, linewidth = 0.5, color = "black") +
  geom_text(aes(y = upper_ci + diff(yB) * 0.05, label = round(posterior_mean, 3)),
            vjust = 0, family = "Avenir", size = 3) +
  geom_point(size = 2, shape = 17, color = "black") +
  scale_fill_identity() +
  scale_y_continuous(labels = number_format(accuracy = 0.01), expand = c(0, 0)) +
  labs(x = "Experimental Condition", y = "Perceived Improvement (latent scale)") +
  nature_theme_vertical
print(p_perceived)

## Pairwise posterior differences + FDR + Hedges' g
sd_obs <- sd(as.numeric(dat_pi$PerceivedImproveCode), na.rm = TRUE)
rows <- list(); k <- 0
for (i in 1:(length(arm_levels) - 1)) for (j in (i + 1):length(arm_levels)) {
  k <- k + 1
  d  <- mm[, i] - mm[, j]
  n1 <- sum(dat_pi$ExperimentType == arm_levels[i]); n2 <- sum(dat_pi$ExperimentType == arm_levels[j])
  cf <- 1 - 3 / (4 * (n1 + n2 - 2) - 1)
  p  <- min(2 * min(mean(d < 0), mean(d > 0)), 1)   # two-tailed Bayesian p
  rows[[k]] <- data.frame(
    Group1 = arm_levels[i], Group2 = arm_levels[j],
    Difference = mean(d),
    CI_lower = quantile(d, 0.025), CI_upper = quantile(d, 0.975),
    p_value = p, Hedges_g = mean(d / sd_obs * cf), row.names = NULL
  )
}
pi_pairs <- do.call(rbind, rows)
pi_pairs$p_adj_fdr   <- p.adjust(pi_pairs$p_value, method = "fdr")
pi_pairs$Significance <- sig_stars(pi_pairs$p_adj_fdr)
cat("\n=== PERCEIVED IMPROVEMENT: pairwise (FDR) + Hedges' g ===\n")
# rbind of the per-contrast frames can leave columns non-atomic, which makes
# order() fail with "cannot xtfrm data frames"; coerce before sorting.
pi_pairs <- as.data.frame(pi_pairs)
for (cl in c("Difference","CI_lower","CI_upper","p_value","Hedges_g","p_adj_fdr"))
  if (cl %in% names(pi_pairs)) pi_pairs[[cl]] <- as.numeric(unlist(pi_pairs[[cl]]))
print(pi_pairs[order(pi_pairs$p_adj_fdr),
               c("Group1", "Group2", "Difference", "CI_lower", "CI_upper",
                 "p_adj_fdr", "Hedges_g", "Significance")],
      row.names = FALSE, digits = 3)

cat("\nDone. Figures: p_performance, p_perceived. Tables: perf_pairs, pi_pairs.\n")
