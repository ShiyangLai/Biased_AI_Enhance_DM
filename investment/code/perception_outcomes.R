# ==============================================================================
# perception_outcomes.R
# Subjective/perception outcomes (all 1-7 Likert; notebook §16 export):
#   perceived_improve : post_portfolio_comp_1  ("final better than initial")
#   conf_change       : post_portfolio_conf_1 - pre_portfolio_conf_1
#   post_conf (ANCOVA): post confidence with pre_conf baseline (pre-registered
#                       structure, since a true baseline exists)
#   perf_exp          : post_performance_exp_1 (post-only)
#
# Part 1 — 5 experimental conditions (single/dual assembly identical to
#          active_m2_single_vs_dual.R): outcome ~ ExperimentType + wave (+pre_conf
#          for the ANCOVA), HC3; emmeans + FDR over the 10 pairwise contrasts.
# Part 2 — single-AI ONLY, 5 bias arms (risk gradient, Neutral excluded):
#          outcome ~ ai_group + wave (+pre_conf), HC3; emmeans + FDR pairwise.
# Figures: perceived-improvement bars (5 conditions, content-type fill) and
#          confidence-change bars (single-AI 5 arms, bias-gradient colors).
#
# CAVEAT: single-AI vs dual-AI contrasts are cross-experiment (separate samples).
# Inputs: perceived_improvement.csv (§16), active_m2_treatment_data.csv ,
#         dual_active_m2.csv 
#   setwd("investment/code"); source("_setup.R"); source("perception_outcomes.R")
# ==============================================================================
suppressPackageStartupMessages({
  library(ggplot2); library(emmeans); library(dplyr); library(sandwich)
  library(lmtest); library(scales)
})

.args <- commandArgs(FALSE); .f <- sub("^--file=", "", .args[grep("^--file=", .args)])
SCRIPT_DIR <- if (length(.f)) dirname(normalizePath(.f)) else getwd()
rd <- function(f) { p <- file.path(DATA_DIR, f); if (!file.exists(p)) p <- file.path(DATA_DIR, f)
                    read.csv(p, check.names = FALSE, stringsAsFactors = FALSE) }

pi_df  <- rd("perceived_improvement.csv") %>%
  mutate(conf_change = post_conf - pre_conf)
single <- rd("active_m2_treatment_data.csv")
dual   <- rd("dual_active_m2.csv")

# ══ Part 1: 5 experimental conditions ═════════════════════════════════════════
# NOTE: 9 participants completed BOTH experiments (CloudResearch did not
# exclude across studies), so pi_df has two rows for them (one per experiment).
# The join is therefore keyed on (participantId, experiment).
s1 <- single %>%
  filter(ai_group %in% c("Default", "Extremely Risk-Averse", "Somewhat Risk-Averse",
                         "Extremely Risk-Seeking", "Somewhat Risk-Seeking")) %>%
  transmute(participantId, wave, experiment = "single",
            ExperimentType = ifelse(ai_group == "Default",
                                    "Single AI Default", "Single AI Biased"))
d1 <- dual %>%
  filter(dual_condition %in% c("dual_nonbiased", "dual_balanced", "dual_opposition")) %>%
  transmute(participantId, wave, experiment = "dual",
            ExperimentType = recode(dual_condition,
                                    "dual_nonbiased"  = "Dual AI Default",
                                    "dual_opposition" = "Dual AI Opposition",
                                    "dual_balanced"   = "Dual AI Balanced"))
LEVELS <- c("Single AI Default", "Single AI Biased",
            "Dual AI Default", "Dual AI Opposition", "Dual AI Balanced")
df <- bind_rows(s1, d1) %>%
  left_join(pi_df %>% select(participantId, experiment, perceived_improve, pre_conf,
                             post_conf, conf_change, perf_exp),
            by = c("participantId", "experiment")) %>%
  mutate(ExperimentType = factor(ExperimentType, levels = LEVELS),
         wave = factor(wave))
cat("N by condition:\n"); print(table(df$ExperimentType))

run5 <- function(fml, label) {
  mo <- lm(as.formula(fml), data = df)
  eo <- emmeans(mo, ~ ExperimentType, vcov. = vcovHC(mo, type = "HC3"))
  cat(sprintf("\n=== %s ===\n    %s (HC3)\n", label, fml))
  print(summary(eo), digits = 4)
  print(summary(pairs(eo, adjust = "fdr")), digits = 3)
  invisible(eo)
}
emp <- run5("perceived_improve ~ ExperimentType + wave", "perceived improvement")
run5("conf_change ~ ExperimentType + wave",              "confidence CHANGE (post - pre)")
run5("post_conf ~ ExperimentType + wave + pre_conf",     "post confidence (ANCOVA, pre-registered)")
run5("perf_exp ~ ExperimentType + wave",                 "performance expectation (post-only)")

# ── shared theme ──────────────────────────────────────────────────────────────
base_theme <- theme_classic() +
  theme(
    text = element_text(family = "Avenir", color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.line = element_blank(),
    axis.text = element_text(family = "Avenir", size = 9, color = "black"),
    axis.title = element_text(family = "Avenir", size = 12, color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 0.4),
    axis.ticks.length = unit(3, "pt"),
    panel.grid = element_blank(),
    plot.margin = margin(t = 15, r = 30, b = 15, l = 15),
    legend.position = "none"
  )

# ── figure: perceived improvement, 5 conditions (content-type fill) ───────────
pd1 <- as.data.frame(summary(emp)) %>%
  rename(estimate = emmean, lower_ci = lower.CL, upper_ci = upper.CL) %>%
  mutate(formal_label = factor(gsub(" AI ", " AI\n", as.character(ExperimentType)),
                               levels = gsub(" AI ", " AI\n", LEVELS)),
         fill_color = ifelse(as.character(ExperimentType) %in%
                               c("Single AI Default", "Dual AI Default"),
                             "#B5B0AD", "#228B22"))
ymin1 <- min(pd1$lower_ci); ymax1 <- max(pd1$upper_ci); pad1 <- 0.15 * (ymax1 - ymin1)
p1 <- ggplot(pd1, aes(x = formal_label, y = estimate)) +
  geom_col(aes(fill = fill_color), color = "black", width = 0.65, linewidth = 0) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.3, linewidth = 0.5) +
  geom_text(aes(y = upper_ci + pad1 * 0.35, label = sprintf("%.2f", estimate)),
            vjust = 0, family = "Avenir", size = 3) +
  geom_point(color = "black", size = 3, shape = 20) +
  scale_fill_identity(guide = "legend", breaks = c("#B5B0AD", "#228B22"),
                      labels = c("Default AI content", "Biased AI content"), name = NULL) +
  scale_y_continuous(labels = number_format(accuracy = 0.1)) +
  coord_cartesian(ylim = c(ymin1 - pad1, ymax1 + pad1)) +
  labs(x = "Experimental Condition", y = "Perceived improvement") +
  base_theme + theme(legend.position = "bottom")
print(p1)
ggsave(file.path(FIG_DIR, "perceived_improvement_vertical.png"), p1,
       width = 6, height = 2.8, dpi = 500)

# ══ Part 2: single-AI ONLY, 5 bias arms (risk gradient) ═══════════════════════
ARM_ORD <- c("Extremely Risk-Averse", "Somewhat Risk-Averse", "Default",
             "Somewhat Risk-Seeking", "Extremely Risk-Seeking")
SHORT   <- c("Extremely Risk-Averse" = "E-Averse", "Somewhat Risk-Averse" = "S-Averse",
             "Default" = "Default",
             "Somewhat Risk-Seeking" = "S-Seeking", "Extremely Risk-Seeking" = "E-Seeking")
sarm <- single %>%
  filter(ai_group %in% ARM_ORD) %>%
  left_join(pi_df %>% filter(experiment == "single") %>%
              select(participantId, perceived_improve, pre_conf,
                     post_conf, conf_change, perf_exp),
            by = "participantId") %>%
  mutate(ai_group = factor(ai_group, levels = ARM_ORD), wave = factor(wave))
cat(sprintf("\n\n========== SINGLE-AI ONLY (n = %d): 5-arm comparison ==========\n", nrow(sarm)))
run_arm <- function(fml, label) {
  mo <- lm(as.formula(fml), data = sarm)
  eo <- emmeans(mo, ~ ai_group, vcov. = vcovHC(mo, type = "HC3"))
  cat(sprintf("\n--- %s ---\n    %s (HC3)\n", label, fml))
  print(summary(eo), digits = 4)
  print(summary(pairs(eo, adjust = "fdr")), digits = 3)
  invisible(eo)
}
run_arm("perceived_improve ~ ai_group + wave",     "perceived improvement")
emc <- run_arm("conf_change ~ ai_group + wave",    "confidence CHANGE (post - pre)")
run_arm("post_conf ~ ai_group + wave + pre_conf",  "post confidence (ANCOVA, pre-registered)")
run_arm("perf_exp ~ ai_group + wave",              "performance expectation")

# ── figure: confidence change by single-AI arm (bias-gradient colors) ─────────
bias_colors <- c("E-Averse" = "#1B7837", "S-Averse" = "#7FBF7B", "Default" = "#999999",
                 "S-Seeking" = "#AF8DC3", "E-Seeking" = "#762A83")
pd2 <- as.data.frame(summary(emc)) %>%
  rename(estimate = emmean, lower_ci = lower.CL, upper_ci = upper.CL) %>%
  mutate(label = factor(SHORT[as.character(ai_group)], levels = unname(SHORT[ARM_ORD])))
ymin2 <- min(pd2$lower_ci, 0); ymax2 <- max(pd2$upper_ci); pad2 <- 0.15 * (ymax2 - ymin2)
p2 <- ggplot(pd2, aes(x = label, y = estimate, fill = label)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  geom_col(alpha = 0.85, color = "black", width = 0.65, linewidth = 0) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.3, linewidth = 0.5) +
  geom_text(aes(y = upper_ci + pad2 * 0.35, label = sprintf("%+.2f", estimate)),
            vjust = 0, family = "Avenir", size = 3) +
  geom_point(color = "black", size = 3, shape = 20) +
  scale_fill_manual(values = bias_colors) +
  scale_y_continuous(labels = number_format(accuracy = 0.1)) +
  coord_cartesian(ylim = c(ymin2, ymax2 + pad2)) +
  labs(x = NULL, y = "Confidence change (post - pre)") +
  base_theme
print(p2)
ggsave(file.path(FIG_DIR, "confidence_change_5arm.png"), p2,
       width = 6, height = 2.8, dpi = 500)
cat(sprintf("\nSaved perceived_improvement_vertical.png + confidence_change_5arm.png in %s\n",
            SCRIPT_DIR))

# ══════════════════════════════════════════════════════════════════════════════
# d2-STYLE ordinal Bayesian analysis — confidence change, 5 experimental
# conditions (investment analog of forth_figure_d2.R).
# Outcome: conf_change (post - pre confidence; discrete, ~13 ordered levels)
# Model: ExperimentType + wave (pre-registered structure; no other controls).
# vs the political script: NID FE -> wave FE; the ~UID random intercept DROPS
# (one observation per participant, no repeated measures); UStance/UIdeo/
# AICorrectness have no analog.
# Pipeline: MCMCglmm ordinal -> posterior marginal means on the LATENT scale
# -> vertical bars (orange value-gradient, triangle markers) -> posterior
# pairwise with Bayesian p, FDR, Hedges' g.
# ══════════════════════════════════════════════════════════════════════════════
suppressPackageStartupMessages({ library(MCMCglmm) })
set.seed(123)

datb <- df %>% filter(!is.na(conf_change)) %>%
  mutate(conf_ord = factor(conf_change, ordered = TRUE))
cat(sprintf("\n\n========== d2-style ordinal analysis: confidence change (n = %d) ==========\n",
            nrow(datb)))

# Bayesian ordinal (fixed effects only; no UID RE — single observation each)
mcmc <- MCMCglmm(conf_ord ~ ExperimentType + wave,
                 family = "ordinal", nitt = 25000, thin = 10, burnin = 5000,
                 data = as.data.frame(datb), verbose = FALSE)
post <- as.matrix(mcmc$Sol)
col_names <- colnames(post)
mode_wave <- names(sort(table(datb$wave), decreasing = TRUE))[1]

treatments <- LEVELS
mm <- matrix(NA, nrow(post), length(treatments), dimnames = list(NULL, treatments))
for (i in seq_along(treatments)) {
  lp <- if ("(Intercept)" %in% col_names) post[, "(Intercept)"] else rep(0, nrow(post))
  cn <- paste0("ExperimentType", treatments[i])
  if (cn %in% col_names) lp <- lp + post[, cn]
  wn <- paste0("wave", mode_wave)
  if (wn %in% col_names) lp <- lp + post[, wn]
  mm[, i] <- lp
}
plot_db <- data.frame(
  ExperimentType = treatments,
  posterior_mean = colMeans(mm),
  lower_ci = apply(mm, 2, quantile, 0.025),
  upper_ci = apply(mm, 2, quantile, 0.975)
)
cat("\n--- posterior marginal means (latent scale; wave at mode) ---\n")
print(plot_db, row.names = FALSE, digits = 3)

# figure (forth_figure_d2 style: orange value ramp, triangles) — VERTICAL
plot_db$formal_label <- factor(gsub(" AI ", " AI\n", plot_db$ExperimentType),
                               levels = gsub(" AI ", " AI\n", LEVELS))
oramp <- colorRampPalette(c("#FFB347", "#FFA500", "#FF8C00", "#D2691E"))(100)
nvb <- with(plot_db, (posterior_mean - min(posterior_mean)) /
                     diff(range(posterior_mean)))
plot_db$fill_color <- oramp[pmax(1, round(nvb * 99) + 1)]
y_minb <- min(plot_db$lower_ci); y_maxb <- max(plot_db$upper_ci)
padb <- 0.15 * (y_maxb - y_minb)
p3 <- ggplot(plot_db, aes(x = formal_label, y = posterior_mean)) +
  geom_col(aes(fill = fill_color), color = "black", width = 0.7, linewidth = 0) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),
                width = 0.3, linewidth = 0.5, color = "black") +
  geom_text(aes(y = upper_ci + padb * 0.35, label = round(posterior_mean, 3)),
            vjust = 0, family = "Avenir", size = 3, color = "black") +
  geom_point(color = "black", size = 2, shape = 17) +
  scale_fill_identity() +
  scale_y_continuous(expand = c(0, 0), labels = number_format(accuracy = 0.01)) +
  coord_cartesian(ylim = c(y_minb - padb * 4, y_maxb + padb * 7)) +
  labs(x = "Experimental Condition", y = "Confidence Change") +
  base_theme
print(p3)

ggsave(file.path(FIG_DIR, "confidence_change_d2_vertical.png"), p3,
       width = 6, height = 4.0, dpi = 500)

# posterior pairwise: Bayesian two-tailed p, FDR, Hedges' g (empirical pooled SD)
pooled_sd_b <- sd(as.numeric(datb$conf_change), na.rm = TRUE)
cat(sprintf("\nUsing empirical pooled SD of observed confidence change: %.4f\n", pooled_sd_b))
cmb <- list(); k <- 0
for (i in 1:(length(treatments) - 1)) for (j in (i + 1):length(treatments)) {
  k <- k + 1
  ds <- mm[, i] - mm[, j]
  pv <- min(1, 2 * min(mean(ds < 0), mean(ds > 0)))
  n1 <- sum(datb$ExperimentType == treatments[i])
  n2 <- sum(datb$ExperimentType == treatments[j])
  Jc <- 1 - 3 / (4 * (n1 + n2 - 2) - 1)
  gs <- ds / pooled_sd_b * Jc
  cmb[[k]] <- data.frame(Group1 = treatments[i], Group2 = treatments[j],
    Difference = mean(ds), CI_lower = quantile(ds, .025), CI_upper = quantile(ds, .975),
    p_value = pv, Hedges_g = mean(gs),
    g_lower = quantile(gs, .025), g_upper = quantile(gs, .975))
}
cmb <- do.call(rbind, cmb)
cmb$p_adj_fdr <- p.adjust(cmb$p_value, "fdr")
cmb$sig <- cut(cmb$p_adj_fdr, c(-Inf, .001, .01, .05, .1, Inf),
               labels = c("***", "**", "*", "†", "ns"))
cat("\n=== posterior pairwise comparisons (latent scale; FDR; Hedges' g) ===\n")
print(cmb %>% arrange(p_adj_fdr), row.names = FALSE, digits = 3)

# ══════════════════════════════════════════════════════════════════════════════
# d2-STYLE ordinal Bayesian analysis — PERCEIVED IMPROVEMENT, 5 conditions.
# Outcome: perceived_improve (1-7 ordered Likert). Same pipeline/model/figure as
# the confidence-change block above (MCMCglmm ordinal, wave FE, no other controls).
# ══════════════════════════════════════════════════════════════════════════════
set.seed(123)
datp <- df %>% filter(!is.na(perceived_improve)) %>%
  mutate(pi_ord = factor(perceived_improve, ordered = TRUE))
cat(sprintf("\n\n========== d2-style ordinal analysis: perceived improvement (n = %d) ==========\n",
            nrow(datp)))

mcmcp <- MCMCglmm(pi_ord ~ ExperimentType + wave,
                  family = "ordinal", nitt = 25000, thin = 10, burnin = 5000,
                  data = as.data.frame(datp), verbose = FALSE)
postp <- as.matrix(mcmcp$Sol)
cnp <- colnames(postp)
mode_wave_p <- names(sort(table(datp$wave), decreasing = TRUE))[1]

mmp <- matrix(NA, nrow(postp), length(treatments), dimnames = list(NULL, treatments))
for (i in seq_along(treatments)) {
  lp <- if ("(Intercept)" %in% cnp) postp[, "(Intercept)"] else rep(0, nrow(postp))
  ce <- paste0("ExperimentType", treatments[i]); if (ce %in% cnp) lp <- lp + postp[, ce]
  we <- paste0("wave", mode_wave_p);             if (we %in% cnp) lp <- lp + postp[, we]
  mmp[, i] <- lp
}
plot_dp <- data.frame(
  ExperimentType = treatments,
  posterior_mean = colMeans(mmp),
  lower_ci = apply(mmp, 2, quantile, 0.025),
  upper_ci = apply(mmp, 2, quantile, 0.975)
)
cat("\n--- posterior marginal means (latent scale; wave at mode) ---\n")
print(plot_dp, row.names = FALSE, digits = 3)

plot_dp$formal_label <- factor(gsub(" AI ", " AI\n", plot_dp$ExperimentType),
                               levels = gsub(" AI ", " AI\n", LEVELS))
nvp <- with(plot_dp, (posterior_mean - min(posterior_mean)) / diff(range(posterior_mean)))
plot_dp$fill_color <- oramp[pmax(1, round(nvp * 99) + 1)]
y_minp <- min(plot_dp$lower_ci); y_maxp <- max(plot_dp$upper_ci)
padp <- 0.15 * (y_maxp - y_minp)
p4 <- ggplot(plot_dp, aes(x = formal_label, y = posterior_mean)) +
  geom_col(aes(fill = fill_color), color = "black", width = 0.7, linewidth = 0) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci),
                width = 0.3, linewidth = 0.5, color = "black") +
  geom_text(aes(y = upper_ci + padp * 0.35, label = round(posterior_mean, 3)),
            vjust = 0, family = "Avenir", size = 3, color = "black") +
  geom_point(color = "black", size = 2, shape = 17) +
  scale_fill_identity() +
  scale_y_continuous(expand = c(0, 0), labels = number_format(accuracy = 0.01)) +
  coord_cartesian(ylim = c(y_minp - padp * 4, y_maxp + padp * 7)) +
  labs(x = "Experimental Condition", y = "Perceived Improvement") +
  base_theme
print(p4)
ggsave(file.path(FIG_DIR, "perceived_improvement_d2_vertical.png"), p4,
       width = 6, height = 4.0, dpi = 500)

pooled_sd_p <- sd(as.numeric(datp$perceived_improve), na.rm = TRUE)
cat(sprintf("\nUsing empirical pooled SD of observed perceived improvement: %.4f\n", pooled_sd_p))
cmp <- list(); k <- 0
for (i in 1:(length(treatments) - 1)) for (j in (i + 1):length(treatments)) {
  k <- k + 1
  ds <- mmp[, i] - mmp[, j]
  pv <- min(1, 2 * min(mean(ds < 0), mean(ds > 0)))
  n1 <- sum(datp$ExperimentType == treatments[i])
  n2 <- sum(datp$ExperimentType == treatments[j])
  Jc <- 1 - 3 / (4 * (n1 + n2 - 2) - 1)
  gs <- ds / pooled_sd_p * Jc
  cmp[[k]] <- data.frame(Group1 = treatments[i], Group2 = treatments[j],
    Difference = mean(ds), CI_lower = quantile(ds, .025), CI_upper = quantile(ds, .975),
    p_value = pv, Hedges_g = mean(gs),
    g_lower = quantile(gs, .025), g_upper = quantile(gs, .975))
}
cmp <- do.call(rbind, cmp)
cmp$p_adj_fdr <- p.adjust(cmp$p_value, "fdr")
cmp$sig <- cut(cmp$p_adj_fdr, c(-Inf, .001, .01, .05, .1, Inf),
               labels = c("***", "**", "*", "†", "ns"))
cat("\n=== posterior pairwise comparisons (latent scale; FDR; Hedges' g) ===\n")
print(cmp %>% arrange(p_adj_fdr), row.names = FALSE, digits = 3)

