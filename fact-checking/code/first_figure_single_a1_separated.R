# Ensure proper factoring
single_ai_processed$StanceRelationship <- factor(single_ai_processed$StanceRelationship,
                                                 levels = c("Baseline", "Echo Chamber",
                                                            "Moderate Opposition", "Strong Opposition"))

single_ai_processed$PreCorrect <- ifelse(single_ai_processed$PreEva == df1$Truth, 1, 0)
single_ai_processed$PostCorrect <- ifelse(single_ai_processed$PostEva == df1$Truth, 1, 0)
single_ai_processed$IsDefault <- ifelse(single_ai_processed$AIStanceLabel == "Default", 1, 0)

# ========================================
# Main analysis
# ========================================
single_ai_processed_ <- single_ai_processed[single_ai_processed$AIStanceLabel_S != "Neutral",]

# Five-level treatment arm: split each partisan direction into its strength using
# the raw AIStanceLabel, instead of collapsing to Republican/Democrat/Non-Biased.
# Reference level = "Default".
single_ai_processed_$StanceGroup <- dplyr::case_when(
  single_ai_processed_$AIStanceLabel == "Strong Republican"   ~ "Strong Republican",
  single_ai_processed_$AIStanceLabel == "Somewhat Republican" ~ "Somewhat Republican",
  single_ai_processed_$AIStanceLabel == "Default"             ~ "Default",
  single_ai_processed_$AIStanceLabel == "Somewhat Democrat"   ~ "Somewhat Democrat",
  single_ai_processed_$AIStanceLabel == "Strong Democrat"     ~ "Strong Democrat"
)
single_ai_processed_$StanceGroup <- factor(single_ai_processed_$StanceGroup,
                                           levels = c("Default",
                                                      "Strong Republican", "Somewhat Republican",
                                                      "Somewhat Democrat", "Strong Democrat"))

# Binary version kept ONLY for the causal-mediation block below (mediate() needs
# a binary treatment).
single_ai_processed_$BiasedType <- ifelse(single_ai_processed_$AIStanceLabel_S == "Default",
                                          "Non-Biased", "Biased")
single_ai_processed_$BiasedType <- factor(single_ai_processed_$BiasedType,
                                          levels = c("Non-Biased", "Biased"))

single_ai_model <- lm(PostPerformance ~ StanceGroup + as.factor(NID) + PrePerformance +
                        AICorrectness,    # + as.factor(UIdeo) + AICorrectness
                      data = single_ai_processed_)

summary(single_ai_model)
vcov_clustered <- vcovCL(single_ai_model,
                        cluster = single_ai_processed_$UID)
clustered_results <- coeftest(single_ai_model, vcov = vcov_clustered)
print(clustered_results)

single_ai_model <- lmer(PostPerformance ~ PrePerformance + StanceGroup + as.factor(NID) +
                          (1|UID),  # + as.factor(UIdeo) + AICorrectness
                        data = single_ai_processed_)
summary(single_ai_model)
summary(single_ai_model)$sigma
# performance::r2(single_ai_model)

# ========================================
# Separate analysis Rep vs Dem
#   Fit each direction separately (Default = reference). Report OLS (cluster-
#   robust) and mixed-effects (lmerTest -> Satterthwaite) p-values, then FDR-
#   adjust the four vs-Default pairwise comparisons.
# ========================================
library(lmerTest)   # so lmer() fixed effects carry Satterthwaite t / df / p

## Republican | Default  (Default + Somewhat/Strong Republican; drop Democrat)
rep_data <- single_ai_processed_[single_ai_processed_$AIStanceLabel_S != "Democrat", ]
rep_lm   <- lm(PostPerformance ~ PrePerformance + AIStanceLabel + as.factor(NID), data = rep_data)
rep_ct   <- coeftest(rep_lm, vcov = vcovCL(rep_lm, cluster = rep_data$UID))
rep_lmer <- lmerTest::lmer(PostPerformance ~ PrePerformance + AIStanceLabel + as.factor(NID) + (1|UID), data = rep_data)

## Democrat | Default  (Default + Somewhat/Strong Democrat; drop Republican)
dem_data <- single_ai_processed_[single_ai_processed_$AIStanceLabel_S != "Republican", ]
dem_lm   <- lm(PostPerformance ~ PrePerformance + AIStanceLabel + as.factor(NID), data = dem_data)
dem_ct   <- coeftest(dem_lm, vcov = vcovCL(dem_lm, cluster = dem_data$UID))
dem_lmer <- lmerTest::lmer(PostPerformance ~ PrePerformance + AIStanceLabel + as.factor(NID) + (1|UID), data = dem_data)

cat("\n=== Republican | Default : OLS + clustered SE ===\n");     print(rep_ct)
cat("=== Republican | Default : Mixed effects (lmerTest) ===\n"); print(coef(summary(rep_lmer)))
cat(sprintf("  sigma = %.3f\n", sigma(rep_lmer))); print(performance::r2(rep_lmer))
cat("\n=== Democrat | Default : OLS + clustered SE ===\n");       print(dem_ct)
cat("=== Democrat | Default : Mixed effects (lmerTest) ===\n");   print(coef(summary(dem_lmer)))
cat(sprintf("  sigma = %.3f\n", sigma(dem_lmer))); print(performance::r2(dem_lmer))

## ---- Pairwise (each arm vs Default) + Benjamini-Hochberg FDR ----
# Pull the AIStanceLabel (vs-Default) rows from a coefficient matrix; `pcol` is
# the column holding the p-value (4 for coeftest, 5 for lmerTest summary).
grab <- function(cmat, side, source, pcol) {
  cmat <- as.matrix(cmat)                       # strip coeftest/summary class -> plain matrix
  keep <- grepl("^AIStanceLabel", rownames(cmat))
  data.frame(source = source, side = side,
             arm = sub("AIStanceLabel", "", rownames(cmat)[keep]),
             estimate = round(cmat[keep, 1], 3),
             SE = round(cmat[keep, 2], 3),
             p_raw = cmat[keep, pcol], row.names = NULL)
}
pairwise <- rbind(
  grab(rep_ct,                  "Republican", "OLS-clustered", 4),
  grab(dem_ct,                  "Democrat",   "OLS-clustered", 4),
  grab(coef(summary(rep_lmer)), "Republican", "Mixed",         5),
  grab(coef(summary(dem_lmer)), "Democrat",   "Mixed",         5)
)
# FDR across the four vs-Default contrasts, separately within each model type
pairwise$p_fdr <- ave(pairwise$p_raw, pairwise$source, FUN = function(p) p.adjust(p, method = "fdr"))
stars <- function(p) as.character(cut(p, c(-Inf, .001, .01, .05, .1, Inf),
                                      c("***", "**", "*", "†", ""), right = FALSE))
pairwise$sig_raw <- stars(pairwise$p_raw)
pairwise$sig_fdr <- stars(pairwise$p_fdr)

cat("\n=== Pairwise vs Default: raw and FDR-adjusted p-values ===\n")
print(pairwise, row.names = FALSE, digits = 3)

# ========================================
# Marginal mean analysis
# ========================================
# Refit the 5-level model as `single_ai_model` so the emmeans / effect-size /
# plot below use StanceGroup (the separate section above overwrote it).
single_ai_model <- lmer(PostPerformance ~ PrePerformance + StanceGroup + as.factor(NID) +
                          (1|UID),
                        data = single_ai_processed_)

emm <- emmeans(single_ai_model, ~ StanceGroup)
contrasts <- pairs(emm, infer = TRUE)
print(contrasts)

# ---- Relative (%) differences vs Default -------------------------------------
# Rigorous version of "X% higher than Default": ratio contrasts from the SAME
# fitted model, via delta-method log-regrid of the adjusted means. Each ratio is
# mu_arm / mu_Default with a delta-method CI; (ratio - 1) x 100 = % difference.
# This inherits the full covariate adjustment (PrePerformance, NID, random UID),
# unlike dividing a raw coefficient by an eyeballed baseline mean.
rel_emm <- regrid(emm, transform = "log")
rel_vs_default <- contrast(rel_emm, "trt.vs.ctrl", ref = "Default",
                           type = "response", adjust = "fdr", infer = TRUE)
cat("\n=== Relative differences vs Default (ratio scale) ===\n")
print(rel_vs_default)
rel_df <- as.data.frame(rel_vs_default)
rel_df$pct_diff <- 100 * (rel_df$ratio - 1)
rel_df$pct_lo95 <- 100 * (rel_df$lower.CL - 1)
rel_df$pct_hi95 <- 100 * (rel_df$upper.CL - 1)
cat("\n=== Percent difference vs Default (FDR-adjusted, delta-method 95% CI) ===\n")
print(rel_df[, c("contrast", "ratio", "pct_diff", "pct_lo95", "pct_hi95", "p.value")],
      row.names = FALSE, digits = 3)

# Extract contrast estimate (one row per pairwise contrast among the 5 arms)
contrast_estimate <- summary(contrasts)$estimate

# For mixed effects, consider total variance
total_var <- VarCorr(single_ai_model)$UID[1] + sigma(single_ai_model)^2
pooled_sd <- sqrt(total_var)

# Calculate effect size
cohens_d <- contrast_estimate / pooled_sd

# Use degrees of freedom from emmeans
df_emmeans <- summary(contrasts)$df
J <- 1 - (3 / (4 * df_emmeans - 1))
hedges_g <- cohens_d * J
print(data.frame(contrast = summary(contrasts)$contrast, hedges_g = hedges_g))

# ========================================
# Robustness: controlling for AI correctness
# ========================================
# Concern: biased instructions also shift the ASSISTANT's own judgment accuracy
# across headlines, so arm effects could partly reflect AI-accuracy differences
# rather than the bias itself. (1) quantify the imbalance; (2) refit the main
# mixed model with AICorrectness and recompute the same vs-Default quantities.
# NOTE: AICorrectness is post-treatment (a mediator), so the adjusted estimates
# are a conservative bound, not the primary specification.

## (1) AI correctness by arm
m_bal <- lm(AICorrectness ~ StanceGroup + as.factor(NID), data = single_ai_processed_)
vcov_bal <- vcovCL(m_bal, cluster = single_ai_processed_$UID)
cat("\n=== AI correctness by arm (vs Default; UID-clustered) ===\n")
print(coeftest(m_bal, vcov = vcov_bal))
emm_bal <- emmeans(m_bal, ~ StanceGroup, vcov. = vcov_bal)
cat("\nRepublican arms vs Democrat arms (pooled):\n")
print(summary(contrast(emm_bal, list(RepVsDem = c(0, .5, .5, -.5, -.5))), infer = TRUE))

## (2) Main mixed model + AICorrectness: same contrasts / g / relative % as above
model_adj <- lmer(PostPerformance ~ PrePerformance + StanceGroup + as.factor(NID) +
                    AICorrectness + (1|UID),
                  data = single_ai_processed_)
emm_adj <- emmeans(model_adj, ~ StanceGroup)
contrasts_adj <- pairs(emm_adj, infer = TRUE)
vc_adj <- as.data.frame(VarCorr(model_adj))
pooled_sd_adj <- sqrt(sum(vc_adj$vcov[vc_adj$grp %in% c("UID", "Residual")]))
sum_adj <- summary(contrasts_adj)
vs_def <- sum_adj[grepl("Default", sum_adj$contrast), ]
vs_def$hedges_g <- vs_def$estimate / pooled_sd_adj * (1 - 3 / (4 * vs_def$df - 1))
cat("\n=== ADJUSTED for AICorrectness: contrasts involving Default (Tukey) ===\n")
print(vs_def[, c("contrast", "estimate", "SE", "p.value", "hedges_g")],
      row.names = FALSE, digits = 3)

rel_adj <- contrast(regrid(emm_adj, transform = "log"), "trt.vs.ctrl", ref = "Default",
                    type = "response", adjust = "fdr", infer = TRUE)
rel_adj_df <- as.data.frame(rel_adj)
rel_adj_df$pct_diff <- 100 * (rel_adj_df$ratio - 1)
rel_adj_df$pct_lo95 <- 100 * (rel_adj_df$lower.CL - 1)
rel_adj_df$pct_hi95 <- 100 * (rel_adj_df$upper.CL - 1)
cat("\n=== ADJUSTED: percent difference vs Default (FDR) ===\n")
print(rel_adj_df[, c("contrast", "pct_diff", "pct_lo95", "pct_hi95", "p.value")],
      row.names = FALSE, digits = 3)

# ========================================
# Casual mediation analysis (kept binary: Biased vs Non-Biased)
# ========================================
single_ai_processed_$NID_factor <- as.factor(single_ai_processed_$NID)

mediator_lm <- lm(AICorrectness ~ BiasedType + NID_factor,
                  data = single_ai_processed_)

outcome_lm <- lm(PostPerformance ~ BiasedType + AICorrectness + NID_factor,
                 data = single_ai_processed_)

total_lm <- lm(PostPerformance ~ BiasedType + NID_factor,
               data = single_ai_processed_)

mediation_results_clustered <- mediate(mediator_lm, outcome_lm,
                                       treat = "BiasedType",
                                       mediator = "AICorrectness",
                                       boot = FALSE,
                                       sims = 1000,
                                       cluster = single_ai_processed_$UID)

summary(mediation_results_clustered)

# Get emmeans for StanceGroup levels
emm_single <- emmeans(single_ai_model, ~ StanceGroup)
emm_single_summary <- summary(emm_single)

print(emm_single_summary)

# Calculate pairwise differences between arms
single_ai_contrast <- pairs(emm_single)
print(single_ai_contrast)

# ========================================
# Prepare data for visualization
# ========================================
# Single AI plot data
single_ai_plot_data <- data.frame(
  StanceGroup = emm_single_summary$StanceGroup,
  MeanPerformanceChange = emm_single_summary$emmean,
  SE_PerformanceChange = emm_single_summary$SE
) %>%
  mutate(
    y_position = case_when(
      StanceGroup == "Strong Republican"   ~ 5,
      StanceGroup == "Somewhat Republican" ~ 4,
      StanceGroup == "Default"             ~ 3,
      StanceGroup == "Somewhat Democrat"   ~ 2,
      StanceGroup == "Strong Democrat"     ~ 1
    ),
    CI_90_lower = MeanPerformanceChange - SE_PerformanceChange * qnorm(0.95),
    CI_90_upper = MeanPerformanceChange + SE_PerformanceChange * qnorm(0.95),
    CI_95_lower = MeanPerformanceChange - SE_PerformanceChange * qnorm(0.975),
    CI_95_upper = MeanPerformanceChange + SE_PerformanceChange * qnorm(0.975),
    CI_99_lower = MeanPerformanceChange - SE_PerformanceChange * qnorm(0.995),
    CI_99_upper = MeanPerformanceChange + SE_PerformanceChange * qnorm(0.995),
    StanceGroup_Legend = case_when(
      StanceGroup == "Strong Republican"   ~ "E-Rep.",
      StanceGroup == "Somewhat Republican" ~ "S-Rep.",
      StanceGroup == "Default"             ~ "Default",
      StanceGroup == "Somewhat Democrat"   ~ "S-Dem.",
      StanceGroup == "Strong Democrat"     ~ "E-Dem."
    )
  )

# ========================================
# Visualization
# ========================================
nature_theme <- theme_classic() +
  theme(
    text = element_text(family = "Avenir", size = 8),
    plot.title = element_text(family = "Avenir", size = 10, face = "bold", hjust = 0),
    axis.title = element_text(family = "Avenir", size = 9, face = "plain"),
    axis.text = element_text(family = "Avenir", size = 8, color = "black"),
    axis.text.y = element_text(family = "Avenir", size = 9, color = "black", face = "plain"),
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.ticks.length = unit(0.15, "cm"),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 10)
  )

# Diverging red -> gray -> blue palette across the partisan gradient
bias_colors <- c(
  "E-Rep."  = "#B2182B",
  "S-Rep." = "#EF8A62",
  "Default"    = "#999999",
  "S-Dem." = "#67A9CF",
  "E-Dem."  = "#2166AC"
)

single_ai_plot <- ggplot(single_ai_plot_data, aes(y = y_position)) +
  geom_errorbarh(aes(xmin = CI_99_lower, xmax = CI_99_upper, color = StanceGroup_Legend),
                 height = 0.25, size = 1.2, alpha = 0.3) +
  geom_errorbarh(aes(xmin = CI_95_lower, xmax = CI_95_upper, color = StanceGroup_Legend),
                 height = 0.2, size = 0.9, alpha = 0.5) +
  geom_errorbarh(aes(xmin = CI_90_lower, xmax = CI_90_upper, color = StanceGroup_Legend),
                 height = 0.15, size = 0.7, alpha = 0.8) +
  geom_point(aes(x = MeanPerformanceChange, color = StanceGroup_Legend),
             size = 3, alpha = 0.9) +
  scale_color_manual(values = bias_colors,
                     breaks = c("E-Rep.", "S-Rep.", "Default", "S-Dem.", "E-Dem.")) +
  guides(color = guide_legend(nrow = 2)) +
  scale_y_continuous(breaks = c(5, 4, 3, 2, 1),
                     labels = c("Ext. Rep.", "Swt. Rep.", "Default",
                                "Swt. Dem.", "Ext. Dem."),
                     expand = expansion(add = c(0.3, 0.3))) +
  scale_x_continuous(labels = label_number(accuracy = 0.01)) +
  labs(x = "Post-Interaction Performance",
       y = NULL) +
  xlim(0.55, 0.85) +   # NOTE: with 5 arms the extreme groups may fall outside this range; widen if points/bars get clipped
  nature_theme +
  theme(legend.position = "bottom",
        legend.justification = "center",
        legend.box.just = "center",
        legend.title = element_blank(),
        legend.text = element_text(family = "Avenir", size = 9),
        axis.text.y = element_text(angle = 90, hjust = 0.5))

# Print the plot
print(single_ai_plot)
ggsave(file.path("../figures/performance_comparison.png"), single_ai_plot, width = 2.34, height = 5., dpi = 500)
