repdem_single_ai <- single_ai_processed_ %>%
  dplyr::filter(
    (UStanceLabel_S != "Independent") & (BiasedType != "Default")
  )

repdem_single_ai$BiasSide <- ifelse(((repdem_single_ai$AIStanceLabel_S == "Republican") &
                                      (repdem_single_ai$UStanceLabel_S == "Democrat")) |
                                      ((repdem_single_ai$AIStanceLabel_S == "Democrat") &
                                         (repdem_single_ai$UStanceLabel_S == "Republican")), "Opposite", "Same")

table(repdem_single_ai$BiasSide)
repdem_single_ai$BiasSide <-factor(repdem_single_ai$BiasSide, levels = c("Same", "Opposite"))

repdem_single_ai$PerceivedImproveCode <- as.numeric(repdem_single_ai$PerceivedImproveCode)

# BiasSide * UStanceLabel_S = echo/opposition crossed with the user's party
# (Rep/Dem). This gives the four echo-chamber/opposition combination cells and
# lets the interaction be estimated. The separate `UStanceLabel` main effect is
# dropped: user party is already carried by UStanceLabel_S in the interaction.
per_model <- lm(PostPerformance ~ BiasSide * UStanceLabel_S + PrePerformance + as.factor(NID) +
                  UIdeo + AICorrectness,
                  data = repdem_single_ai)
summary(per_model)
vcov_per <- vcovCL(per_model, cluster = repdem_single_ai$UID)
clustered_results_per <- coeftest(per_model, vcov = vcov_per)
print(clustered_results_per)
df.residual(per_model)
performance::r2(per_model)

per_model <- lmer(PostPerformance ~ BiasSide * UStanceLabel_S + PrePerformance + as.factor(NID) +
                    (1|UID) + UIdeo + AICorrectness,
                        data = repdem_single_ai)
summary(per_model)
summary(per_model)$sigma
df.residual(per_model)
performance::r2(per_model)

# Four combination cells (echo/opposition x Rep/Dem user)
emm_full <- emmeans(per_model, ~ BiasSide * UStanceLabel_S)
print("Estimated marginal means (echo/opposition x user party):")
print(emm_full)

# Simple effect: echo (Same) vs opposition (Opposite) WITHIN each user party
simple_by_party <- contrast(emm_full, "pairwise", by = "UStanceLabel_S", adjust = "fdr")
print("Echo vs Opposition within each user party:")
print(simple_by_party)

# Interaction: does the echo-vs-opposition effect differ between Rep and Dem users?
interaction_test_custom <- contrast(emm_full,
                                    interaction = c("pairwise", "pairwise"),
                                    adjust = "fdr")
print("Custom interaction contrasts:")
print(interaction_test_custom)

# Calculate Hedges' g for interaction contrasts
print("\n=== Calculating Hedges' g for Interaction Contrasts ===")

# Extract contrast results
contrast_summary <- summary(interaction_test_custom, infer = TRUE)
print("Contrast summary with confidence intervals:")
print(contrast_summary)

# Get pooled standard deviation from the lmer model
# For lmer, we use the residual standard deviation
pooled_sd <- sigma(per_model)
print(paste("Pooled standard deviation (residual SD from model):", round(pooled_sd, 4)))

# Calculate sample sizes for bias correction
# Get the sample size for each combination of BiasSide and UStanceLabel_S
sample_sizes <- table(repdem_single_ai$BiasSide, repdem_single_ai$UStanceLabel_S)
print("Sample sizes by BiasSide and UStanceLabel_S:")
print(sample_sizes)

# Total sample size for bias correction (conservative approach)
total_n <- nrow(repdem_single_ai)
print(paste("Total sample size:", total_n))

# Calculate Hedges' g for each contrast
hedges_results <- data.frame(
  Contrast = rownames(contrast_summary),
  Estimate = contrast_summary$estimate,
  SE = contrast_summary$SE,
  df = contrast_summary$df,
  t_ratio = contrast_summary$t.ratio,
  p_value = contrast_summary$p.value,
  Lower_CI = contrast_summary$lower.CL,
  Upper_CI = contrast_summary$upper.CL
)

# Calculate Hedges' g
hedges_results$Cohens_d <- hedges_results$Estimate / pooled_sd

# Apply bias correction for Hedges' g
# Use conservative total sample size for correction
correction_factor <- 1 - (3 / (4 * total_n - 9))
hedges_results$Hedges_g <- hedges_results$Cohens_d * correction_factor

# Calculate confidence intervals for Hedges' g
hedges_results$Hedges_g_Lower_CI <- hedges_results$Lower_CI / pooled_sd * correction_factor
hedges_results$Hedges_g_Upper_CI <- hedges_results$Upper_CI / pooled_sd * correction_factor

# Add effect size interpretation
hedges_results$Interpretation <- sapply(abs(hedges_results$Hedges_g), function(x) {
  if(x < 0.2) "negligible"
  else if(x < 0.5) "small"
  else if(x < 0.8) "medium"
  else "large"
})

# Display results
print("\n=== Hedges' g Results for Interaction Contrasts ===")
hedges_final <- hedges_results[, c("Contrast", "Estimate", "p_value", 
                                   "Hedges_g", "Hedges_g_Lower_CI", "Hedges_g_Upper_CI", 
                                   "Interpretation")]
print(hedges_final)


# BiasSide analysis
emm_bias <- emmeans(per_model, ~ BiasSide)
emm_bias_df <- as.data.frame(emm_bias)

# Pairwise comparison using emmeans
contrast_bias <- contrast(emm_bias, "pairwise")
contrast_bias_df <- as.data.frame(contrast_bias)

# Print results
cat("\n=== EMMEANS RESULTS FOR BIASSIDE (Mixed Effects) ===\n")
cat("\nPost-Interaction Performance by Bias Side:\n")
print(emm_bias_df)

cat("\n=== PAIRWISE COMPARISON (Mixed Effects) ===\n")
print(contrast_bias_df)
confint(contrast_bias)

cat("\n=== ALTERNATIVE: Manual calculation using raw data ===\n")
bias_same <- repdem_single_ai$PostPerformance[repdem_single_ai$BiasSide == "Same"]
bias_opposite <- repdem_single_ai$PostPerformance[repdem_single_ai$BiasSide == "Opposite"]

# Calculate means
mean_same <- mean(bias_same, na.rm = TRUE)
mean_opposite <- mean(bias_opposite, na.rm = TRUE)
diff_raw <- mean_same - mean_opposite

# Calculate pooled standard deviation
n1 <- length(bias_same[!is.na(bias_same)])
n2 <- length(bias_opposite[!is.na(bias_opposite)])
sd1 <- sd(bias_same, na.rm = TRUE)
sd2 <- sd(bias_opposite, na.rm = TRUE)

# Pooled SD formula
pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))

# Cohen's d
cohens_d_manual <- diff_raw / pooled_sd

# Hedges' g correction factor
df_total <- n1 + n2 - 2
correction_factor_raw <- 1 - (3 / (4 * df_total - 1))
hedges_g_raw_data <- cohens_d_manual * correction_factor_raw


# Add bias_side_numeric for plotting (Opposite = 0, Same = 1)
emm_bias_df <- emm_bias_df %>%
  mutate(bias_side_numeric = case_when(
    BiasSide == "Opposite" ~ 0,
    BiasSide == "Same" ~ 1,
    TRUE ~ NA_real_
  ))


# Define colors
color_bias <- "#006400"  # Sea green
# Add a column to identify which bar has higher/lower values
emm_bias_df <- emm_bias_df %>%
  mutate(value_level = ifelse(emmean == max(emmean), "Higher", "Lower"))

# Define colors - darker for higher value, lighter for lower value
color_higher <- "#006400"  # Dark green for higher value
color_lower <- "#90EE90"   # Light green for lower value


# Calculate degrees of freedom and t-values for different confidence levels
df_bias <- df.residual(per_model)  # was `df_clustered`, which is never defined
t_90 <- qt(0.95, df_bias)   # 90% CI
t_95 <- qt(0.975, df_bias)  # 95% CI
t_99 <- qt(0.995, df_bias)  # 99% CI

# Define color gradients (green family)
color_bias_90 <- "#2E8B57"    # Sea green (darkest)
color_bias_95 <- "#90EE90"    # Light green (medium)
color_bias_99 <- "#F0FFF0"    # Honeydew (lightest)

# Add multiple confidence intervals
emm_bias_multi <- emm_bias_df %>%
  mutate(
    # 90% CI
    lower.CL_90 = emmean - SE * t_90,
    upper.CL_90 = emmean + SE * t_90,
    # 95% CI (already exists)
    lower.CL_95 = lower.CL,
    upper.CL_95 = upper.CL,
    # 99% CI
    lower.CL_99 = emmean - SE * t_99,
    upper.CL_99 = emmean + SE * t_99
  )

# Define box widths for different confidence levels
box_width_base <- 0.08
box_width_90 <- box_width_base * 1.4   # Narrowest (90% CI)
box_width_95 <- box_width_base * 1.0   # Medium (95% CI)
box_width_99 <- box_width_base * 0.7   # Widest (99% CI)

# Add box coordinates for all confidence levels
emm_bias_multi <- emm_bias_multi %>%
  mutate(
    # 90% CI boxes
    xmin_90 = bias_side_numeric - box_width_90,
    xmax_90 = bias_side_numeric + box_width_90,
    # 95% CI boxes
    xmin_95 = bias_side_numeric - box_width_95,
    xmax_95 = bias_side_numeric + box_width_95,
    # 99% CI boxes
    xmin_99 = bias_side_numeric - box_width_99,
    xmax_99 = bias_side_numeric + box_width_99
  )

#  Define color for bars
color_bias <- "#228B22"  # Sea green

# Create the horizontal bar plot with 95% CI error bars
p_bias_side <- ggplot(emm_bias_df, aes(y = BiasSide, x = emmean)) +
  geom_col(fill = color_bias, alpha = 0.7, width = 0.6) +
  geom_errorbar(aes(xmin = lower.CL, xmax = upper.CL),
                width = 0.2, color = "black", linewidth = 0.6) +
  geom_point(shape = 15,
             size = 3, color = "black", fill = "white") +
  geom_text(aes(x = upper.CL + max(upper.CL, na.rm = TRUE) * 0.02,
                label = round(emmean, 3)),
            hjust = 0, size = 3.7, family = "Avenir") +
  scale_x_continuous(
    name = "Post-Interaction Performance",
    labels = scales::number_format(accuracy = 0.01),
    limits = c(0, max(emm_bias_df$upper.CL) * 1.05),
    expand = c(0, 0)
  ) +

  scale_y_discrete(
    name = "AI Bias Direction",
    labels = c("Opposite" = "Opposition Bias", "Same" = "Echo-Chamber Bias")
  ) +

  coord_cartesian(xlim = c(0.5, .8)) +
  
  theme_classic() +
  theme(
    text = element_text(family = "Avenir"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.line = element_blank(),
    axis.title.x = element_text(family = "Avenir", size = 12, margin = margin(t = 12)),
    axis.title.y = element_text(family = "Avenir", size = 12, color = "black", margin = margin(r = 12)),
    axis.text.x = element_text(family = "Avenir", size = 10, color = "black"),
    axis.text.y = element_text(family = "Avenir", size = 10, color = "black", angle = 90, hjust = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.4),
    axis.ticks.length = unit(3, "pt"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    plot.margin = margin(t = 15, r = 20, b = 15, l = 15),
    plot.title = element_text(family = "Avenir", size = 14, hjust = 0.5, margin = margin(b = 20))
  )

# Display the plot
print(p_bias_side)

# =============================================================================
# Visualization: echo/opposition x user party (the four combination cells)
# =============================================================================
emm_full_df <- as.data.frame(emm_full)

# Party colors for the user's party (Republican = red, Democrat = blue)
party_colors <- c("Republican" = "#E15759", "Democrat" = "#4E79A7")

# Reference: average post-interaction performance in the Default AI arm.
# (repdem_single_ai excludes Default, so compute it from single_ai_processed_;
# raw mean, since Default rows are not in per_model.)
default_avg <- mean(
  single_ai_processed_$PostPerformance[single_ai_processed_$BiasedType == "Default"],
  na.rm = TRUE
)
cat(sprintf("Default AI arm mean PostPerformance (reference line): %.3f\n", default_avg))

dodge <- position_dodge(width = 0.7)

p_bias_party <- ggplot(emm_full_df,
                       aes(y = BiasSide, x = emmean,
                           fill = UStanceLabel_S, group = UStanceLabel_S)) +
  geom_col(position = dodge, width = 0.6, alpha = 0.85) +
  # Default AI treatment average as reference
  geom_vline(xintercept = default_avg, linetype = "dashed",
             color = "gray70", linewidth = 0.5) +
  geom_errorbar(aes(xmin = lower.CL, xmax = upper.CL),
                position = dodge, width = 0.2, color = "black", linewidth = 0.6) +
  geom_text(aes(x = upper.CL + max(upper.CL, na.rm = TRUE) * 0.03,
                label = round(emmean, 3)),
            position = dodge, hjust = 0, size = 4., family = "Avenir") +
  scale_fill_manual(name = "User party", values = party_colors) +
  scale_x_continuous(
    name = "Post-Interaction Performance",
    labels = scales::number_format(accuracy = 0.01)
  ) +
  scale_y_discrete(
    name = "AI Bias Direction",
    labels = c("Opposite" = "Opposition Bias", "Same" = "Echo-Chamber Bias")
  ) +
  coord_cartesian(xlim = c(0.5, 1)) +   # widen if any bar/CI is clipped
  theme_classic() +
  theme(
    text = element_text(family = "Avenir"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.line = element_blank(),
    axis.title.x = element_text(family = "Avenir", size = 12, margin = margin(t = 12)),
    axis.title.y = element_text(family = "Avenir", size = 12, color = "black", margin = margin(r = 12)),
    axis.text.x = element_text(family = "Avenir", size = 10, color = "black"),
    axis.text.y = element_text(family = "Avenir", size = 10, color = "black", hjust = 0.5, angle = 90),
    axis.ticks = element_line(color = "black", linewidth = 0.4),
    axis.ticks.length = unit(3, "pt"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    plot.margin = margin(t = 15, r = 20, b = 15, l = 15),
    legend.position = "none"
  )

# Display the 4-cell plot
print(p_bias_party)
ggsave("../figures/relative_bias_misinfo.png", p_bias_party, width = 5, height = 5, dpi = 500)
