repdem_single_ai <- single_ai_processed_ %>%
  filter(
    (UStanceLabel_S != "Independent") & (BiasedType == "Biased")
  )

repdem_single_ai$BiasSide <- ifelse(((repdem_single_ai$AIStanceLabel_S == "Republican") &
                                      (repdem_single_ai$UStanceLabel_S == "Democrat")) |
                                      ((repdem_single_ai$AIStanceLabel_S == "Democrat") &
                                         (repdem_single_ai$UStanceLabel_S == "Republican")), "Opposite", "Same")

table(repdem_single_ai$BiasSide)
repdem_single_ai$BiasSide <-factor(repdem_single_ai$BiasSide, levels = c("Same", "Opposite"))

repdem_single_ai$PerceivedImproveCode <- as.numeric(repdem_single_ai$PerceivedImproveCode)

per_model <- lm(PostPerformance ~ BiasSide + PrePerformance + as.factor(NID) + UStanceLabel +
                  UIdeo + AICorrectness,
                  data = repdem_single_ai)
summary(per_model)
vcov_per <- vcovCL(per_model, cluster = repdem_single_ai$UID)
clustered_results_per <- coeftest(per_model, vcov = vcov_per)
print(clustered_results_per)
df.residual(per_model)
r2(per_model)

per_model <- lmer(PostPerformance ~ BiasSide + PrePerformance + as.factor(NID) + 
                    (1|UID) + UStanceLabel + UIdeo + AICorrectness,
                        data = repdem_single_ai)
summary(per_model)
summary(per_model)$sigma
df.residual(per_model)
r2(per_model)

emm_full <- emmeans(per_model, ~ BiasSide * UStanceLabel_S)
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
df_bias <- df_clustered  # Use clustered df
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

# Create the bar plot with 95% CI error bars
p_bias_side <- ggplot(emm_bias_df, aes(x = BiasSide, y = emmean)) +
  geom_col(fill = color_bias, alpha = 0.7, width = 0.6) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), 
                width = 0.2, color = "black", linewidth = 0.6) +
  geom_point(shape = 20, 
             size = 3, color = "black", fill = "white") +
  geom_text(aes(y = upper.CL + max(upper.CL, na.rm = TRUE) * 0.02, 
                label = round(emmean, 3)), 
            size = 3.7, family = "Avenir") +
  scale_y_continuous(
    name = "Post-Interaction Performance",
    labels = scales::number_format(accuracy = 0.01),
    limits = c(0, max(emm_bias_df$upper.CL) * 1.05),
    expand = c(0, 0)
  ) +
  
  scale_x_discrete(
    name = "AI Bias Direction",
    labels = c("Opposite" = "Opposition Bias", "Same" = "Echo-Chamber Bias")
  ) +
  
  coord_cartesian(ylim = c(0.5, .8)) +
  
  theme_classic() +
  theme(
    text = element_text(family = "Avenir"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.line = element_blank(),
    axis.title.x = element_text(family = "Avenir", size = 12, margin = margin(t = 12)),
    axis.title.y = element_text(family = "Avenir", size = 12, color = "black", margin = margin(r = 12)),
    axis.text.x = element_text(family = "Avenir", size = 10, color = "black"),
    axis.text.y = element_text(family = "Avenir", size = 10, color = "black"),
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

