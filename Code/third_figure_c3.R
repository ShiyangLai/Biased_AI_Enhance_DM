per_model <- lm(ConvLength ~ BiasSide + PrePerformance + as.factor(NID) +
                  UStanceLabel, # UIdeo + AICorrectness
                data = repdem_single_ai,
                na.action = na.omit)
summary(per_model)
vcov_per <- vcovCL(per_model, cluster = repdem_single_ai$UID)
clustered_results_per <- coeftest(per_model, vcov = vcov_per)
print(clustered_results_per)
df.residual(per_model)
r2(per_model)


per_model <- lmer(ConvLength ~ BiasSide + PrePerformance + UStanceLabel +
                  as.factor(NID) + (1|UID), # UIdeo + AICorrectness
                  data = repdem_single_ai,
                  na.action = na.omit)

summary(per_model)
df.residual(per_model)
r2(per_model)

# Using emmeans for mixed effects model
emm_bias <- emmeans(per_model, ~ BiasSide)
emm_bias_df <- as.data.frame(emm_bias)

# Pairwise comparison using emmeans
contrast_bias <- contrast(emm_bias, "pairwise")
contrast_bias_df <- as.data.frame(contrast_bias)

# Hedges' g calculation
hedges_g_calculation <- function(model, contrast_result) {
  
  # Extract the difference from contrast
  diff <- summary(contrast_result)$estimate[1]
  
  if (inherits(model, "lmerMod")) {
    cat("Using mixed effects model variance components...\n")
    
    # Extract variance components for mixed effects model
    variance_components <- as.data.frame(VarCorr(model))
    
    # Random effect variance (between-subject)
    sigma_u <- variance_components$vcov[variance_components$grp == "UID"]
    
    # Residual variance (within-subject)
    sigma_e <- variance_components$vcov[variance_components$grp == "Residual"]
    
    # Total variance (for effect size calculation)
    sigma_total <- sqrt(sigma_u + sigma_e)
    
    # Calculate Hedges' g
    hedges_g <- diff / sigma_total
    
    # Degrees of freedom for mixed effects
    df_contrast <- df.residual(model)
    
    results <- data.frame(
      Comparison = "Same - Opposite",
      Mean_Difference = round(diff, 4),
      Sigma_Random = round(sqrt(sigma_u), 4),
      Sigma_Residual = round(sqrt(sigma_e), 4),
      Sigma_Total = round(sigma_total, 4),
      Hedges_g_raw = round(hedges_g, 4),
      Model_Type = "Mixed Effects"
    )
    
  } else {
    cat("Using linear model residual standard error...\n")
    
    # For linear model, use residual standard error
    sigma_residual <- summary(model)$sigma
    
    # Calculate Hedges' g using residual SE
    hedges_g <- diff / sigma_residual
    
    # Degrees of freedom for linear model
    df_contrast <- df.residual(model)
    
    results <- data.frame(
      Comparison = "Same - Opposite",
      Mean_Difference = round(diff, 4),
      Sigma_Residual = round(sigma_residual, 4),
      Hedges_g_raw = round(hedges_g, 4),
      Model_Type = "Linear Model"
    )
  }
  
  # Apply small sample correction
  correction_factor <- 1 - (3 / (4 * df_contrast - 1))
  hedges_g_corrected <- hedges_g * correction_factor
  
  # Add corrected values to results
  results$Hedges_g_corrected <- round(hedges_g_corrected, 4)
  results$df <- df_contrast
  
  # Interpretation
  interpretation <- ifelse(abs(hedges_g_corrected) < 0.2, "negligible",
                           ifelse(abs(hedges_g_corrected) < 0.5, "small",
                                  ifelse(abs(hedges_g_corrected) < 0.8, "medium", "large")))
  results$Effect_Size <- interpretation
  
  cat("\n=== HEDGES' G CALCULATION FOR CONVERSATION LENGTH ===\n")
  print(results)
  
  return(results)
}

# Calculate Hedges' g using emmeans contrast result
hedges_result <- hedges_g_calculation(per_model, contrast_bias)

# Print results
cat("\n=== EMMEANS RESULTS FOR BIASSIDE (CONVERSATION LENGTH) ===\n")
cat("\nConversation Length by Bias Side:\n")
print(emm_bias_df)

cat("\n=== PAIRWISE COMPARISON (Mixed Effects) ===\n")
print(contrast_bias_df)

# Add bias_side_numeric for plotting (Opposite = 0, Same = 1)
emm_bias_df <- emm_bias_df %>%
  mutate(bias_side_numeric = case_when(
    BiasSide == "Opposite" ~ 0,
    BiasSide == "Same" ~ 1,
    TRUE ~ NA_real_
  ))

# Add value level indicator for plotting
emm_bias_df <- emm_bias_df %>%
  mutate(value_level = ifelse(emmean == max(emmean), "Higher", "Lower"))


# Define colors (brown family for conversation length)
color_bias <- "#8B4513"      # Saddle brown

# Use degrees of freedom from emmeans (instead of df_clustered)
df_bias <- emm_bias_df$df[1]  # emmeans provides df for each estimate
t_90 <- qt(0.95, df_bias)   # 90% CI
t_95 <- qt(0.975, df_bias)  # 95% CI
t_99 <- qt(0.995, df_bias)  # 99% CI

# Define color gradients (brown family)
color_bias_90 <- "#CD853F"   # Peru 
color_bias_95 <- "#DEB887"   # Burlywood
color_bias_99 <- "#F5F5DC"   # Beige

# Add multiple confidence intervals using emmeans SE
emm_bias_multi <- emm_bias_df %>%
  mutate(
    # 90% CI
    lower.CL_90 = emmean - SE * t_90,
    upper.CL_90 = emmean + SE * t_90,
    # 95% CI (rename emmeans output for consistency)
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

# Prepare plotting data
bias_plot_data <- emm_bias_df %>%
  dplyr::select(BiasSide, emmean, SE, lower.CL, upper.CL) %>%
  mutate(
    Mean_ConvLength = emmean,
    CI_95_Lower = lower.CL,
    CI_95_Upper = upper.CL
  )

# Define colors for bars (brown/red family for conversation length)
bias_colors <- c("Opposite" = "#CC0000", "Same" = "#FF7F7F")  # Dark red and light red

# Define a basic theme (assuming nature_theme might not be available)
# If you have nature_theme defined elsewhere, you can replace this
basic_theme <- theme_classic() +
  theme(
    text = element_text(family = "Avenir"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.line = element_blank(),
    axis.title = element_text(family = "Avenir", size = 12, face = "plain"),
    axis.text = element_text(family = "Avenir", size = 10, color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 0.4),
    axis.ticks.length = unit(3, "pt"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid = element_blank()
  )

# Create vertical bar plot for conversation length
bias_side_vertical <- ggplot(bias_plot_data, aes(x = BiasSide, y = Mean_ConvLength, fill = BiasSide)) +
  # Clean vertical bars
  geom_col(alpha = 0.8, width = 0.7) +
  # Error bars (vertical)
  geom_errorbar(aes(ymin = CI_95_Lower, ymax = CI_95_Upper), 
                width = 0.3, linewidth = 0.6, color = "black") +
  # Value labels positioned above error bars
  geom_text(aes(y = CI_95_Upper + max(Mean_ConvLength) * 0.05, 
                label = round(Mean_ConvLength, 1)), 
            size = 3.5, family = "Avenir", vjust = 0, color = "black") +
  scale_fill_manual(values = bias_colors) +
  scale_y_continuous(
    labels = scales::label_number(accuracy = 0.1), 
    expand = expansion(mult = c(0, 0.12)),
    limits = c(0, 35)
  ) +
  scale_x_discrete(
    labels = c("Opposite" = "Opposition\nBias", "Same" = "Echo-Chamber\nBias")
  ) +
  labs(
    x = "AI Bias Direction",
    y = "Conversation Length"
  ) +
  nature_theme +
  theme(
    legend.position = "none",
    # axis.ticks.x = element_blank
    axis.line.x.bottom = element_line(color = "black", linewidth = 0.5),  # Bottom border only
    axis.line.y.left = element_line(color = "black", linewidth = 0.5),    # Left border only
    axis.line.x.top = element_blank(),    # Remove top border
    axis.line.y.right = element_blank(),  # Remove right border
    panel.border = element_blank(),
    axis.title = element_text(family = "Avenir", size = 12, face = "plain"),
    axis.text.x = element_text(family = "Avenir", size = 10, color = "black", face = "plain"),
    axis.title.x = element_text(margin = margin(t=8)),
    axis.title.y = element_text(margin = margin(r=8)),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )

bias_side_vertical

# ===================================================================
# Engagement analysis by BiasSide (Opposite vs Same)
# ===================================================================
# Calculate mean engagement scores by BiasSide
engagement_bias_data <- repdem_single_ai %>%
  group_by(BiasSide) %>%
  summarise(
    Behavioral = mean(EngagementBehavioral, na.rm = TRUE),
    Cognitive = mean(EngagementCognitive, na.rm = TRUE),
    Emotional = mean(EngagementEmotional, na.rm = TRUE),
    Autonomy = mean(EngagementAutonomy, na.rm = TRUE),
    SocialPresence = mean(EngagementSocialPresence, na.rm = TRUE),
    .groups = 'drop'
  )

# Print the engagement statistics
cat("\n=== ENGAGEMENT STATISTICS BY BIAS SIDE ===\n")
print(engagement_bias_data)

# Transform data for radar chart
engagement_bias_long <- engagement_bias_data %>%
  pivot_longer(cols = -BiasSide, 
               names_to = "Dimension", 
               values_to = "Score") %>%
  mutate(
    # Create more readable dimension names
    Dimension = case_when(
      Dimension == "Behavioral" ~ "Behavioral",
      Dimension == "Cognitive" ~ "Cognitive", 
      Dimension == "Emotional" ~ "Emotional",
      Dimension == "Autonomy" ~ "Autonomy",
      Dimension == "SocialPresence" ~ "Social Presence"
    ),
    # Create factor for proper ordering
    Dimension = factor(Dimension, levels = c("Behavioral", "Cognitive", "Emotional", 
                                             "Autonomy", "Social Presence"))
  )

# Add angles for radar chart positioning
n_dimensions <- length(unique(engagement_bias_long$Dimension))
angles <- seq(0, 2*pi, length.out = n_dimensions + 1)[1:n_dimensions]

engagement_bias_radar <- engagement_bias_long %>%
  mutate(
    angle = rep(angles, times = 2),  # 2 groups (Opposite, Same)
    x = Score * cos(angle),
    y = Score * sin(angle)
  )

# Create color palette for BiasSide
radar_bias_colors <- c(
  "Opposite" = "#CC0000",    # Muted red/pink for opposite
  "Same" = "#FF7F7F"         # Muted blue for same
)

# Calculate distribution statistics for each dimension within repdem_single_ai
dimension_bias_stats <- repdem_single_ai %>%
  select(BiasSide, EngagementBehavioral, EngagementCognitive, EngagementEmotional, 
         EngagementAutonomy, EngagementSocialPresence) %>%
  pivot_longer(cols = -BiasSide, names_to = "Dimension_raw", values_to = "Score") %>%
  mutate(
    Dimension = case_when(
      Dimension_raw == "EngagementBehavioral" ~ "Behavioral",
      Dimension_raw == "EngagementCognitive" ~ "Cognitive", 
      Dimension_raw == "EngagementEmotional" ~ "Emotional",
      Dimension_raw == "EngagementAutonomy" ~ "Autonomy",
      Dimension_raw == "EngagementSocialPresence" ~ "Social Presence"
    )
  ) %>%
  group_by(Dimension) %>%
  summarise(
    mean_score = mean(Score, na.rm = TRUE),
    sd_score = sd(Score, na.rm = TRUE),
    min_score = min(Score, na.rm = TRUE),
    max_score = max(Score, na.rm = TRUE),
    .groups = 'drop'
  )

# Apply aggressive scaling to maximize separation between groups
engagement_bias_normalized <- engagement_bias_long %>%
  left_join(dimension_bias_stats, by = "Dimension") %>%
  mutate(
    # Z-score normalization: (value - mean) / sd
    z_score = ifelse(sd_score > 0, 
                     (Score - mean_score) / sd_score, 
                     0),  # If no variation, set to 0
    # Aggressive scaling for maximum separation: center at 4, scaling factor of 8
    Score_normalized = pmax(0.5, 4 + z_score * 8),
    Dimension = factor(Dimension, levels = c("Behavioral", "Cognitive", "Emotional", 
                                             "Autonomy", "Social Presence"))
  ) %>%
  mutate(
    angle = rep(angles, times = 2),  # 2 groups (Opposite, Same)
    x = Score_normalized * cos(angle),
    y = Score_normalized * sin(angle)
  )

# Create labels for each dimension
dimension_bias_labels <- dimension_bias_stats %>%
  mutate(
    Dimension = factor(Dimension, levels = c("Behavioral", "Cognitive", "Emotional", 
                                             "Autonomy", "Social Presence")),
    label_text = as.character(Dimension)  # Just the dimension names
  ) %>%
  arrange(Dimension)

# Create the radar chart with aggressive scaling for maximum separation
radar_bias_plot <- ggplot(engagement_bias_normalized, aes(x = x, y = y, color = BiasSide, fill = BiasSide)) +
  # Add concentric circles that match the aggressive scaling (center=4, factor=8)
  annotate("path", 
           x = 1 * cos(seq(0, 2*pi, length.out = 100)), 
           y = 1 * sin(seq(0, 2*pi, length.out = 100)),
           color = "gray80", size = 0.3) +
  annotate("path", 
           x = 4 * cos(seq(0, 2*pi, length.out = 100)), 
           y = 4 * sin(seq(0, 2*pi, length.out = 100)),
           color = "gray80", size = 0.3) +
  annotate("path", 
           x = 8 * cos(seq(0, 2*pi, length.out = 100)), 
           y = 8 * sin(seq(0, 2*pi, length.out = 100)),
           color = "gray80", size = 0.3) +
  
  # Add axis lines from center to max radius
  annotate("segment", x = 0, y = 0, 
           xend = 9 * cos(angles), yend = 9 * sin(angles),
           color = "gray60", size = 0.3) +
  
  # Add the radar polygon for each group
  geom_polygon(alpha = 0.2, size = 1.2) +
  geom_point(size = 3, alpha = 0.8) +
  
  # Add dimension labels
  annotate("text", x = 9.5 * cos(angles), y = 9.5 * sin(angles),
           label = dimension_bias_labels$label_text,
           hjust = 0.5,
           vjust = 0.5,
           size = 3.2, family = "Avenir") +
  
  # Add scale reference with correct positions for aggressive scaling
  annotate("text", x = c(-0.3, -0.3, -0.3), y = c(1, 4, 8),
           label = c("-1SD", "Mean", "+1SD"),
           size = 2.8, family = "Avenir", color = "gray60", hjust = 1) +
  
  # Styling with appropriate limits for aggressive separation
  scale_color_manual(values = radar_bias_colors, name = "AI Bias Direction") +
  scale_fill_manual(values = radar_bias_colors, name = "AI Bias Direction") +
  coord_fixed() +
  xlim(-12, 12) + ylim(-12, 12) +  # Appropriate limits for this scaling
  theme_void() +
  theme(
    legend.position = "none",
    legend.title = element_text(family = "Avenir", size = 11),
    legend.text = element_text(family = "Avenir", size = 10),
    plot.margin = margin(10, 10, 10, 10),
    panel.background = element_rect(fill = "white", color = NA)
  )

print(radar_bias_plot)

# ===================================================================
# Engagement analysis with clustered standard errors (BiasSide)
# ===================================================================

cat("\n=== ENGAGEMENT ANALYSIS BY BIAS SIDE WITH CLUSTERED STANDARD ERRORS ===\n")

dimensions <- c("EngagementBehavioral", "EngagementCognitive", "EngagementEmotional", 
                "EngagementAutonomy", "EngagementSocialPresence")
dimension_names <- c("Behavioral", "Cognitive", "Emotional", "Autonomy", "Social Presence")

# Create a comprehensive results table with clustered standard errors
clustered_bias_results <- data.frame()

# Load required library if not already loaded
if (!require(estimatr)) {
  install.packages("estimatr")
  library(estimatr)
}

for(i in 1:length(dimensions)) {
  # Run regression with clustered standard errors using BiasSide
  engagement_bias_model <- lm_robust(
    formula = as.formula(paste(dimensions[i], "~ BiasSide + PrePerformance + as.factor(AIStanceLabel_S) + as.factor(UStanceLabel) + as.factor(NID)")),
    data = repdem_single_ai,
    clusters = UID,
    se_type = "stata"
  )
  
  # Extract results for BiasSide coefficient
  bias_coef <- tidy(engagement_bias_model, conf.int = TRUE) %>%
    filter(term == "BiasSideSame")
  
  # Calculate means for each group for reporting
  same_mean <- mean(repdem_single_ai[[dimensions[i]]][repdem_single_ai$BiasSide == "Same"], na.rm = TRUE)
  opposite_mean <- mean(repdem_single_ai[[dimensions[i]]][repdem_single_ai$BiasSide == "Opposite"], na.rm = TRUE)
  
  # Store results
  clustered_bias_results <- rbind(clustered_bias_results, data.frame(
    Dimension = dimension_names[i],
    Same_Mean = round(same_mean, 3),
    Opposite_Mean = round(opposite_mean, 3),
    Coefficient = round(bias_coef$estimate, 3),
    Std_Error_Clustered = round(bias_coef$std.error, 3),
    t_statistic = round(bias_coef$statistic, 3),
    p_value = round(bias_coef$p.value, 4),
    CI_lower = round(bias_coef$conf.low, 3),
    CI_upper = round(bias_coef$conf.high, 3),
    Significance = case_when(
      bias_coef$p.value < 0.01 ~ "***",
      bias_coef$p.value < 0.05 ~ "**", 
      bias_coef$p.value < 0.1 ~ "*",
      TRUE ~ ""
    )
  ))
  
  # Print individual results
  cat(sprintf("%s Engagement (Clustered SEs):\n", dimension_names[i]))
  cat(sprintf("  Same Bias Direction: M = %.3f\n", same_mean))
  cat(sprintf("  Opposite Bias Direction: M = %.3f\n", opposite_mean))
  cat(sprintf("  Coefficient (Same vs Opposite): %.3f\n", bias_coef$estimate))
  cat(sprintf("  Clustered SE: %.3f\n", bias_coef$std.error))
  cat(sprintf("  t(%.0f) = %.3f, p = %.4f %s\n", 
              bias_coef$df, bias_coef$statistic, bias_coef$p.value,
              case_when(
                bias_coef$p.value < 0.01 ~ "***",
                bias_coef$p.value < 0.05 ~ "**", 
                bias_coef$p.value < 0.1 ~ "*",
                TRUE ~ ""
              )))
  cat(sprintf("  95%% CI: [%.3f, %.3f]\n", bias_coef$conf.low, bias_coef$conf.high))
  cat("\n")
}

# Print summary table
cat("=== CLUSTERED REGRESSION RESULTS SUMMARY (BIAS SIDE) ===\n")
print(clustered_bias_results)

# Multiple comparison adjustment (Bonferroni correction)
adjusted_bias_p_values <- p.adjust(clustered_bias_results$p_value, method = "bonferroni")
clustered_bias_results$p_adjusted = round(adjusted_bias_p_values, 4)
clustered_bias_results$Significance_Adjusted = case_when(
  adjusted_bias_p_values < 0.01 ~ "***",
  adjusted_bias_p_values < 0.05 ~ "**", 
  adjusted_bias_p_values < 0.1 ~ "*",
  TRUE ~ ""
)

cat("\n=== BONFERRONI CORRECTED RESULTS (BIAS SIDE, CLUSTERED) ===\n")
print(clustered_bias_results[, c("Dimension", "Coefficient", "p_value", "p_adjusted", 
                                 "Significance", "Significance_Adjusted")])

# Count significant results
n_significant_raw_bias <- sum(clustered_bias_results$p_value < 0.1)
n_significant_corrected_bias <- sum(clustered_bias_results$p_adjusted < 0.1)

cat(sprintf("\nBIAS SIDE ENGAGEMENT ANALYSIS SUMMARY:\n"))
cat(sprintf("- %d out of %d dimensions show significant differences (uncorrected)\n", 
            n_significant_raw_bias, length(dimensions)))
cat(sprintf("- %d out of %d dimensions remain significant after Bonferroni correction\n", 
            n_significant_corrected_bias, length(dimensions)))

if(n_significant_corrected_bias > 0) {
  significant_dims_bias <- clustered_bias_results$Dimension[clustered_bias_results$p_adjusted < 0.1]
  cat(sprintf("- Significant dimensions: %s\n", paste(significant_dims_bias, collapse = ", ")))
}

# Display clustering information
n_clusters_bias <- length(unique(repdem_single_ai$UID))
cat(sprintf("\nCLUSTERING INFORMATION:\n"))
cat(sprintf("- Standard errors clustered by UID\n"))
cat(sprintf("- Number of clusters: %d\n", n_clusters_bias))
cat(sprintf("- Observations per cluster: %.1f (average)\n", nrow(repdem_single_ai)/n_clusters_bias))

# Create a visual summary table for the engagement differences
engagement_summary_plot_data <- clustered_bias_results %>%
  mutate(
    Direction = ifelse(Coefficient > 0, "Same > Opposite", "Opposite > Same"),
    Abs_Coefficient = abs(Coefficient),
    Significant = ifelse(p_value < 0.05, "Significant", "Non-significant"),
    Dimension_ordered = reorder(Dimension, Abs_Coefficient)
  )

# Create a coefficient plot for engagement differences
engagement_coef_plot <- ggplot(engagement_summary_plot_data, 
                               aes(x = Coefficient, y = Dimension_ordered, 
                                   color = Significant, shape = Significant)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2, alpha = 0.7) +
  geom_point(size = 4, alpha = 0.8) +
  scale_color_manual(values = c("Significant" = "#E15759", "Non-significant" = "#86BCB6")) +
  scale_shape_manual(values = c("Significant" = 16, "Non-significant" = 1)) +
  labs(
    x = "Engagement Difference (Same - Opposite Bias Direction)",
    y = "Engagement Dimension",
    color = "Statistical Significance",
    shape = "Statistical Significance",
    title = "Engagement Differences by AI Bias Direction"
  ) +
  theme_classic() +
  theme(
    text = element_text(family = "Avenir"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.line = element_blank(),
    legend.position = "bottom"
  )

print(engagement_coef_plot)

cat("\n=== ENGAGEMENT ANALYSIS COMPLETE ===\n")
cat("This analysis shows how engagement across five dimensions differs between\n")
cat("'Same' vs 'Opposite' AI bias direction conditions in the repdem_single_ai dataset.\n")

single_ai_processed$BiasedType2 <- ifelse(single_ai_processed$AIStanceLabel_S == "Default", "Default",
                                          ifelse(single_ai_processed$AIStanceLabel_S == "Neutral", "Neutral", "Biased"))
  
per_model_ <- lm(PostPerformance ~ as.factor(BiasedType2),
                data = single_ai_processed)

vcov_clustered <- vcovCL(per_model_, cluster = single_ai_processed$UID)
clustered_results <- coeftest(per_model_, vcov = vcov_clustered)
print(clustered_results)

summary(per_model_)

emm <- emmeans(per_model_, ~ AIStanceLabel_S, vcov. = vcov_clustered)

contrast(emm, list(Biased_vs_NonBiased = c(
  "Democrat" = 0.5,
  "Republican" = 0.5,
  "Neutral" = -0.5,
  "Default" = -0.5
)))
