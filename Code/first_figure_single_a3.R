# ========================================
# Conversation analysis
# ========================================
# Calculate conversation length statistics by BiasedType
single_ai_processed_$BiasedType <- factor(single_ai_processed_$BiasedType,
                                        levels = c("Non-Biased", "Biased"))

conv_length_stats <- single_ai_processed_ %>%
  group_by(BiasedType) %>%
  summarise(
    Mean_ConvLength = mean(ConvLength, na.rm = TRUE),
    SD_ConvLength = sd(ConvLength, na.rm = TRUE),
    Count = n(),
    SE_ConvLength = SD_ConvLength / sqrt(Count),
    CI_95_Lower = Mean_ConvLength - 1.96 * SE_ConvLength,
    CI_95_Upper = Mean_ConvLength + 1.96 * SE_ConvLength,
    .groups = 'drop'
  )

cat("=== CONVERSATION LENGTH STATISTICS ===\n")
print(conv_length_stats)

# ========================================
# Regression analysis
# ========================================
conv_length_model <- lm(ConvLength ~ BiasedType + PrePerformance + C(NID) +
                          UIdeo + AICorrectness + UStanceLabel,
                        data = single_ai_processed_)

# Calculate clustered variance-covariance matrix by UID
vcov_clustered_conv <- vcovCL(conv_length_model, cluster = single_ai_processed_$UID)

# Get coefficients with clustered standard errors
clustered_results_conv <- coeftest(conv_length_model, vcov = vcov_clustered_conv)

cat("\n=== CONVERSATION LENGTH MODEL WITH CLUSTERED SEs ===\n")
print(clustered_results_conv)
summary(conv_length_model)

# ========================================
# Regression analysis -- mixed effects
# ========================================
conv_length_model <- lmer(ConvLength ~ BiasedType + PrePerformance + C(NID) + (1|UID),
                        data = single_ai_processed_)
summary(conv_length_model)
summary(conv_length_model)$sigma
df.residual(conv_length_model)
r2(conv_length_model)

# ========================================
# Effect size
# ========================================
model_sigma <- sigma(conv_length_model)
model_df <- df.residual(conv_length_model)

marginal_means <- emmeans(conv_length_model, ~ BiasedType)
marginal_comparison <- pairs(marginal_means)

marginal_contrast_summary <- summary(marginal_comparison, infer = TRUE)
marginal_estimate <- marginal_contrast_summary$estimate
marginal_se <- marginal_contrast_summary$SE
marginal_df <- marginal_contrast_summary$df

# Calculate Hedges' g manually
marginal_cohens_d <- marginal_estimate / model_sigma
marginal_hedges_g <- marginal_cohens_d * J
marginal_hedges_se <- (marginal_se / model_sigma) * J

# Get tidy output for easier interpretation
tidy_conv_results_fmt <- broom::tidy(clustered_results_conv, conf.int = TRUE) %>% 
  mutate(across(where(is.numeric), ~ sprintf("%.3f", .x)))

print(tidy_conv_results_fmt, row.names = FALSE)

# Get model-adjusted means using emmeans
emm_conv_length <- emmeans(conv_length_model, "BiasedType", vcov. = vcov_clustered_conv)

# Convert to dataframe for plotting
conv_length_emmeans <- as.data.frame(emm_conv_length) %>%
  rename(
    Mean_ConvLength = emmean,
    SE_ConvLength = SE
  ) %>%
  mutate(
    CI_95_Lower = Mean_ConvLength - 1.96 * SE_ConvLength,
    CI_95_Upper = Mean_ConvLength + 1.96 * SE_ConvLength
  )

cat("\n=== MODEL-ADJUSTED CONVERSATION LENGTH ===\n")
print(conv_length_emmeans)

# Extract BiasedType coefficient for summary
biased_coef <- tidy_conv_results[tidy_conv_results$term == "BiasedTypeBiased", ]
if(nrow(biased_coef) > 0) {
  cat("\n=== CONVERSATION LENGTH SUMMARY ===\n")
  cat("Difference (Biased - Non-Biased):", round(biased_coef$estimate, 3), "± SE:", round(biased_coef$std.error, 3), "\n")
  cat("p-value:", round(biased_coef$p.value, 4), "\n")
  cat("95% CI: [", round(biased_coef$conf.low, 3), ",", round(biased_coef$conf.high, 3), "]\n")
}

# ========================================
# Engagement analysis
# ========================================
single_ai_processed_$BiasedType <- factor(single_ai_processed_$BiasedType,
                                          levels = c("Non-Biased", "Biased"))

dimensions <- c("EngagementBehavioral", "EngagementCognitive", "EngagementEmotional", 
                "EngagementAutonomy", "EngagementSocialPresence")
dimension_names <- c("Behavioral", "Cognitive", "Emotional", "Autonomy", "Social Presence")

# Create a comprehensive results table with clustered standard errors
engagement_results <- data.frame()

for(i in 1:length(dimensions)) {
  cat(sprintf("\n--- %s Engagement ---\n", dimension_names[i]))
  
  # Mixed effects model with UID random intercept and NID fixed effect
  engagement_model <- lmer(
    formula = paste(dimensions[i], "~ BiasedType + as.factor(NID) + PrePerformance + (1|UID) + as.factor(UStanceLabel)"),
    data = single_ai_processed_
  )
  
  # Get model summary with proper degrees of freedom
  model_summary <- summary(engagement_model)
  
  # Extract results using tidy
  tidy_results <- tidy(engagement_model, effects = "fixed", conf.int = TRUE)
  biased_coef <- tidy_results[tidy_results$term == "BiasedTypeBiased", ]
  
  # Calculate means for each group for reporting
  biased_mean <- mean(single_ai_processed_[[dimensions[i]]][single_ai_processed_$BiasedType == "Biased"], na.rm = TRUE)
  nonbiased_mean <- mean(single_ai_processed_[[dimensions[i]]][single_ai_processed_$BiasedType == "Non-Biased"], na.rm = TRUE)
  
  # Store results
  if(nrow(biased_coef) > 0) {
    engagement_results <- rbind(engagement_results, data.frame(
      Dimension = dimension_names[i],
      Biased_Mean = round(biased_mean, 3),
      NonBiased_Mean = round(nonbiased_mean, 3),
      Difference = round(biased_coef$estimate, 3),
      Std_Error = round(biased_coef$std.error, 3),
      df = round(biased_coef$df, 1),
      t_statistic = round(biased_coef$statistic, 3),
      p_value = round(biased_coef$p.value, 4),
      CI_lower = round(biased_coef$conf.low, 3),
      CI_upper = round(biased_coef$conf.high, 3),
      Significance = case_when(
        biased_coef$p.value < 0.01 ~ "***",
        biased_coef$p.value < 0.05 ~ "**", 
        biased_coef$p.value < 0.1 ~ "*",
        TRUE ~ ""
      )
    ))
    
    # Print individual results
    cat(sprintf("  Biased AI: M = %.3f\n", biased_mean))
    cat(sprintf("  Non-Biased AI: M = %.3f\n", nonbiased_mean))
    cat(sprintf("  Difference (Biased - Non-Biased): %.3f ± %.3f\n", biased_coef$estimate, biased_coef$std.error))
    cat(sprintf("  t(%.1f) = %.3f, p = %.4f %s\n", 
                biased_coef$df, biased_coef$statistic, biased_coef$p.value,
                case_when(
                  biased_coef$p.value < 0.01 ~ "***",
                  biased_coef$p.value < 0.05 ~ "**", 
                  biased_coef$p.value < 0.1 ~ "*",
                  TRUE ~ ""
                )))
    cat(sprintf("  95%% CI: [%.3f, %.3f]\n", biased_coef$conf.low, biased_coef$conf.high))
    
    # Print random effects variance
    random_effects_var <- as.data.frame(VarCorr(engagement_model))
    uid_var <- random_effects_var[random_effects_var$grp == "UID", "vcov"]
    residual_var <- random_effects_var[random_effects_var$grp == "Residual", "vcov"]
    icc <- uid_var / (uid_var + residual_var)
    cat(sprintf("  ICC (UID): %.3f\n", icc))
  }
}

# Print comprehensive summary table
cat("\n=== ENGAGEMENT RESULTS SUMMARY ===\n")
print(engagement_results, digits = 3)

# Multiple comparison adjustments
if(nrow(engagement_results) > 0) {
  # Calculate different adjustments for reference
  engagement_results$p_bonferroni <- round(p.adjust(engagement_results$p_value, method = "bonferroni"), 4)
  engagement_results$p_fdr <- round(p.adjust(engagement_results$p_value, method = "fdr"), 4)
  
  cat("\n=== MULTIPLE COMPARISON ADJUSTMENTS (REFERENCE ONLY) ===\n")
  print(engagement_results[, c("Dimension", "p_value", "p_bonferroni", "p_fdr")], digits = 3)
  
  # Count significant results
  n_significant_raw <- sum(engagement_results$p_value < 0.1)
  n_significant_bonferroni <- sum(engagement_results$p_bonferroni < 0.1)
  n_significant_fdr <- sum(engagement_results$p_fdr < 0.1)
  
  cat(sprintf("\nENGAGEMENT ANALYSIS SUMMARY:\n"))
  cat(sprintf("- %d out of %d dimensions show significant differences (uncorrected p < 0.05)\n", 
              n_significant_raw, length(dimensions)))
  cat(sprintf("- %d out of %d would remain significant with Bonferroni correction\n", 
              n_significant_bonferroni, length(dimensions)))
  cat(sprintf("- %d out of %d would remain significant with FDR correction\n", 
              n_significant_fdr, length(dimensions)))
  
  if(n_significant_raw > 0) {
    significant_dims <- engagement_results$Dimension[engagement_results$p_value < 0.1]
    cat(sprintf("- Significant dimensions (uncorrected): %s\n", paste(significant_dims, collapse = ", ")))
  }
}

# ========================================
# Visualization
# ========================================
# Define colors and theme
bias_colors <- c(
  "Biased" = "#E15759",      
  "Non-Biased" = "#4E79A7"   
)

nature_theme <- theme_classic() +
  theme(
    text = element_text(family = "Avenir", size = 8),
    plot.title = element_text(family = "Avenir", size = 10, face = "bold", hjust = 0),
    axis.title = element_text(family = "Avenir", size = 9, face = "plain"),
    axis.text = element_text(family = "Avenir", size = 8, color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 10)
  )

# Create conversation length plot using model-adjusted means
conv_length_plot_data <- conv_length_emmeans %>%
  mutate(y_position = ifelse(BiasedType == "Biased", 2, 1))

conv_length_plot <- ggplot(conv_length_plot_data, aes(x = Mean_ConvLength, y = BiasedType, fill = BiasedType)) +
  geom_col(alpha = 0.8, width = 0.85) +
  geom_errorbar(aes(xmin = CI_95_Lower, xmax = CI_95_Upper), 
                width = 0.4, size = 0.5) +
  geom_text(aes(x = CI_95_Upper + max(Mean_ConvLength) * 0.05, 
                label = round(Mean_ConvLength, 1)), 
            size = 3.3, family = "Avenir", hjust = 0) +
  scale_fill_manual(values = bias_colors) +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.1), 
                     expand = expansion(mult = c(0, 0.15))) +
  labs(
    y = NULL,
    x = "Conversation Length"
  ) +
  nature_theme + 
  theme(
    legend.position = "none",
    panel.border = element_blank()
  )

print(conv_length_plot)
  
  # Create engagement data for radar plot
  if(nrow(engagement_results) > 0) {
    engagement_plot_data <- engagement_results %>%
      select(Dimension, Biased_Mean, NonBiased_Mean) %>%
      pivot_longer(cols = c(Biased_Mean, NonBiased_Mean), 
                   names_to = "BiasedType_raw", 
                   values_to = "Score") %>%
      mutate(
        BiasedType = case_when(
          BiasedType_raw == "Biased_Mean" ~ "Biased",
          BiasedType_raw == "NonBiased_Mean" ~ "Non-Biased"
        ),
        Dimension = factor(Dimension, levels = c("Behavioral", "Cognitive", "Emotional", 
                                                 "Autonomy", "Social Presence"))
      )
    
    # Calculate distribution statistics for each dimension (pooling both groups)
    dimension_stats <- single_ai_processed_ %>%
      select(BiasedType, EngagementBehavioral, EngagementCognitive, EngagementEmotional, 
             EngagementAutonomy, EngagementSocialPresence) %>%
      pivot_longer(cols = -BiasedType, names_to = "Dimension_raw", values_to = "Score") %>%
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
    
    # Apply proper scaling using overall distribution statistics
    engagement_radar <- engagement_plot_data %>%
      left_join(dimension_stats, by = "Dimension") %>%
      mutate(
        # Z-score normalization: (value - overall_mean) / overall_sd
        z_score = ifelse(sd_score > 0, 
                         (Score - mean_score) / sd_score, 
                         0),  # If no variation, set to 0
        # Aggressive scaling for maximum separation: center at 4, scaling factor of 8
        Score_normalized = pmax(0.5, 4 + z_score * 8),
        Dimension = factor(Dimension, levels = c("Behavioral", "Cognitive", "Emotional", 
                                                 "Autonomy", "Social Presence"))
      )
    
    # Add radar chart coordinates with proper ordering
    n_dimensions <- length(unique(engagement_radar$Dimension))
    angles <- seq(0, 2*pi, length.out = n_dimensions + 1)[1:n_dimensions]
    
    engagement_radar <- engagement_radar %>%
      arrange(BiasedType, Dimension) %>%  # Ensure proper ordering
      group_by(BiasedType) %>%
      mutate(
        angle = angles,  # Each group gets the same angles
        x = Score_normalized * cos(angle),
        y = Score_normalized * sin(angle)
      ) %>%
      ungroup()
    
    # Define radar colors
    radar_colors <- c(
      "Biased" = "#E15759",      # Red for biased
      "Non-Biased" = "#4E79A7"   # Blue for non-biased
    )
    
    # Create dimension labels
    dimension_labels <- data.frame(
      Dimension = factor(c("Behavioral", "Cognitive", "Emotional", "Autonomy", "Social Presence"),
                         levels = c("Behavioral", "Cognitive", "Emotional", "Autonomy", "Social Presence")),
      label_text = c("Behavioral", "Cognitive", "Emotional", "Autonomy", "Social Presence")
    )
    
    # Create radar plot
    radar_plot <- ggplot(engagement_radar, aes(x = x, y = y, color = BiasedType, fill = BiasedType, group = BiasedType)) +
      # Add concentric circles
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
      
      # Add axis lines
      annotate("segment", x = 0, y = 0, 
               xend = 9 * cos(angles), yend = 9 * sin(angles),
               color = "gray60", size = 0.3) +
      
      # Add the radar polygon for each group
      geom_polygon(alpha = 0.2, size = 1.2) +
      geom_point(size = 3, alpha = 0.8) +
      
      # Add dimension labels
      annotate("text", x = 9.5 * cos(angles), y = 9.5 * sin(angles),
               label = dimension_labels$label_text,
               hjust = 0.5, vjust = 0.5,
               size = 3.2, family = "Avenir") +
      
      # Add scale reference
      annotate("text", x = c(-0.3, -0.3, -0.3), y = c(1, 4, 8),
               label = c("-1SD", "Mean", "+1SD"),
               size = 2.8, family = "Avenir", color = "gray60", hjust = 1) +
      
      # Styling
      scale_color_manual(values = radar_colors) +
      scale_fill_manual(values = radar_colors) +
      coord_fixed() +
      xlim(-12, 12) + ylim(-12, 12) +
      theme_void() +
      theme(
        legend.position = "none",
        legend.text = element_text(family = "Avenir", size = 8),
        plot.margin = margin(0, 0, 0, 0),
        panel.background = element_rect(fill = "white", color = NA)
      )
    
    print(radar_plot)
  }
