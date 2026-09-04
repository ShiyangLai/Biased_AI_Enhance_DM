# =============================================================================
# a3 analysis, but with the AI treatment arm SEPARATED into three groups
# (Default = reference, Democrat, Republican) instead of Biased / Non-Biased.
# Applies to BOTH the conversation-length plot and the engagement radar plot.
# Display order across the figures: Republican, Default, Democrat.
# Analysis method is unchanged; models now carry a 3-level BiasedType.
# =============================================================================

# Three-level treatment arm (Default = reference). Neutral treatment excluded.
single_ai_processed_ <- single_ai_processed_ %>%
  dplyr::filter(AIStanceLabel_S %in% c("Default", "Democrat", "Republican")) %>%
  mutate(BiasedType = factor(
    case_when(
      AIStanceLabel_S == "Default"    ~ "Default",
      AIStanceLabel_S == "Democrat"   ~ "Democrat",
      AIStanceLabel_S == "Republican" ~ "Republican"
    ),
    levels = c("Default", "Republican", "Democrat")   # first level = reference
  ))

# ========================================
# Conversation analysis
# ========================================
# Calculate conversation length statistics by BiasedType
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
# Keep the OLS model under its own name: the clustered vcov (and the emmeans
# below) must be paired with THIS model, not the lmer that reuses the name later.
conv_length_lm <- lm(ConvLength ~ BiasedType + PrePerformance + C(NID) +
                          UIdeo + AICorrectness + UStanceLabel,
                        data = single_ai_processed_)

# Calculate clustered variance-covariance matrix by UID
vcov_clustered_conv <- vcovCL(conv_length_lm, cluster = single_ai_processed_$UID)

# Get coefficients with clustered standard errors
clustered_results_conv <- coeftest(conv_length_lm, vcov = vcov_clustered_conv)

cat("\n=== CONVERSATION LENGTH MODEL WITH CLUSTERED SEs ===\n")
print(clustered_results_conv)
summary(conv_length_lm)

# ========================================
# Regression analysis -- mixed effects
# ========================================
conv_length_model <- lmer(ConvLength ~ BiasedType + PrePerformance + C(NID) + (1|UID),
                        data = single_ai_processed_)
summary(conv_length_model)
summary(conv_length_model)$sigma
df.residual(conv_length_model)
performance::r2(conv_length_model)

# ========================================
# Effect size
# ========================================
model_sigma <- sigma(conv_length_model)
model_df <- df.residual(conv_length_model)

# pairs() now returns all pairwise contrasts among the three arms
marginal_means <- emmeans(conv_length_model, ~ BiasedType)
marginal_comparison <- pairs(marginal_means)

marginal_contrast_summary <- summary(marginal_comparison, infer = TRUE)
marginal_estimate <- marginal_contrast_summary$estimate
marginal_se <- marginal_contrast_summary$SE
marginal_df <- marginal_contrast_summary$df

# Calculate Hedges' g manually (one value per contrast)
marginal_cohens_d <- marginal_estimate / model_sigma
marginal_hedges_g <- marginal_cohens_d * J
marginal_hedges_se <- (marginal_se / model_sigma) * J

# Get tidy output for easier interpretation
tidy_conv_results <- broom::tidy(clustered_results_conv, conf.int = TRUE)
tidy_conv_results_fmt <- tidy_conv_results %>%
  mutate(across(where(is.numeric), ~ sprintf("%.3f", .x)))

print(tidy_conv_results_fmt, row.names = FALSE)

# Get model-adjusted means using emmeans (from the OLS model + clustered vcov;
# dimensions match because both come from conv_length_lm)
emm_conv_length <- emmeans(conv_length_lm, "BiasedType", vcov. = vcov_clustered_conv)

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

# Extract each partisan arm's coefficient (vs Default) for summary
conv_treat_terms <- tidy_conv_results[grepl("^BiasedType", tidy_conv_results$term), ]
if(nrow(conv_treat_terms) > 0) {
  cat("\n=== CONVERSATION LENGTH SUMMARY (each arm vs Default) ===\n")
  for(k in 1:nrow(conv_treat_terms)) {
    arm <- sub("BiasedType", "", conv_treat_terms$term[k])
    cat(sprintf("%s - Default: %.3f ± SE %.3f | p = %.4f | 95%% CI [%.3f, %.3f]\n",
                arm,
                conv_treat_terms$estimate[k], conv_treat_terms$std.error[k],
                conv_treat_terms$p.value[k],
                conv_treat_terms$conf.low[k], conv_treat_terms$conf.high[k]))
  }
}

# ========================================
# Engagement analysis
# ========================================
dimensions <- c("EngagementBehavioral", "EngagementCognitive", "EngagementEmotional",
                "EngagementAutonomy", "EngagementSocialPresence")
dimension_names <- c("Behavioral", "Cognitive", "Emotional", "Autonomy", "Social Presence")

groups <- c("Default", "Republican", "Democrat")

# Comprehensive results table (each partisan arm vs Default) and raw group means
engagement_results <- data.frame()
engagement_means   <- data.frame()

for(i in 1:length(dimensions)) {
  cat(sprintf("\n--- %s Engagement ---\n", dimension_names[i]))

  # Mixed effects model with UID random intercept and NID fixed effect.
  # Use lmerTest::lmer explicitly so tidy() carries Satterthwaite df / t / p
  # (plain lme4::lmer omits those columns, regardless of package load order).
  engagement_model <- lmerTest::lmer(
    formula = paste(dimensions[i], "~ BiasedType + as.factor(NID) + PrePerformance + (1|UID) + as.factor(UStanceLabel)"),
    data = single_ai_processed_
  )

  # Extract fixed-effect results
  tidy_results <- tidy(engagement_model, effects = "fixed", conf.int = TRUE)

  # Guard: if df / p.value are still absent (lmerTest unavailable), add as NA so
  # the loop reports estimates/CIs instead of erroring.
  for(col in c("statistic", "df", "p.value", "conf.low", "conf.high")) {
    if(!col %in% names(tidy_results)) tidy_results[[col]] <- NA_real_
  }

  # Raw mean for each group (used by the radar plot)
  for(g in groups) {
    engagement_means <- rbind(engagement_means, data.frame(
      Dimension = dimension_names[i],
      BiasedType = g,
      Mean = mean(single_ai_processed_[[dimensions[i]]][single_ai_processed_$BiasedType == g], na.rm = TRUE)
    ))
  }

  default_mean <- mean(single_ai_processed_[[dimensions[i]]][single_ai_processed_$BiasedType == "Default"], na.rm = TRUE)

  # One row per partisan arm (Republican, Democrat) vs Default
  for(term_name in c("BiasedTypeRepublican", "BiasedTypeDemocrat")) {
    coef_row <- tidy_results[tidy_results$term == term_name, ]
    if(nrow(coef_row) == 0) next
    arm <- sub("BiasedType", "", term_name)
    arm_mean <- mean(single_ai_processed_[[dimensions[i]]][single_ai_processed_$BiasedType == arm], na.rm = TRUE)

    engagement_results <- rbind(engagement_results, data.frame(
      Dimension = dimension_names[i],
      Arm = arm,
      Arm_Mean = round(arm_mean, 3),
      Default_Mean = round(default_mean, 3),
      Difference = round(coef_row$estimate, 3),
      Std_Error = round(coef_row$std.error, 3),
      df = round(coef_row$df, 1),
      t_statistic = round(coef_row$statistic, 3),
      p_value = round(coef_row$p.value, 4),
      CI_lower = round(coef_row$conf.low, 3),
      CI_upper = round(coef_row$conf.high, 3),
      Significance = case_when(
        coef_row$p.value < 0.01 ~ "***",
        coef_row$p.value < 0.05 ~ "**",
        coef_row$p.value < 0.1 ~ "*",
        TRUE ~ ""
      )
    ))

    # Print individual results
    cat(sprintf("  %s AI: M = %.3f | Default AI: M = %.3f\n", arm, arm_mean, default_mean))
    cat(sprintf("  Difference (%s - Default): %.3f ± %.3f\n", arm, coef_row$estimate, coef_row$std.error))
    cat(sprintf("  t(%.1f) = %.3f, p = %.4f %s\n",
                coef_row$df, coef_row$statistic, coef_row$p.value,
                case_when(
                  coef_row$p.value < 0.01 ~ "***",
                  coef_row$p.value < 0.05 ~ "**",
                  coef_row$p.value < 0.1 ~ "*",
                  TRUE ~ ""
                )))
    cat(sprintf("  95%% CI: [%.3f, %.3f]\n", coef_row$conf.low, coef_row$conf.high))
  }

  # Print random effects variance / ICC
  random_effects_var <- as.data.frame(VarCorr(engagement_model))
  uid_var <- random_effects_var[random_effects_var$grp == "UID", "vcov"]
  residual_var <- random_effects_var[random_effects_var$grp == "Residual", "vcov"]
  icc <- uid_var / (uid_var + residual_var)
  cat(sprintf("  ICC (UID): %.3f\n", icc))
}

# Print comprehensive summary table
cat("\n=== ENGAGEMENT RESULTS SUMMARY ===\n")
print(engagement_results, digits = 3)

# Multiple comparison adjustments (across all arm-vs-Default contrasts)
if(nrow(engagement_results) > 0) {
  engagement_results$p_bonferroni <- round(p.adjust(engagement_results$p_value, method = "bonferroni"), 4)
  engagement_results$p_fdr <- round(p.adjust(engagement_results$p_value, method = "fdr"), 4)

  cat("\n=== MULTIPLE COMPARISON ADJUSTMENTS (REFERENCE ONLY) ===\n")
  print(engagement_results[, c("Dimension", "Arm", "p_value", "p_bonferroni", "p_fdr")], digits = 3)

  n_tests <- nrow(engagement_results)
  n_significant_raw <- sum(engagement_results$p_value < 0.1)
  n_significant_bonferroni <- sum(engagement_results$p_bonferroni < 0.1)
  n_significant_fdr <- sum(engagement_results$p_fdr < 0.1)

  cat(sprintf("\nENGAGEMENT ANALYSIS SUMMARY:\n"))
  cat(sprintf("- %d out of %d arm-vs-Default contrasts show significant differences (uncorrected p < 0.1)\n",
              n_significant_raw, n_tests))
  cat(sprintf("- %d out of %d would remain significant with Bonferroni correction\n",
              n_significant_bonferroni, n_tests))
  cat(sprintf("- %d out of %d would remain significant with FDR correction\n",
              n_significant_fdr, n_tests))

  if(n_significant_raw > 0) {
    significant_dims <- with(engagement_results[engagement_results$p_value < 0.1, ],
                             paste(Dimension, Arm))
    cat(sprintf("- Significant (uncorrected): %s\n", paste(significant_dims, collapse = ", ")))
  }
}

# ========================================
# Visualization
# ========================================
# Define colors and theme (Default = gray, Democrat = blue, Republican = red)
bias_colors <- c(
  "Default"    = "#79706E",
  "Democrat"   = "#4E79A7",
  "Republican" = "#E15759"
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

# Create conversation length plot using model-adjusted means (one bar per arm)
conv_length_plot_data <- conv_length_emmeans

conv_length_plot <- ggplot(conv_length_plot_data, aes(x = Mean_ConvLength, y = BiasedType, fill = BiasedType)) +
  geom_col(alpha = 0.8, width = 0.85) +
  geom_errorbar(aes(xmin = CI_95_Lower, xmax = CI_95_Upper),
                width = 0.4, size = 0.5) +
  geom_text(aes(x = CI_95_Upper + max(Mean_ConvLength) * 0.02,
                label = round(Mean_ConvLength, 1)),
            size = 3.3, family = "Avenir", hjust = 0) +
  scale_fill_manual(values = bias_colors) +
  # Bar order top -> bottom: Republican, Default, Democrat
  scale_y_discrete(limits = c("Democrat", "Default", "Republican")) +
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

ggsave("../figures/reply_turns_single.png", conv_length_plot,
       width = 4., height = 1.8, dpi = 500)

  # Create engagement data for radar plot (three groups)
  if(nrow(engagement_means) > 0) {
    engagement_plot_data <- engagement_means %>%
      mutate(
        BiasedType = factor(BiasedType, levels = c("Republican", "Default", "Democrat")),
        Dimension = factor(Dimension, levels = c("Behavioral", "Cognitive", "Emotional",
                                                 "Autonomy", "Social Presence"))
      )

    # Calculate distribution statistics for each dimension (pooling all groups)
    dimension_stats <- single_ai_processed_ %>%
      dplyr::select(BiasedType, EngagementBehavioral, EngagementCognitive, EngagementEmotional,
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
      rename(Score = Mean) %>%
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

    # Define radar colors (three groups)
    radar_colors <- c(
      "Default"    = "#79706E",
      "Democrat"   = "#4E79A7",
      "Republican" = "#E15759"
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
               x = 2 * cos(seq(0, 2*pi, length.out = 100)),
               y = 2 * sin(seq(0, 2*pi, length.out = 100)),
               color = "gray80", size = 0.3) +
      annotate("path",
               x = 4 * cos(seq(0, 2*pi, length.out = 100)),
               y = 4 * sin(seq(0, 2*pi, length.out = 100)),
               color = "gray80", size = 0.3) +
      annotate("path",
               x = 6 * cos(seq(0, 2*pi, length.out = 100)),
               y = 6 * sin(seq(0, 2*pi, length.out = 100)),
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
      # CORRECTED (2026-07-04): with Score_normalized = 4 + z*8, ring radius r sits
      # at z = (r-4)/8. The old rings (1/4/8, labeled -1SD/Mean/+1SD) were actually
      # -0.375/0/+0.5 SD. Rings moved to 2/4/6 = -0.25/0/+0.25 SD, labels to match.
      annotate("text", x = c(-0.3, -0.3, -0.3), y = c(2, 4, 6),
               label = c("-0.25 SD", "Mean", "+0.25 SD"),
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
    
    ggsave("../figures/raddar_single.png", radar_plot,
           width = 3,height = 3, dpi = 500)
  }
