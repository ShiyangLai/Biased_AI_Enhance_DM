# Reshape df2 from wide to long format and create BiasedCat from StanceCode
df2_step1 <- df2[df2$AI1StanceLabel_S != "Neutral" & df2$AI2StanceLabel_S != "Neutral", ] %>%
  pivot_longer(
    cols = c(AI1StanceCode, AI2StanceCode),
    names_to = "AI_Type",
    values_to = "AI_StanceCode",
    names_pattern = "(AI[12])StanceCode"
  )

# Check what columns we have after pivot_longer
cat("Columns after pivot_longer:", paste(names(df2_step1), collapse = ", "), "\n")

df2_long <- df2_step1 %>%
  mutate(
    AI_Correctness_Score = case_when(
      AI_Type == "AI1" ~ AI1Correctness,
      AI_Type == "AI2" ~ AI2Correctness,
      TRUE ~ NA_real_
    ),
    # Create BiasedCat from StanceCode
    BiasedCat = case_when(
      AI_StanceCode %in% c(-2, 2) ~ "Strong Bias",
      AI_StanceCode %in% c(-1, 1) ~ "Moderate Bias", 
      AI_StanceCode == 0 ~ "No Bias",
      TRUE ~ NA_character_
    )
  ) %>%
  # Use dplyr::select explicitly to avoid conflicts
  dplyr::select(BiasedCat, AI_Correctness_Score)

# Check what we ended up with
cat("Final columns in df2_long:", paste(names(df2_long), collapse = ", "), "\n")

# Prepare single_ai_processed_ data - use existing BiasedCat column
single_ai_renamed <- single_ai_processed_ %>%
  dplyr::select(BiasedCat, AICorrectness) %>%
  rename(
    AI_Correctness_Score = AICorrectness
  ) %>%
  # Ensure BiasedCat is character type to match df2_long
  mutate(BiasedCat = as.character(BiasedCat))

# Combine both datasets
combined_data <- bind_rows(
  single_ai_renamed,
  df2_long
)

# Aggregate AI_Correctness_Score by bias magnitude
bias_summary <- single_ai_renamed %>%
  group_by(BiasedCat) %>%
  summarise(
    mean_correctness = mean(AI_Correctness_Score, na.rm = TRUE),
    median_correctness = median(AI_Correctness_Score, na.rm = TRUE),
    sd_correctness = sd(AI_Correctness_Score, na.rm = TRUE),
    n_observations = n(),
    .groups = 'drop'
  )

# Display results
print("Summary statistics by AI bias magnitude:")
print(bias_summary)

# ANOVA comparing three bias magnitude categories (No Bias, Moderate Bias, Strong Bias)
# First, check if we have all three groups
available_bias_cats <- unique(single_ai_renamed$BiasedCat)
cat("Available bias categories:", paste(available_bias_cats, collapse = ", "), "\n")

# Filter to only include the three bias categories of interest and remove missing values
anova_data <- single_ai_renamed %>%
  filter(BiasedCat %in% c("No Bias", "Moderate Bias", "Strong Bias")) %>%
  filter(!is.na(AI_Correctness_Score) & !is.na(BiasedCat))

# Check sample sizes
sample_sizes <- anova_data %>%
  group_by(BiasedCat) %>%
  summarise(n = n(), .groups = 'drop')

cat("\nSample sizes by bias category:\n")
print(sample_sizes)

if(nrow(anova_data) > 0 && length(unique(anova_data$BiasedCat)) >= 2) {
  
  # Perform ANOVA
  anova_model <- aov(AI_Correctness_Score ~ BiasedCat, data = anova_data)
  anova_summary <- summary(anova_model)
  
  cat("\n=== ANOVA RESULTS ===\n")
  cat("Testing differences in AI Correctness across No Bias, Moderate Bias, and Strong Bias categories\n\n")
  print(anova_summary)
  
  # Extract F-statistic and p-value
  f_stat <- anova_summary[[1]][["F value"]][1]
  p_value <- anova_summary[[1]][["Pr(>F)"]][1]
  
  cat("\nANOVA Summary:\n")
  cat("F-statistic:", round(f_stat, 4), "\n")
  cat("p-value:", round(p_value, 6), "\n")
  cat("Significant overall difference (p < 0.05):", ifelse(p_value < 0.05, "YES", "NO"), "\n")
  
  # Calculate confidence intervals for group means
  group_stats <- anova_data %>%
    group_by(BiasedCat) %>%
    summarise(
      n = n(),
      mean_score = mean(AI_Correctness_Score, na.rm = TRUE),
      sd_score = sd(AI_Correctness_Score, na.rm = TRUE),
      se_score = sd_score / sqrt(n),
      .groups = 'drop'
    ) %>%
    mutate(
      # 95% confidence intervals for means
      ci_lower = mean_score - qt(0.975, n-1) * se_score,
      ci_upper = mean_score + qt(0.975, n-1) * se_score
    )
  
  cat("\n=== GROUP MEANS WITH 95% CONFIDENCE INTERVALS ===\n")
  print(group_stats %>% 
          dplyr::select(BiasedCat, n, mean_score, sd_score, ci_lower, ci_upper) %>%
          mutate(across(where(is.numeric), ~round(.x, 4))))
  
  # Post-hoc pairwise comparisons with FDR correction
  if(p_value < 0.05 && length(unique(anova_data$BiasedCat)) >= 2) {
    cat("\n=== POST-HOC PAIRWISE COMPARISONS (FDR Corrected) ===\n")
    
    pairwise_results <- pairwise.t.test(anova_data$AI_Correctness_Score, 
                                        anova_data$BiasedCat, 
                                        p.adjust.method = "fdr")
    
    print(pairwise_results)
    
    # Calculate estimated marginal means and pairwise differences with confidence intervals
    cat("\n=== ESTIMATED MARGINAL MEANS ===\n")
    library(emmeans)
    
    # Calculate estimated marginal means
    emm_results <- emmeans(anova_model, ~ BiasedCat)
    print(emm_results)
    
    # Calculate pairwise differences with confidence intervals
    cat("\n=== PAIRWISE DIFFERENCES OF MARGINAL ESTIMATED MEANS ===\n")
    pairwise_diffs <- pairs(emm_results, adjust = "fdr")  # No adjustment since we're using FDR above
    print(pairwise_diffs)
    
    # Extract and format the results for cleaner display
    pairwise_summary <- summary(pairwise_diffs, infer = TRUE)
    
    cat("\nFormatted Pairwise Differences Results:\n")
    formatted_results <- data.frame(
      comparison = pairwise_summary$contrast,
      difference_delta = round(pairwise_summary$estimate, 4),
      se = round(pairwise_summary$SE, 4),
      ci_lower = round(pairwise_summary$lower.CL, 4),
      ci_upper = round(pairwise_summary$upper.CL, 4),
      t_ratio = round(pairwise_summary$t.ratio, 4),
      p_value = round(pairwise_summary$p.value, 6)
    )
    
    print(formatted_results)
    
    # Also show the marginal means in a clean format
    cat("\nEstimated Marginal Means by Bias Category:\n")
    emm_summary <- summary(emm_results, infer = TRUE)
    formatted_means <- data.frame(
      bias_category = emm_summary$BiasedCat,
      estimated_mean = round(emm_summary$emmean, 4),
      se = round(emm_summary$SE, 4),
      ci_lower = round(emm_summary$lower.CL, 4),
      ci_upper = round(emm_summary$upper.CL, 4)
    )
    
    print(formatted_means)
    
  } else if(p_value >= 0.05) {
    cat("\nNo significant overall difference found, skipping post-hoc tests.\n")
  }
  
} else {
  cat("Insufficient data for ANOVA analysis.\n")
  cat("Need at least 2 groups with data.\n")
}
