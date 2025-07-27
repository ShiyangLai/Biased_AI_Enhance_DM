# Calculate performance improvement for each observation
single_ai_processed_ <- single_ai_processed_ %>%
  mutate(PerformanceImprovement = PostPerformance - PrePerformance)

# Create BiasedType variable (ensure it exists)
single_ai_processed_ <- single_ai_processed_ %>%
  mutate(BiasedType = ifelse(AIStanceLabel_S %in% c("Democrat", "Republican"), 
                             "Biased", "Non-Biased"))

# =============================================================================
# Mixed effects bias analysis functions
# =============================================================================
# Fit the mixed effects model
model <- lmer(PostPerformance ~ PrePerformance + 
                BiasedType * PoliBias + UStanceLabel + # UIdeo + AICorrectness + 
                as.factor(NID) +
                (1 | UID), 
              data = single_ai_processed_)

# Get model summary and key parameters
model_summary <- tidy(model, effects = "fixed")
model_sigma <- sigma(model)
model_df <- df.residual(model)
summary(model)$sigma 
r2(model)

# Calculate Hedges' g correction factor (J)
J <- 1 - (3 / (4 * model_df - 1))

# Extract interaction terms
interaction_terms <- model_summary %>%
  filter(grepl("BiasedType.*PoliBias", term))

# ANOVA to test interaction significance
interaction_test <- anova(model)

# Get marginal means for BiasedType (overall effect)
marginal_means <- emmeans(model, ~ BiasedType)
marginal_comparison <- pairs(marginal_means)
marginal_contrast_summary <- summary(marginal_comparison, infer = TRUE)

# Extract marginal effect parameters
marginal_estimate <- marginal_contrast_summary$estimate
marginal_se <- marginal_contrast_summary$SE
marginal_df <- marginal_contrast_summary$df

# Calculate effect sizes
marginal_cohens_d <- marginal_estimate / model_sigma
marginal_hedges_g <- marginal_cohens_d * J
marginal_hedges_se <- (marginal_se / model_sigma) * J

# Get emmeans for BiasedType by PoliBias
emm_by_biased_polibias <- emmeans(model, ~ BiasedType | PoliBias)
emm_summary <- as.data.frame(emm_by_biased_polibias)

# Create bias scores
if("BiasedType" %in% names(emm_summary)) {
  print("BiasedType column found")
  
  # Calculate bias scores for each BiasedType
  bias_scores_step1 <- emm_summary %>%
    group_by(BiasedType) %>%
    summarise(
      means = list(emmean),
      ses = list(SE),
      n_groups = n(),
      .groups = 'drop'
    )
  
  # Add pairwise differences
  bias_scores_step2 <- bias_scores_step1 %>%
    rowwise() %>%
    mutate(
      pairwise_diffs = list({
        means_vec <- unlist(means)
        if(length(means_vec) < 2) {
          NA
        } else {
          diffs <- c()
          for(i in 1:(length(means_vec)-1)) {
            for(j in (i+1):length(means_vec)) {
              diffs <- c(diffs, abs(means_vec[i] - means_vec[j]))
            }
          }
          diffs
        }
      }),
      BiasScore = ifelse(n_groups < 2, NA, mean(unlist(pairwise_diffs), na.rm = TRUE)),
      BiasScore_SE = ifelse(n_groups < 2, NA, sqrt(sum(unlist(ses)^2)) / n_groups)
    )
  
  # Clean up bias scores
  bias_scores <- bias_scores_step2 %>%
    dplyr::select(BiasedType, BiasScore, BiasScore_SE, n_groups) %>%
    mutate(
      Lower_CI = BiasScore - 1.96 * BiasScore_SE,
      Upper_CI = BiasScore + 1.96 * BiasScore_SE,
      Lower_CI = pmax(Lower_CI, 0, na.rm = TRUE)
    )
  
} else {
  print("BiasedType column NOT found in emm_summary")
  print("Available columns:")
  print(names(emm_summary))
}

# Create effect sizes summary
effect_sizes_summary <- data.frame(
  Comparison = "Biased_vs_NonBiased",
  Estimate = marginal_estimate,
  SE = marginal_se,
  df = marginal_df,
  Cohens_d = marginal_cohens_d,
  Hedges_g = marginal_hedges_g,
  Hedges_g_SE = marginal_hedges_se,
  Hedges_g_Lower_CI = marginal_hedges_g - 1.96 * marginal_hedges_se,
  Hedges_g_Upper_CI = marginal_hedges_g + 1.96 * marginal_hedges_se
)

# Create performance_by_group for visualization
performance_by_group <- emm_summary %>%
  rename(MeanImprovement = emmean) %>%
  dplyr::select(all_of(c(names(emm_summary)[1], "PoliBias", "MeanImprovement", "SE")))  # Use first column as BiasedType

# Rename the first column to BiasedType for easier joining
names(performance_by_group)[1] <- "BiasedType"

performance_by_group <- performance_by_group %>%
  left_join(counts, by = c("BiasedType", "PoliBias"))

# =============================================================================
# Compare Biased vs Non-Biased AI for each political condition PAIR gap
# =============================================================================
calculate_condition_pair_comparisons_debug <- function(emm_data, model_df = NULL) {
  cat("\n=== DEBUGGING: Function inputs ===\n")
  print("Input data:")
  print(emm_data)
  
  # Get degrees of freedom from model if not provided
  if(is.null(model_df)) {
    model_df <- df.residual(model)
  }
  
  # Check what columns we actually have
  cat("\nAvailable columns:", paste(names(emm_data), collapse = ", "), "\n")
  
  # Try to identify the correct column names
  biased_col <- NULL
  polibias_col <- NULL
  
  # Look for BiasedType column (might have different name)
  possible_biased_cols <- c("BiasedType", names(emm_data)[1])
  for(col in possible_biased_cols) {
    if(col %in% names(emm_data)) {
      unique_vals <- unique(emm_data[[col]])
      if(any(c("Biased", "Non-Biased") %in% unique_vals)) {
        biased_col <- col
        break
      }
    }
  }
  
  # Look for PoliBias column
  possible_polibias_cols <- c("PoliBias", "Political", "Condition")
  for(col in possible_polibias_cols) {
    if(col %in% names(emm_data)) {
      unique_vals <- unique(emm_data[[col]])
      if(any(c("Republican", "Neutral", "Democrat") %in% unique_vals)) {
        polibias_col <- col
        break
      }
    }
  }
  
  cat(sprintf("Identified BiasedType column: %s\n", ifelse(is.null(biased_col), "NONE", biased_col)))
  cat(sprintf("Identified PoliBias column: %s\n", ifelse(is.null(polibias_col), "NONE", polibias_col)))
  
  if(is.null(biased_col) || is.null(polibias_col)) {
    cat("ERROR: Cannot find required columns\n")
    return(data.frame())
  }
  
  # Print unique values in identified columns
  cat(sprintf("Unique values in %s: %s\n", biased_col, paste(unique(emm_data[[biased_col]]), collapse = ", ")))
  cat(sprintf("Unique values in %s: %s\n", polibias_col, paste(unique(emm_data[[polibias_col]]), collapse = ", ")))
  
  # Define condition pairs
  condition_pairs <- list(
    c("Republican", "Neutral"),
    c("Republican", "Democrat"), 
    c("Democrat", "Neutral")
  )
  
  results_df <- data.frame()
  
  for(pair in condition_pairs) {
    cond1 <- pair[1]
    cond2 <- pair[2]
    pair_name <- paste(cond1, "vs", cond2)
    
    cat(sprintf("\n--- Processing pair: %s ---\n", pair_name))
    
    # Calculate gap for Biased AI
    biased_data <- emm_data[emm_data[[biased_col]] == "Biased", ]
    cat(sprintf("Biased data rows: %d\n", nrow(biased_data)))
    
    if(nrow(biased_data) > 0) {
      cat("Biased data PoliBias values:", paste(biased_data[[polibias_col]], collapse = ", "), "\n")
    }
    
    if(cond1 %in% biased_data[[polibias_col]] && cond2 %in% biased_data[[polibias_col]]) {
      biased_cond1 <- biased_data$emmean[biased_data[[polibias_col]] == cond1]
      biased_cond2 <- biased_data$emmean[biased_data[[polibias_col]] == cond2]
      biased_se1 <- biased_data$SE[biased_data[[polibias_col]] == cond1]
      biased_se2 <- biased_data$SE[biased_data[[polibias_col]] == cond2]
      
      biased_gap <- abs(biased_cond1 - biased_cond2)
      biased_gap_se <- sqrt(biased_se1^2 + biased_se2^2)
      
      cat(sprintf("Biased %s: %.3f, %s: %.3f, Gap: %.3f\n", cond1, biased_cond1, cond2, biased_cond2, biased_gap))
    } else {
      cat(sprintf("Missing conditions for Biased AI: %s or %s\n", cond1, cond2))
      next
    }
    
    # Calculate gap for Non-Biased AI
    nonbiased_data <- emm_data[emm_data[[biased_col]] == "Non-Biased", ]
    cat(sprintf("Non-Biased data rows: %d\n", nrow(nonbiased_data)))
    
    if(nrow(nonbiased_data) > 0) {
      cat("Non-Biased data PoliBias values:", paste(nonbiased_data[[polibias_col]], collapse = ", "), "\n")
    }
    
    if(cond1 %in% nonbiased_data[[polibias_col]] && cond2 %in% nonbiased_data[[polibias_col]]) {
      nonbiased_cond1 <- nonbiased_data$emmean[nonbiased_data[[polibias_col]] == cond1]
      nonbiased_cond2 <- nonbiased_data$emmean[nonbiased_data[[polibias_col]] == cond2]
      nonbiased_se1 <- nonbiased_data$SE[nonbiased_data[[polibias_col]] == cond1]
      nonbiased_se2 <- nonbiased_data$SE[nonbiased_data[[polibias_col]] == cond2]
      
      nonbiased_gap <- abs(nonbiased_cond1 - nonbiased_cond2)
      nonbiased_gap_se <- sqrt(nonbiased_se1^2 + nonbiased_se2^2)
      
      cat(sprintf("Non-Biased %s: %.3f, %s: %.3f, Gap: %.3f\n", cond1, nonbiased_cond1, cond2, nonbiased_cond2, nonbiased_gap))
    } else {
      cat(sprintf("Missing conditions for Non-Biased AI: %s or %s\n", cond1, cond2))
      next
    }
    
    # Compare the gaps: Biased gap - Non-Biased gap
    gap_difference <- biased_gap - nonbiased_gap
    gap_diff_se <- sqrt(biased_gap_se^2 + nonbiased_gap_se^2)
    
    # Calculate t-statistic and p-value
    t_stat <- gap_difference / gap_diff_se
    p_value <- 2 * (1 - pt(abs(t_stat), df = model_df))
    
    # Calculate confidence intervals
    ci_lower <- gap_difference - 1.96 * gap_diff_se
    ci_upper <- gap_difference + 1.96 * gap_diff_se
    
    # Add to results
    results_df <- rbind(results_df, data.frame(
      Condition_Pair = pair_name,
      Biased_Gap = biased_gap,
      NonBiased_Gap = nonbiased_gap,
      Gap_Difference = gap_difference,
      SE = gap_diff_se,
      t_stat = t_stat,
      p_value = p_value,
      CI_Lower = ci_lower,
      CI_Upper = ci_upper,
      Significant = ifelse(p_value < 0.05, "***", 
                           ifelse(p_value < 0.1, "*", ""))
    ))
    
    cat(sprintf("Added result: Gap difference = %.3f, p = %.3f\n", gap_difference, p_value))
  }
  
  return(results_df)
}

# Run the debugging version
cat("\n=== RUNNING DEBUG VERSION ===\n")
condition_pair_results_debug <- calculate_condition_pair_comparisons_debug(emm_summary)

cat("\n=== FINAL RESULTS ===\n")
print(condition_pair_results_debug)


# =============================================================================
# Visualization
# =============================================================================
# Check if bias_scores exists and has data
if(exists("bias_scores") && nrow(bias_scores) > 0 && any(!is.na(bias_scores$BiasScore))) {
  
  # Define colors
  bias_colors <- c(
    "Biased" = "#E15759",      # Red for biased AI
    "Non-Biased" = "#4E79A7"   # Blue for non-biased AI
  )
  
  # Stylize theme
  nature_theme <- theme_classic() +
    theme(
      text = element_text(family = "Avenir", size = 8),
      plot.title = element_text(family = "Avenir", size = 10, face = "bold", hjust = 0),
      axis.title = element_text(family = "Avenir", size = 9, face = "plain"),
      axis.text = element_text(family = "Avenir", size = 8, color = "black"),
      axis.text.y = element_text(family = "Avenir", size = 9, color = "black", face = "plain"),
      axis.line = element_line(color = "black", linewidth = 0.5),
      axis.ticks = element_line(color = "black", linewidth = 0.5),
      axis.ticks.length.x = unit(0.15, "cm"),
      axis.ticks.length = unit(0.15, "cm"),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 10)
    )
  
  # Create comprehensive bias mitigation plot
  comprehensive_plot <- ggplot(bias_scores, aes(x = BiasedType, y = BiasScore, fill = BiasedType)) +
    # Clean bars
    geom_col(alpha = 0.8, width = 0.6) +
    # Error bars
    geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), 
                  width = 0.15, size = 0.7, alpha = 0.9) +
    # Value labels positioned above error bars
    geom_text(aes(y = Upper_CI + max(Upper_CI, na.rm = TRUE) * 0.07, 
                  label = round(BiasScore, 3)), 
              size = 4, family = "Avenir") +
    scale_fill_manual(values = bias_colors) +
    scale_y_continuous(labels = label_number(accuracy = 0.01), 
                       expand = expansion(mult = c(0, 0.15)),
                       limits = c(0, max(bias_scores$Upper_CI, na.rm = TRUE) * 1.2)) +
    labs(
      x = NULL,
      y = "Post-Interaction Performance Gap\nAcross Rep/Neu/Dem News"
    ) +
    nature_theme + 
    theme(
      legend.position = "none",
      text = element_text(family = "Avenir", size = 12),
      axis.text = element_text(family = "Avenir", size = 12, color = "black"),
      axis.text.y = element_text(family = "Avenir", size = 10, color = "black", face = "plain"),
      axis.title.y = element_text(family = "Avenir", size = 12, color = "black", margin = margin(r=6))
    )
  
  print(comprehensive_plot)
  
  # Print comprehensive interpretation
  cat("\n=== COMPREHENSIVE BIAS MITIGATION ANALYSIS (Mixed Effects) ===\n")
  cat("Political Bias Scores (Average Absolute Pairwise Differences):\n")
  for(i in 1:nrow(bias_scores)) {
    if(!is.na(bias_scores$BiasScore[i])) {
      cat(sprintf("%s AI: %.3f (95%% CI: %.3f to %.3f) [%d political groups]\n", 
                  bias_scores$BiasedType[i], 
                  bias_scores$BiasScore[i],
                  bias_scores$Lower_CI[i], 
                  bias_scores$Upper_CI[i],
                  bias_scores$n_groups[i]))
    }
  }
  
  # Calculate bias mitigation effect
  if(nrow(bias_scores) == 2 && all(!is.na(bias_scores$BiasScore))) {
    biased_idx <- which(bias_scores$BiasedType == "Biased")
    nonbiased_idx <- which(bias_scores$BiasedType == "Non-Biased")
    
    if(length(biased_idx) == 1 && length(nonbiased_idx) == 1) {
      bias_reduction <- bias_scores$BiasScore[biased_idx] - bias_scores$BiasScore[nonbiased_idx]
      
      # Calculate SE for the difference
      se_biased <- bias_scores$BiasScore_SE[biased_idx]
      se_nonbiased <- bias_scores$BiasScore_SE[nonbiased_idx]
      se_difference <- sqrt(se_biased^2 + se_nonbiased^2)
      
      # Calculate confidence interval for the difference
      ci_lower <- bias_reduction - 1.96 * se_difference
      ci_upper <- bias_reduction + 1.96 * se_difference
      
      # Calculate p-value for the difference
      t_stat <- bias_reduction / se_difference
      p_value <- 2 * (1 - pnorm(abs(t_stat)))
      
      cat(sprintf("\nBias Mitigation Effect: %.3f (95%% CI: %.3f to %.3f, p = %.3f)\n", 
                  bias_reduction, ci_lower, ci_upper, p_value))
      
      if(bias_reduction > 0 && p_value < 0.05) {
        cat("Interpretation: Biased AI shows significantly higher political bias than non-biased AI.\n")
        cat("*** BIAS MITIGATION SUCCESSFUL ***\n")
      } else if(bias_reduction > 0 && p_value >= 0.05) {
        cat("Interpretation: Biased AI shows higher political bias, but difference is not statistically significant.\n")
      } else if(bias_reduction < 0 && p_value < 0.05) {
        cat("Interpretation: Non-biased AI unexpectedly shows higher political bias.\n")
      } else {
        cat("Interpretation: No significant difference in political bias between AI types.\n")
      }
    }
  }
  
  # Also show the overall effect size from the marginal comparison (use individual variables)
  if(exists("effect_sizes_summary")) {
    cat(sprintf("\nOverall Effect Size (Hedges' g): %.3f (95%% CI: %.3f to %.3f)\n",
                effect_sizes_summary$Hedges_g,
                effect_sizes_summary$Hedges_g_Lower_CI,
                effect_sizes_summary$Hedges_g_Upper_CI))
  }
  
} else {
  cat("No bias scores data available for visualization.\n")
  if(exists("bias_scores")) {
    cat("bias_scores exists but:")
    cat("  - Number of rows:", nrow(bias_scores), "\n")
    cat("  - BiasScore values:", bias_scores$BiasScore, "\n")
  } else {
    cat("bias_scores variable does not exist\n")
  }
}

# =============================================================================
# Effect size analysis
# =============================================================================
model <- lmer(PostPerformance ~ PrePerformance + 
                BiasedType * PoliBias + 
                as.factor(NID) + (1 | UID), data = single_ai_processed_)

model_summary <- tidy(model, effects = "fixed")
interaction_terms <- model_summary %>%
  filter(grepl("BiasedType.*PoliBias", term))

interaction_test <- anova(model)

model_sigma <- sigma(model)
model_df <- df.residual(model)

marginal_means <- emmeans(model, ~ BiasedType)
marginal_comparison <- pairs(marginal_means)

marginal_contrast_summary <- summary(marginal_comparison, infer = TRUE)
marginal_estimate <- marginal_contrast_summary$estimate
marginal_se <- marginal_contrast_summary$SE
marginal_df <- marginal_contrast_summary$df

# Calculate Hedges' g manually
marginal_cohens_d <- marginal_estimate / model_sigma
marginal_hedges_g <- marginal_cohens_d * J
marginal_hedges_se <- (marginal_se / model_sigma) * J

# =============================================================================
# Print key results
# =============================================================================
# Overall ANOVA (shows if interaction is significant)
cat("1. OVERALL ANOVA (Is there a BiasedType × PoliBias interaction?)\n")
print(interaction_results$anova_results)

# Main comparison: Biased vs Non-Biased overall
cat("2. MARGINAL COMPARISON: Biased vs Non-Biased AI (overall effect)\n")
cat("Marginal Means:\n")
print(interaction_results$marginal_means)
cat("\nComparison:\n")
print(interaction_results$marginal_comparison)
cat("\n")

# Conditional comparisons within each political group
cat("3. CONDITIONAL COMPARISONS: Biased vs Non-Biased within each PoliBias group\n")
cat("Conditional Means:\n")
print(interaction_results$conditional_means)
cat("\nComparisons within each PoliBias:\n")
print(interaction_results$conditional_comparisons)
cat("\n")

# Interaction contrasts (tests if the Biased vs Non-Biased effect differs by PoliBias)
cat("4. INTERACTION CONTRASTS: Does Biased vs Non-Biased effect vary by PoliBias?\n")
print(interaction_results$interaction_contrasts)
cat("\n")

# PoliBias effects within each AI type
cat("5. POLIBIAS EFFECTS: Political group differences within each AI type\n")
print(interaction_results$polibias_comparisons)

# =============================================================================
# Eextract coefficients and p
# =============================================================================
cat("\n=== SUMMARY OF KEY TESTS ===\n")
# Overall interaction p-value
interaction_p <- interaction_results$anova_results["BiasedType:PoliBias", "Pr(>F)"]
cat(sprintf("1. BiasedType × PoliBias Interaction: p = %.3f\n", interaction_p))

# Marginal Biased vs Non-Biased effect
marginal_test <- as.data.frame(interaction_results$marginal_comparison)
if(nrow(marginal_test) > 0) {
  cat(sprintf("2. Overall Biased vs Non-Biased Effect: %.3f (SE = %.3f, p = %.3f)\n", 
              marginal_test$estimate[1], marginal_test$SE[1], marginal_test$p.value[1]))
}

# Conditional effects
conditional_test <- as.data.frame(interaction_results$conditional_comparisons)
if(nrow(conditional_test) > 0) {
  cat("3. Biased vs Non-Biased within each PoliBias group:\n")
  for(i in 1:nrow(conditional_test)) {
    cat(sprintf("   %s: %.3f (SE = %.3f, p = %.3f)\n", 
                conditional_test$PoliBias[i], 
                conditional_test$estimate[i], 
                conditional_test$SE[i], 
                conditional_test$p.value[i]))
  }
}

# =============================================================================
# OLS with robust standard errors bias analysis
# =============================================================================
# Fit OLS model
model <- lm(PostPerformance ~ PrePerformance + 
              BiasedType * PoliBias +
              as.factor(NID), 
            data = single_ai_processed_)
summary(model)

# Calculate cluster-robust standard errors
robust_vcov <- vcovCL(model, cluster = single_ai_processed_$UID)

# Get emmeans with robust SEs
emm_by_biased_polibias <- emmeans(model, ~ BiasedType | PoliBias, vcov = robust_vcov)
emm_summary <- as.data.frame(emm_by_biased_polibias)

print("=== EMMEANS BY GROUP ===")
print(emm_summary)

# Calculate bias scores for each BiasedType
calculate_bias_scores <- function(emm_data) {
  bias_scores <- emm_data %>%
    group_by(BiasedType) %>%
    summarise(
      means = list(emmean),
      ses = list(SE),
      n_groups = n(),
      .groups = 'drop'
    ) %>%
    rowwise() %>%
    mutate(
      # Calculate all pairwise absolute differences
      pairwise_diffs = list({
        means_vec <- unlist(means)
        if(length(means_vec) < 2) {
          NA
        } else {
          diffs <- c()
          for(i in 1:(length(means_vec)-1)) {
            for(j in (i+1):length(means_vec)) {
              diffs <- c(diffs, abs(means_vec[i] - means_vec[j]))
            }
          }
          diffs
        }
      }),
      # Bias score = average absolute pairwise difference
      BiasScore = ifelse(n_groups < 2, NA, mean(unlist(pairwise_diffs), na.rm = TRUE)),
      # Standard error (less conservative)
      BiasScore_SE = ifelse(n_groups < 2, NA, sqrt(sum(unlist(ses)^2)) / n_groups)
    ) %>%
    dplyr::select(BiasedType, BiasScore, BiasScore_SE, n_groups) %>%
    mutate(
      Lower_CI = BiasScore - 1.96 * BiasScore_SE,
      Upper_CI = BiasScore + 1.96 * BiasScore_SE,
      Lower_CI = pmax(Lower_CI, 0, na.rm = TRUE)
    )
  
  return(bias_scores)
}

# Calculate bias scores
bias_scores <- calculate_bias_scores(emm_summary)

print("=== BIAS SCORES ===")
print(bias_scores)

# T-test comparing bias scores between groups
if(nrow(bias_scores) == 2 && all(!is.na(bias_scores$BiasScore))) {
  
  # Get values
  group1_idx <- which(bias_scores$BiasedType == "Biased")
  group2_idx <- which(bias_scores$BiasedType == "Non-Biased")
  
  if(length(group1_idx) == 1 && length(group2_idx) == 1) {
    
    group1_bias <- bias_scores$BiasScore[group1_idx]
    group2_bias <- bias_scores$BiasScore[group2_idx]
    group1_se <- bias_scores$BiasScore_SE[group1_idx]
    group2_se <- bias_scores$BiasScore_SE[group2_idx]
    
    # Calculate difference
    bias_difference <- group1_bias - group2_bias
    
    # Calculate SE for the difference
    diff_se <- sqrt(group1_se^2 + group2_se^2)
    
    # Calculate t-statistic and p-value
    t_stat <- bias_difference / diff_se
    
    # Use conservative df (smaller of the two groups, approximated)
    df <- df.residual(model)  # Use model df as approximation
    p_value <- 2 * (1 - pt(abs(t_stat), df = df))
    
    # Calculate confidence intervals
    ci_lower <- bias_difference - qt(0.975, df) * diff_se
    ci_upper <- bias_difference + qt(0.975, df) * diff_se
    
    # Print results
    cat("\n=== T-TEST RESULTS: BIAS SCORE COMPARISON ===\n")
    cat(sprintf("Group 1 (Biased): %.4f (SE = %.4f)\n", group1_bias, group1_se))
    cat(sprintf("Group 2 (Non-Biased): %.4f (SE = %.4f)\n", group2_bias, group2_se))
    cat(sprintf("Difference: %.4f (Biased - Non-Biased)\n", bias_difference))
    cat(sprintf("SE of difference: %.4f\n", diff_se))
    cat(sprintf("t-statistic: %.4f\n", t_stat))
    cat(sprintf("df: %d\n", df))
    cat(sprintf("p-value: %.4f\n", p_value))
    cat(sprintf("95%% CI: [%.4f, %.4f]\n", ci_lower, ci_upper))
    
    # Interpretation
    cat("\n=== INTERPRETATION ===\n")
    if(p_value < 0.05) {
      if(bias_difference > 0) {
        cat("*** SIGNIFICANT: Biased AI shows significantly MORE political bias than Non-Biased AI ***\n")
      } else {
        cat("*** SIGNIFICANT: Non-Biased AI shows significantly MORE political bias than Biased AI ***\n")
      }
    } else {
      cat("NOT SIGNIFICANT: No significant difference in political bias between AI types\n")
    }
    
    # Effect size interpretation
    effect_size <- abs(bias_difference) / sqrt((group1_se^2 + group2_se^2)/2)
    cat(sprintf("Effect size (approximate): %.3f\n", effect_size))
    
    if(effect_size < 0.2) {
      cat("Effect size: Small\n")
    } else if(effect_size < 0.5) {
      cat("Effect size: Small to Medium\n") 
    } else if(effect_size < 0.8) {
      cat("Effect size: Medium to Large\n")
    } else {
      cat("Effect size: Large\n")
    }
    
  } else {
    cat("ERROR: Could not find both Biased and Non-Biased groups\n")
  }
  
} else {
  cat("ERROR: Need exactly 2 groups with valid bias scores for t-test\n")
  if(exists("bias_scores")) {
    cat("Current bias_scores:\n")
    print(bias_scores)
  }
}

