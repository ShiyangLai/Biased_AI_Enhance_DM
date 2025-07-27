# =====================================
# Comprehensive bias analysis: ExperimentType × PoliBias
# =====================================
# Verify model and data
cat("\n=== CHECKING MODEL AND DATA ===\n")
if(exists("bias_model")) {
  cat("Model formula:\n")
  print(formula(bias_model))
} else {
  stop("bias_model not found!")
}

if(exists("combined_data")) {
  cat("ExperimentType levels:\n")
  if(is.factor(combined_data$ExperimentType)) {
    print(levels(combined_data$ExperimentType))
    experiment_types <- levels(combined_data$ExperimentType)
  } else {
    print(unique(combined_data$ExperimentType))
    experiment_types <- unique(combined_data$ExperimentType)
  }
  
  cat("PoliBias levels:\n")
  if(is.factor(combined_data$PoliBias)) {
    print(levels(combined_data$PoliBias))
    polibias_levels <- levels(combined_data$PoliBias)
  } else {
    print(unique(combined_data$PoliBias))
    polibias_levels <- unique(combined_data$PoliBias)
  }
} else {
  stop("combined_data not found!")
}

# ==================================================
# 1. Pairwsie comparison between experimental types
# ==================================================
cat("\n=== PAIRWISE COMPARISONS BETWEEN EXPERIMENT TYPES ===\n")

# Get emmeans for the full interaction
emm_full_interaction <- emmeans(bias_model, ~ ExperimentType | PoliBias)

cat("Full interaction emmeans:\n")
print(summary(emm_full_interaction))

# Perform pairwise comparisons within each PoliBias level using the 'by' argument
pairwise_by_polibias <- pairs(emm_full_interaction, by = "PoliBias", adjust = "none")

cat("\nPairwise comparisons within each PoliBias level:\n")
print(summary(pairwise_by_polibias))

# Convert to dataframe for further processing
pairwise_results_df <- as.data.frame(pairwise_by_polibias)

if(nrow(pairwise_results_df) > 0) {
  # Add effect sizes and corrections
  pooled_sd <- sigma(bias_model)
  
  combined_pairwise_results <- pairwise_results_df %>%
    mutate(
      cohens_d = estimate / pooled_sd,
      hedges_g = cohens_d * (1 - (3 / (4 * df - 1))),
      significant_raw = p.value < 0.05,
      effect_size_category = case_when(
        abs(hedges_g) < 0.2 ~ "Negligible",
        abs(hedges_g) < 0.5 ~ "Small",
        abs(hedges_g) < 0.8 ~ "Medium",
        TRUE ~ "Large"
      )
    ) %>%
    group_by(PoliBias) %>%
    mutate(
      # Within-group FDR correction
      p_fdr_within = p.adjust(p.value, method = "fdr"),
      significant_fdr_within = p_fdr_within < 0.05
    ) %>%
    ungroup() %>%
    mutate(
      # Global FDR correction
      p_fdr_global = p.adjust(p.value, method = "fdr"),
      significant_fdr_global = p_fdr_global < 0.05
    ) %>%
    mutate(
      estimate = round(estimate, 4),
      SE = round(SE, 4),
      t.ratio = round(t.ratio, 3),
      p.value = round(p.value, 4),
      p_fdr_within = round(p_fdr_within, 4),
      p_fdr_global = round(p_fdr_global, 4),
      hedges_g = round(hedges_g, 4)
    ) %>%
    dplyr::select(PoliBias, contrast, estimate, SE, t.ratio, df, p.value, 
                  p_fdr_within, p_fdr_global, hedges_g, effect_size_category, 
                  significant_raw, significant_fdr_within, significant_fdr_global)
  
  cat(sprintf("\nTotal pairwise comparisons: %d\n", nrow(combined_pairwise_results)))
} else {
  combined_pairwise_results <- NULL
  cat("No pairwise results generated\n")
}

# ========================================================
# 2. Political gaps within each pairwise experimental type
# ========================================================

cat("\n=== POLITICAL BIAS GAPS WITHIN EACH EXPERIMENT TYPE ===\n")

# Get emmeans for the full interaction (ExperimentType by PoliBias)
emm_interaction <- emmeans(bias_model, ~ ExperimentType * PoliBias)
emm_summary <- as.data.frame(emm_interaction)

cat("Emmeans for ExperimentType × PoliBias:\n")
print(emm_summary)

# Calculate bias gaps for each ExperimentType (following your bias score approach)
bias_gaps_results <- emm_summary %>%
  group_by(ExperimentType) %>%
  summarise(
    means = list(emmean),
    ses = list(SE),
    polibias_groups = list(PoliBias),
    n_groups = n(),
    .groups = 'drop'
  ) %>%
  rowwise() %>%
  mutate(
    # Calculate all pairwise absolute differences (your bias score approach)
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
    # Calculate bias gap (average absolute pairwise difference)
    BiasGap = ifelse(n_groups < 2, NA, mean(unlist(pairwise_diffs), na.rm = TRUE)),
    # Conservative SE estimate
    BiasGap_SE = ifelse(n_groups < 2, NA, sqrt(sum(unlist(ses)^2)) / n_groups),
    # Also calculate specific Rep-Dem gap if available
    RepDem_Gap = ifelse(n_groups >= 2 && 
                          all(c("Republican", "Democrat") %in% unlist(polibias_groups)),
                        {
                          rep_idx <- which(unlist(polibias_groups) == "Republican")
                          dem_idx <- which(unlist(polibias_groups) == "Democrat")
                          if(length(rep_idx) > 0 && length(dem_idx) > 0) {
                            abs(unlist(means)[rep_idx] - unlist(means)[dem_idx])
                          } else NA
                        }, NA)
  ) %>%
  dplyr::select(ExperimentType, BiasGap, BiasGap_SE, RepDem_Gap, n_groups)

# Add confidence intervals for bias gaps
bias_gaps_results <- bias_gaps_results %>%
  mutate(
    BiasGap_Lower = BiasGap - 1.96 * BiasGap_SE,
    BiasGap_Upper = BiasGap + 1.96 * BiasGap_SE,
    BiasGap_Lower = pmax(BiasGap_Lower, 0, na.rm = TRUE)  # Non-negative
  )

cat("\nBias Gaps (Political Variability) by ExperimentType:\n")
print(bias_gaps_results)

# =================================================
# 3. Specific political bias pairwise comparisons
# =================================================

cat("\n=== POLIBIAS PAIRWISE COMPARISONS WITHIN EACH EXPERIMENT TYPE ===\n")

# Use the interaction emmeans and do pairwise comparisons by ExperimentType
emm_for_polibias <- emmeans(bias_model, ~ PoliBias | ExperimentType)

cat("Emmeans for PoliBias by ExperimentType:\n")
print(summary(emm_for_polibias))

# Perform pairwise comparisons of PoliBias within each ExperimentType
polibias_pairwise <- pairs(emm_for_polibias, by = "ExperimentType", adjust = "none")

cat("\nPoliBias pairwise comparisons within each ExperimentType:\n")
print(summary(polibias_pairwise))

# Convert to dataframe for processing
polibias_results_df <- as.data.frame(polibias_pairwise)

if(nrow(polibias_results_df) > 0) {
  pooled_sd <- sigma(bias_model)
  
  combined_polibias_results <- polibias_results_df %>%
    mutate(
      cohens_d = estimate / pooled_sd,
      hedges_g = cohens_d * (1 - (3 / (4 * df - 1))),
      significant_raw = p.value < 0.05,
      effect_size_category = case_when(
        abs(hedges_g) < 0.2 ~ "Negligible",
        abs(hedges_g) < 0.5 ~ "Small",
        abs(hedges_g) < 0.8 ~ "Medium",
        TRUE ~ "Large"
      )
    ) %>%
    group_by(ExperimentType) %>%
    mutate(
      # Within-group FDR correction
      p_fdr_within = p.adjust(p.value, method = "fdr"),
      significant_fdr_within = p_fdr_within < 0.05
    ) %>%
    ungroup() %>%
    mutate(
      # Global FDR correction
      p_fdr_global = p.adjust(p.value, method = "fdr"),
      significant_fdr_global = p_fdr_global < 0.05
    ) %>%
    mutate_if(is.numeric, ~round(.x, 4))
  
  cat(sprintf("\nTotal PoliBias comparisons: %d\n", nrow(combined_polibias_results)))
} else {
  combined_polibias_results <- NULL
}

# =====================================
# 4. Bias mitigation comparison
# =====================================

cat("\n=== BIAS MITIGATION EFFECTIVENESS COMPARISONS ===\n")
cat("Comparing political consistency (bias gaps) between AI approaches\n")

# Define groups for bias mitigation comparisons
# Dual AI comparison
dual_ai_nonbiased_conditions <- c("Dual_AI_Non_Biased", "Dual_AI_Non_Biased_Exp")
dual_ai_balanced_conditions <- c("Dual_AI_Balanced")

# Single AI comparison
single_ai_nonbiased_conditions <- c("Single_AI_Non_Biased", "Single_AI_Non_Biased_Exp")
single_ai_opposition_conditions <- c("Single_AI_Opposition")

# Filter to available conditions
dual_ai_nonbiased_available <- intersect(dual_ai_nonbiased_conditions, experiment_types)
dual_ai_balanced_available <- intersect(dual_ai_balanced_conditions, experiment_types)
single_ai_nonbiased_available <- intersect(single_ai_nonbiased_conditions, experiment_types)
single_ai_opposition_available <- intersect(single_ai_opposition_conditions, experiment_types)

cat("=== COMPARISON GROUPS ===\n")
cat("Dual AI Non-biased conditions:", paste(dual_ai_nonbiased_available, collapse = ", "), "\n")
cat("Dual AI Balanced conditions:", paste(dual_ai_balanced_available, collapse = ", "), "\n")
cat("Single AI Non-biased conditions:", paste(single_ai_nonbiased_available, collapse = ", "), "\n")
cat("Single AI Opposition conditions:", paste(single_ai_opposition_available, collapse = ", "), "\n")

# Get sample sizes for proper weighting
sample_sizes_for_weighting <- combined_data %>%
  group_by(ExperimentType) %>%
  summarise(n = n(), .groups = 'drop')

cat("\nSample sizes by condition:\n")
print(sample_sizes_for_weighting)

# Function to calculate weighted bias gap for a group of conditions
calculate_weighted_bias_gap <- function(condition_names, group_label) {
  cat(sprintf("\n--- %s Bias Gap Calculation ---\n", group_label))
  
  condition_bias_gaps <- bias_gaps_results %>%
    filter(ExperimentType %in% condition_names)
  
  if(nrow(condition_bias_gaps) == 0) {
    cat(sprintf("No conditions found for %s\n", group_label))
    return(list(weighted_gap = NA, weighted_se = NA))
  }
  
  cat("Individual bias gaps:\n")
  print(condition_bias_gaps[, c("ExperimentType", "BiasGap", "BiasGap_SE")])
  
  if(nrow(condition_bias_gaps) == 1) {
    # Single condition - no weighting needed
    weighted_gap <- condition_bias_gaps$BiasGap[1]
    weighted_se <- condition_bias_gaps$BiasGap_SE[1]
    cat(sprintf("Single condition bias gap: %.4f (SE = %.4f)\n", weighted_gap, weighted_se))
  } else {
    # Multiple conditions - use sample-size weighting
    condition_with_n <- condition_bias_gaps %>%
      left_join(sample_sizes_for_weighting, by = "ExperimentType")
    
    total_n <- sum(condition_with_n$n, na.rm = TRUE)
    condition_with_n <- condition_with_n %>%
      mutate(weight = n / total_n)
    
    cat("Weighting details:\n")
    print(condition_with_n[, c("ExperimentType", "BiasGap", "n", "weight")])
    
    weighted_gap <- sum(condition_with_n$BiasGap * condition_with_n$weight, na.rm = TRUE)
    weighted_se <- sqrt(sum((condition_with_n$BiasGap_SE^2) * (condition_with_n$weight^2), na.rm = TRUE))
    
    cat(sprintf("Weighted bias gap: %.4f (SE = %.4f)\n", weighted_gap, weighted_se))
  }
  
  return(list(weighted_gap = weighted_gap, weighted_se = weighted_se))
}

# Calculate weighted bias gaps for all groups
dual_nonbiased_results <- calculate_weighted_bias_gap(dual_ai_nonbiased_available, "Dual AI Non-biased")
dual_balanced_results <- calculate_weighted_bias_gap(dual_ai_balanced_available, "Dual AI Balanced")
single_nonbiased_results <- calculate_weighted_bias_gap(single_ai_nonbiased_available, "Single AI Non-biased")
single_opposition_results <- calculate_weighted_bias_gap(single_ai_opposition_available, "Single AI Opposition")

# Function to perform bias mitigation comparison
perform_bias_mitigation_comparison <- function(group1_results, group2_results, group1_name, group2_name, comparison_name) {
  cat(sprintf("\n=== %s ===\n", comparison_name))
  
  gap1 <- group1_results$weighted_gap
  se1 <- group1_results$weighted_se
  gap2 <- group2_results$weighted_gap
  se2 <- group2_results$weighted_se
  
  if(is.na(gap1) || is.na(gap2)) {
    cat("Cannot perform comparison - missing data\n")
    return(NULL)
  }
  
  # Calculate bias mitigation effect
  bias_mitigation_effect <- gap1 - gap2  # positive = group2 better at bias mitigation
  bias_mitigation_se <- sqrt(se1^2 + se2^2)
  
  # Calculate Hedges' g for effect size
  pooled_sd <- sigma(bias_model)
  cohens_d <- bias_mitigation_effect / pooled_sd
  # Apply Hedges' correction
  df_conservative <- min(bias_gaps_results$n_groups, na.rm = TRUE) - 1
  df_conservative <- max(df_conservative, 10)  # minimum df
  hedges_g <- cohens_d * (1 - (3 / (4 * df_conservative - 1)))
  
  # Statistical test
  t_stat <- bias_mitigation_effect / bias_mitigation_se
  p_value <- 2 * pt(abs(t_stat), df = df_conservative, lower.tail = FALSE)
  
  # Confidence interval
  ci_lower <- bias_mitigation_effect - 1.96 * bias_mitigation_se
  ci_upper <- bias_mitigation_effect + 1.96 * bias_mitigation_se
  
  # Effect size category
  effect_size_category <- case_when(
    abs(hedges_g) < 0.2 ~ "Negligible",
    abs(hedges_g) < 0.5 ~ "Small", 
    abs(hedges_g) < 0.8 ~ "Medium",
    TRUE ~ "Large"
  )
  
  # Create results summary
  results <- data.frame(
    Group1_BiasGap = round(gap1, 4),
    Group2_BiasGap = round(gap2, 4),
    BiasReduction = round(bias_mitigation_effect, 4),
    SE = round(bias_mitigation_se, 4),
    CI_Lower = round(ci_lower, 4),
    CI_Upper = round(ci_upper, 4),
    hedges_g = round(hedges_g, 4),
    effect_size_category = effect_size_category,
    t_stat = round(t_stat, 3),
    p_value = round(p_value, 4),
    stringsAsFactors = FALSE
  )
  
  # Add meaningful column names
  names(results)[1] <- paste0(group1_name, "_BiasGap")
  names(results)[2] <- paste0(group2_name, "_BiasGap")
  
  cat("Results:\n")
  print(results)
  
  # Interpretation
  cat(sprintf("\nBias Reduction = %s Gap - %s Gap (positive = %s is better)\n", 
              group1_name, group2_name, group2_name))
  
  if(bias_mitigation_effect > 0 && p_value < 0.05) {
    cat("IGNIFICANT BIAS MITIGATION:\n")
    cat(sprintf("%s reduces political bias gaps by %.4f points (p = %.3f)\n", 
                group2_name, bias_mitigation_effect, p_value))
    cat(sprintf("Interpretation: %s leads to more consistent performance across political contexts\n", group2_name))
  } else if(bias_mitigation_effect > 0 && p_value >= 0.05) {
    cat("MARGINAL BIAS MITIGATION:\n")
    cat(sprintf("%s shows %.4f point reduction in bias gaps (p = %.3f, n.s.)\n", 
                group2_name, bias_mitigation_effect, p_value))
    cat("Interpretation: Suggestive evidence for bias mitigation, but not statistically significant\n")
  } else if(bias_mitigation_effect < 0 && p_value < 0.05) {
    cat("EVERSE EFFECT:\n")
    cat(sprintf("%s actually shows better consistency (%.4f points, p = %.3f)\n", 
                group1_name, abs(bias_mitigation_effect), p_value))
  } else {
    cat("➖ NO MEANINGFUL DIFFERENCE:\n")
    cat(sprintf("No significant difference in bias mitigation effectiveness (difference = %.4f, p = %.3f)\n", 
                bias_mitigation_effect, p_value))
  }
  
  cat(sprintf("95%% Confidence Interval for bias reduction: [%.4f, %.4f]\n", ci_lower, ci_upper))
  
  return(results)
}

# Perform both comparisons
cat("\n" %>% rep(3) %>% paste(collapse=""))
cat("=== BIAS MITIGATION EFFECTIVENESS RESULTS ===\n")
cat("Lower Bias Gap = Better bias mitigation (more consistent across Rep/Dem/Neutral news)\n")

# Dual AI comparison
dual_ai_comparison <- perform_bias_mitigation_comparison(
  dual_nonbiased_results, dual_balanced_results,
  "Dual_NonBiased", "Dual_Balanced",
  "DUAL AI COMPARISON: Non-Biased vs Balanced"
)

# Single AI comparison  
single_ai_comparison <- perform_bias_mitigation_comparison(
  single_nonbiased_results, single_opposition_results,
  "Single_NonBiased", "Single_Opposition", 
  "SINGLE AI COMPARISON: Non-Biased vs Opposition"
)

# Store results
if(!is.null(dual_ai_comparison)) {
  combined_dual_bias_mitigation_results <- dual_ai_comparison
}
if(!is.null(single_ai_comparison)) {
  combined_single_bias_mitigation_results <- single_ai_comparison
}

# =====================================
# 5. Summary results
# =====================================

cat("\n=== COMPREHENSIVE RESULTS SUMMARY ===\n")

# Pairwise comparison summary
if(!is.null(combined_pairwise_results)) {
  cat("1. EXPERIMENT TYPE PAIRWISE COMPARISONS:\n")
  total_comparisons <- nrow(combined_pairwise_results)
  raw_sig <- sum(combined_pairwise_results$significant_raw)
  within_fdr_sig <- sum(combined_pairwise_results$significant_fdr_within)
  global_fdr_sig <- sum(combined_pairwise_results$significant_fdr_global)
  
  cat(sprintf("   Total comparisons: %d\n", total_comparisons))
  cat(sprintf("   Raw significant (p < 0.05): %d (%.1f%%)\n", 
              raw_sig, 100 * raw_sig / total_comparisons))
  cat(sprintf("   Within-group FDR significant: %d (%.1f%%)\n", 
              within_fdr_sig, 100 * within_fdr_sig / total_comparisons))
  cat(sprintf("   Global FDR significant: %d (%.1f%%)\n", 
              global_fdr_sig, 100 * global_fdr_sig / total_comparisons))
  
  # Show significant comparisons
  if(global_fdr_sig > 0) {
    cat("\n   Significant comparisons (Global FDR < 0.05):\n")
    sig_results <- combined_pairwise_results %>%
      filter(significant_fdr_global) %>%
      dplyr::select(PoliBias, contrast, estimate, p.value, p_fdr_global, hedges_g) %>%
      arrange(PoliBias, p_fdr_global)
    print(sig_results)
  }
}

# Bias gaps summary
cat("\n2. POLITICAL BIAS GAPS BY EXPERIMENT TYPE:\n")
bias_summary <- bias_gaps_results %>%
  arrange(BiasGap) %>%
  dplyr::select(ExperimentType, BiasGap, BiasGap_SE, RepDem_Gap)
print(bias_summary)

# PoliBias comparison summary
if(exists("combined_polibias_results")) {
  cat("\n3. POLIBIAS COMPARISONS WITHIN EXPERIMENT TYPES:\n")
  if(sum(combined_polibias_results$significant_fdr_global) > 0) {
    polibias_sig <- combined_polibias_results %>%
      filter(significant_fdr_global) %>%
      dplyr::select(ExperimentType, contrast, estimate, p_fdr_global, hedges_g) %>%
      arrange(ExperimentType, p_fdr_global)
    print(polibias_sig)
  } else {
    cat("   No significant PoliBias differences after FDR correction\n")
  }
}

# Bias mitigation comparison summaries
if(exists("combined_dual_bias_mitigation_results")) {
  cat("\n4. DUAL AI BIAS MITIGATION COMPARISON:\n")
  print(combined_dual_bias_mitigation_results)
  
  if(combined_dual_bias_mitigation_results$BiasReduction > 0) {
    if(combined_dual_bias_mitigation_results$p_value < 0.05) {
      cat("   ✅ Dual Balanced AI significantly reduces political bias gaps\n")
    } else {
      cat("   ⚠️  Dual Balanced AI shows trend toward reducing bias gaps (not significant)\n")
    }
  } else {
    cat("   ➖ No evidence that Dual Balanced AI improves bias mitigation\n")
  }
}

if(exists("combined_single_bias_mitigation_results")) {
  cat("\n5. SINGLE AI BIAS MITIGATION COMPARISON:\n")
  print(combined_single_bias_mitigation_results)
  
  if(combined_single_bias_mitigation_results$BiasReduction > 0) {
    if(combined_single_bias_mitigation_results$p_value < 0.05) {
      cat("ingle Opposition AI significantly reduces political bias gaps\n")
    } else {
      cat("Single Opposition AI shows trend toward reducing bias gaps (not significant)\n")
    }
  } else {
    cat("No evidence that Single Opposition AI improves bias mitigation\n")
  }
}

