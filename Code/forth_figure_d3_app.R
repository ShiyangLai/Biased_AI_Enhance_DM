# Grouped Comparison Analysis: Non-Biased vs Other+Balanced

# Create grouped experimental conditions
combined_data <- combined_data %>%
  mutate(
    # Create grouped condition variable
    GroupedCondition = case_when(
      ExperimentType %in% c("Single_AI_Non_Biased", "Dual_AI_Non_Biased") ~ "Non_Biased",
      ExperimentType %in% c("Single_AI_Other", "Single_AI_Balanced", 
                            "Dual_AI_Other", "Dual_AI_Balanced") ~ "Other_Balanced",
      TRUE ~ "Other"
    ),
    
    # Also create AI type for potential interaction analysis
    AI_Type = case_when(
      grepl("Single", ExperimentType) ~ "Single_AI",
      grepl("Dual", ExperimentType) ~ "Dual_AI",
      TRUE ~ "Unknown"
    )
  )

# Check distribution of grouped conditions
cat("=== GROUPED CONDITION DISTRIBUTION ===\n")
table(combined_data$GroupedCondition, combined_data$AI_Type)

# Function for grouped bias analysis (robust version)
analyze_grouped_bias <- function(data) {
  library(sandwich)
  library(emmeans)
  
  # Clean data
  data_clean <- data %>%
    filter(!is.na(PostPerformance), 
           !is.na(PrePerformance),
           !is.na(GroupedCondition),
           !is.na(PoliBias),
           !is.na(NID),
           !is.na(UStanceLabel),
           !is.na(AIStanceLabel_S))
  
  cat("Clean data for grouped analysis:", nrow(data_clean), "observations\n")
  cat("Grouped condition distribution:\n")
  print(table(data_clean$GroupedCondition))
  
  # Check factor levels after filtering to avoid "contrasts" error
  cat("Checking factor levels after filtering...\n")
  
  # Check each potential factor variable
  polibias_levels <- length(unique(data_clean$PoliBias))
  ustance_levels <- length(unique(data_clean$UStanceLabel))
  aistance_levels <- length(unique(data_clean$AIStanceLabel_S))
  nid_levels <- length(unique(data_clean$NID))
  grouped_levels <- length(unique(data_clean$GroupedCondition))
  
  cat(sprintf("PoliBias levels: %d\n", polibias_levels))
  cat(sprintf("UStanceLabel levels: %d\n", ustance_levels))
  cat(sprintf("AIStanceLabel_S levels: %d\n", aistance_levels))
  cat(sprintf("NID levels: %d\n", nid_levels))
  cat(sprintf("GroupedCondition levels: %d\n", grouped_levels))
  
  # Check if we have enough variation for analysis
  if(grouped_levels < 2) {
    return(list(error = "Insufficient variation in GroupedCondition"))
  }
  if(polibias_levels < 2) {
    return(list(error = "Insufficient variation in PoliBias"))
  }
  
  # Build model formula dynamically based on available factors
  model_terms <- c("PrePerformance", "GroupedCondition * PoliBias")
  
  # Only include factors with sufficient variation (>1 level) and reasonable sample sizes
  if(nid_levels > 1 && nid_levels < nrow(data_clean)/5) {
    model_terms <- c(model_terms, "as.factor(NID)")
  }
  if(ustance_levels > 1) {
    model_terms <- c(model_terms, "as.factor(UStanceLabel)")  
  }
  if(aistance_levels > 1) {
    model_terms <- c(model_terms, "as.factor(AIStanceLabel_S)")
  }
  
  model_formula <- as.formula(paste("PostPerformance ~", paste(model_terms, collapse = " + ")))
  cat("Model formula:", deparse(model_formula), "\n")
  
  # Fit the model with error handling
  model_result <- tryCatch({
    model_grouped <- lm(model_formula, data = data_clean)
    
    # Clustered standard errors
    vcov_grouped <- vcovCL(model_grouped, cluster = data_clean$UID)
    
    # Emmeans for grouped analysis
    emm_grouped <- emmeans(model_grouped, ~ GroupedCondition * PoliBias, vcov. = vcov_grouped)
    
    # Performance by grouped condition
    performance_grouped <- as.data.frame(emm_grouped) %>%
      rename(MeanImprovement = emmean)
    
    # Calculate bias gaps for grouped conditions
    emm_by_group <- emmeans(model_grouped, ~ PoliBias | GroupedCondition, vcov. = vcov_grouped)
    
    # Political bias contrasts within each grouped condition
    available_polibias <- unique(data_clean$PoliBias)
    cat("Available political bias categories:", available_polibias, "\n")
    
    # Initialize contrast results
    grouped_gaps <- data.frame()
    
    # Calculate contrasts if we have the necessary categories
    if(all(c("Republican", "Democrat") %in% available_polibias)) {
      rep_vs_dem <- contrast(emm_by_group, list("Rep-Dem" = c(-1, 0, 1)), by = "GroupedCondition")
      rep_vs_dem_df <- as.data.frame(rep_vs_dem) %>%
        mutate(Comparison = "Rep vs Dem")
      grouped_gaps <- bind_rows(grouped_gaps, rep_vs_dem_df)
    }
    
    if(all(c("Republican", "Neutral") %in% available_polibias)) {
      rep_vs_neutral <- contrast(emm_by_group, list("Rep-Neutral" = c(0, -1, 1)), by = "GroupedCondition")
      rep_vs_neutral_df <- as.data.frame(rep_vs_neutral) %>%
        mutate(Comparison = "Rep vs Neutral")
      grouped_gaps <- bind_rows(grouped_gaps, rep_vs_neutral_df)
    }
    
    if(all(c("Democrat", "Neutral") %in% available_polibias)) {
      dem_vs_neutral <- contrast(emm_by_group, list("Dem-Neutral" = c(1, -1, 0)), by = "GroupedCondition")
      dem_vs_neutral_df <- as.data.frame(dem_vs_neutral) %>%
        mutate(Comparison = "Dem vs Neutral")
      grouped_gaps <- bind_rows(grouped_gaps, dem_vs_neutral_df)
    }
    
    # Summary of bias gaps by grouped condition
    bias_summary_grouped <- grouped_gaps %>%
      group_by(GroupedCondition) %>%
      summarise(
        Average_Abs_Gap = mean(abs(estimate), na.rm = TRUE),
        Average_SE = sqrt(mean(SE^2, na.rm = TRUE)),
        N_Comparisons = sum(!is.na(estimate)),
        .groups = 'drop'
      ) %>%
      mutate(
        Lower_CI = Average_Abs_Gap - 1.96 * Average_SE,
        Upper_CI = Average_Abs_Gap + 1.96 * Average_SE,
        Lower_CI = pmax(Lower_CI, 0)
      )
    
    # Return successful result
    list(
      model_grouped = model_grouped,
      vcov_grouped = vcov_grouped,
      performance_grouped = performance_grouped,
      grouped_gaps = grouped_gaps,
      bias_summary_grouped = bias_summary_grouped,
      data_used = data_clean,
      model_formula = model_formula,
      success = TRUE
    )
    
  }, error = function(e) {
    cat("Error fitting model:", e$message, "\n")
    list(error = paste("Model fitting failed:", e$message), success = FALSE)
  })
  
  return(model_result)
}

model_result <- analyze_grouped_bias(combined_data)

# Clustered standard errors
model_grouped <- model_result$model_grouped
vcov_grouped <- vcovCL(model_result$model_grouped, cluster = combined_data$UID)
vcov_grouped_ai <- vcovCL(model_result$model_grouped, cluster = combined_data$UID)

# Emmeans for grouped analysis
emm_grouped <- emmeans(model_grouped, ~ GroupedCondition * PoliBias, vcov. = vcov_grouped)

# Performance by grouped condition
performance_grouped <- as.data.frame(emm_grouped) %>%
  rename(MeanImprovement = emmean)

# Calculate bias gaps for grouped conditions
emm_by_group <- emmeans(model_grouped, ~ PoliBias | GroupedCondition, vcov. = vcov_grouped)

# Political bias contrasts within each grouped condition
available_polibias <- unique(combined_data$PoliBias)
cat("Available political bias categories:", available_polibias, "\n")

# Initialize contrast results
grouped_gaps <- data.frame()

# Calculate contrasts if we have the necessary categories
if(all(c("Republican", "Democrat") %in% available_polibias)) {
  rep_vs_dem <- contrast(emm_by_group, list("Rep-Dem" = c(-1, 0, 1)), by = "GroupedCondition")
  rep_vs_dem_df <- as.data.frame(rep_vs_dem) %>%
    mutate(Comparison = "Rep vs Dem")
  grouped_gaps <- bind_rows(grouped_gaps, rep_vs_dem_df)
}

if(all(c("Republican", "Neutral") %in% available_polibias)) {
  rep_vs_neutral <- contrast(emm_by_group, list("Rep-Neutral" = c(0, -1, 1)), by = "GroupedCondition")
  rep_vs_neutral_df <- as.data.frame(rep_vs_neutral) %>%
    mutate(Comparison = "Rep vs Neutral")
  grouped_gaps <- bind_rows(grouped_gaps, rep_vs_neutral_df)
}

if(all(c("Democrat", "Neutral") %in% available_polibias)) {
  dem_vs_neutral <- contrast(emm_by_group, list("Dem-Neutral" = c(1, -1, 0)), by = "GroupedCondition")
  dem_vs_neutral_df <- as.data.frame(dem_vs_neutral) %>%
    mutate(Comparison = "Dem vs Neutral")
  grouped_gaps <- bind_rows(grouped_gaps, dem_vs_neutral_df)
}

# Summary of bias gaps by grouped condition
bias_summary_grouped <- grouped_gaps %>%
  group_by(GroupedCondition) %>%
  summarise(
    Average_Abs_Gap = mean(abs(estimate), na.rm = TRUE),
    Average_SE = sqrt(mean(SE^2, na.rm = TRUE)),
    N_Comparisons = sum(!is.na(estimate)),
    .groups = 'drop'
  ) %>%
  mutate(
    Lower_CI = Average_Abs_Gap - 1.96 * Average_SE,
    Upper_CI = Average_Abs_Gap + 1.96 * Average_SE,
    Lower_CI = pmax(Lower_CI, 0)
  )

# Run grouped analysis
cat("\n=== RUNNING GROUPED BIAS ANALYSIS ===\n")
grouped_analysis <- analyze_grouped_bias(combined_data)

# Print results
cat("\n=== GROUPED BIAS MITIGATION RESULTS ===\n")
print(grouped_analysis$bias_summary_grouped)

# Statistical test: Non-Biased vs Other+Balanced
if(nrow(grouped_analysis$bias_summary_grouped) >= 2) {
  
  # Direct comparison of bias measures
  non_biased_bias <- grouped_analysis$bias_summary_grouped$Average_Abs_Gap[
    grouped_analysis$bias_summary_grouped$GroupedCondition == "Non_Biased"]
  other_balanced_bias <- grouped_analysis$bias_summary_grouped$Average_Abs_Gap[
    grouped_analysis$bias_summary_grouped$GroupedCondition == "Other_Balanced"]
  
  if(length(non_biased_bias) > 0 && length(other_balanced_bias) > 0) {
    cat("\n=== KEY COMPARISON: NON-BIASED vs OTHER+BALANCED ===\n")
    cat(sprintf("Non-Biased average bias: %.4f\n", non_biased_bias))
    cat(sprintf("Other+Balanced average bias: %.4f\n", other_balanced_bias))
    cat(sprintf("Difference: %.4f (positive = Non-Biased has higher bias)\n", 
                non_biased_bias - other_balanced_bias))
  }
  
  # Permutation test for grouped comparison
  permutation_test_grouped <- function(data, n_permutations = 1000) {
    
    # Calculate observed difference
    obs_bias_nonbiased <- data %>%
      filter(GroupedCondition == "Non_Biased") %>%
      group_by(PoliBias) %>%
      summarise(MeanPost = mean(PostPerformance, na.rm = TRUE), .groups = 'drop')
    
    obs_bias_other <- data %>%
      filter(GroupedCondition == "Other_Balanced") %>%
      group_by(PoliBias) %>%
      summarise(MeanPost = mean(PostPerformance, na.rm = TRUE), .groups = 'drop')
    
    # Calculate bias measures for each group
    calc_group_bias <- function(group_data) {
      if(nrow(group_data) < 2) return(NA)
      polibias_present <- group_data$PoliBias
      
      gaps <- c()
      if(all(c("Republican", "Democrat") %in% polibias_present)) {
        rep_mean <- group_data$MeanPost[group_data$PoliBias == "Republican"]
        dem_mean <- group_data$MeanPost[group_data$PoliBias == "Democrat"]
        gaps <- c(gaps, abs(rep_mean - dem_mean))
      }
      if(all(c("Republican", "Neutral") %in% polibias_present)) {
        rep_mean <- group_data$MeanPost[group_data$PoliBias == "Republican"]
        neu_mean <- group_data$MeanPost[group_data$PoliBias == "Neutral"]
        gaps <- c(gaps, abs(rep_mean - neu_mean))
      }
      if(all(c("Democrat", "Neutral") %in% polibias_present)) {
        dem_mean <- group_data$MeanPost[group_data$PoliBias == "Democrat"]
        neu_mean <- group_data$MeanPost[group_data$PoliBias == "Neutral"]
        gaps <- c(gaps, abs(dem_mean - neu_mean))
      }
      
      if(length(gaps) > 0) return(mean(gaps)) else return(NA)
    }
    
    observed_bias_nonbiased <- calc_group_bias(obs_bias_nonbiased)
    observed_bias_other <- calc_group_bias(obs_bias_other)
    
    if(is.na(observed_bias_nonbiased) || is.na(observed_bias_other)) {
      return(list(observed_diff = NA, p_value = NA, error = "Cannot calculate bias measures"))
    }
    
    observed_diff <- observed_bias_nonbiased - observed_bias_other
    
    # Permutation test
    set.seed(123)
    comparison_data <- data %>%
      filter(GroupedCondition %in% c("Non_Biased", "Other_Balanced"))
    
    perm_differences <- replicate(n_permutations, {
      shuffled_data <- comparison_data
      shuffled_data$GroupedCondition <- sample(comparison_data$GroupedCondition)
      
      perm_bias_nonbiased <- shuffled_data %>%
        filter(GroupedCondition == "Non_Biased") %>%
        group_by(PoliBias) %>%
        summarise(MeanPost = mean(PostPerformance, na.rm = TRUE), .groups = 'drop')
      
      perm_bias_other <- shuffled_data %>%
        filter(GroupedCondition == "Other_Balanced") %>%
        group_by(PoliBias) %>%
        summarise(MeanPost = mean(PostPerformance, na.rm = TRUE), .groups = 'drop')
      
      perm_bias_nb <- calc_group_bias(perm_bias_nonbiased)
      perm_bias_ob <- calc_group_bias(perm_bias_other)
      
      if(is.na(perm_bias_nb) || is.na(perm_bias_ob)) return(NA)
      return(perm_bias_nb - perm_bias_ob)
    })
    
    perm_differences <- perm_differences[!is.na(perm_differences)]
    if(length(perm_differences) == 0) {
      return(list(observed_diff = observed_diff, p_value = NA, error = "All permutations failed"))
    }
    
    p_value <- mean(abs(perm_differences) >= abs(observed_diff))
    
    return(list(
      observed_diff = observed_diff,
      p_value = p_value,
      n_valid_perms = length(perm_differences),
      observed_bias_nonbiased = observed_bias_nonbiased,
      observed_bias_other = observed_bias_other
    ))
  }
  
  # Run permutation test
  cat("\n=== PERMUTATION TEST: NON-BIASED vs OTHER+BALANCED ===\n")
  perm_result <- permutation_test_grouped(grouped_analysis$data_used, n_permutations = 1500)
  
  if(!is.null(perm_result$p_value) && !is.na(perm_result$p_value)) {
    sig_symbol <- ifelse(perm_result$p_value < 0.001, "***",
                         ifelse(perm_result$p_value < 0.01, "**",
                                ifelse(perm_result$p_value < 0.05, "*",
                                       ifelse(perm_result$p_value < 0.1, "†", "ns"))))
    
    cat(sprintf("Non-Biased bias measure: %.4f\n", perm_result$observed_bias_nonbiased))
    cat(sprintf("Other+Balanced bias measure: %.4f\n", perm_result$observed_bias_other))
    cat(sprintf("Observed difference: %.4f\n", perm_result$observed_diff))
    cat(sprintf("P-value: %.4f %s\n", perm_result$p_value, sig_symbol))
    cat(sprintf("Valid permutations: %d\n", perm_result$n_valid_perms))
    
    if(perm_result$p_value < 0.05) {
      if(perm_result$observed_diff < 0) {
        cat("*** Non-Biased conditions show significantly BETTER bias mitigation (lower bias)\n")
      } else {
        cat("*** Non-Biased conditions show significantly WORSE bias mitigation (higher bias)\n")
      }
    } else {
      cat("No significant difference in bias mitigation between grouped conditions\n")
    }
  } else {
    cat("Permutation test failed - check data structure\n")
  }
}

# Additional analysis: Within AI type comparisons
cat("\n=== WITHIN AI TYPE ANALYSIS ===\n")

# Single AI: Non-Biased vs Other+Balanced
single_ai_data <- combined_data %>%
  filter(AI_Type == "Single_AI") %>%
  mutate(GroupedCondition = case_when(
    ExperimentType == "Single_AI_Non_Biased" ~ "Non_Biased",
    ExperimentType %in% c("Single_AI_Other", "Single_AI_Balanced") ~ "Other_Balanced"
  )) %>%
  filter(!is.na(GroupedCondition))  # Remove any rows where grouping failed

# Dual AI: Non-Biased vs Other+Balanced  
dual_ai_data <- combined_data %>%
  filter(AI_Type == "Dual_AI") %>%
  mutate(GroupedCondition = case_when(
    ExperimentType == "Dual_AI_Non_Biased" ~ "Non_Biased",
    ExperimentType %in% c("Dual_AI_Other", "Dual_AI_Balanced") ~ "Other_Balanced"
  )) %>%
  filter(!is.na(GroupedCondition))  # Remove any rows where grouping failed

# Run analysis for each AI type separately with better error handling
if(nrow(single_ai_data) > 50) {
  cat("Single AI Analysis:\n")
  cat("Single AI data rows:", nrow(single_ai_data), "\n")
  cat("Single AI grouped condition distribution:\n")
  print(table(single_ai_data$GroupedCondition))
  
  single_analysis <- analyze_grouped_bias(single_ai_data)
  
  if(!single_analysis$success || "error" %in% names(single_analysis)) {
    cat("Single AI analysis failed:", single_analysis$error, "\n")
  } else {
    print(single_analysis$bias_summary_grouped)
  }
} else {
  cat("Insufficient Single AI data for analysis (n =", nrow(single_ai_data), ")\n")
}

if(nrow(dual_ai_data) > 50) {
  cat("\nDual AI Analysis:\n")
  cat("Dual AI data rows:", nrow(dual_ai_data), "\n")
  cat("Dual AI grouped condition distribution:\n")
  print(table(dual_ai_data$GroupedCondition))
  
  dual_analysis <- analyze_grouped_bias(dual_ai_data)
  
  if(!dual_analysis$success || "error" %in% names(dual_analysis)) {
    cat("Dual AI analysis failed:", dual_analysis$error, "\n")
  } else {
    cat("Dual AI bias mitigation results:\n")
    print(dual_analysis$bias_summary_grouped)
    
    # ADD THIS SECTION FOR DUAL AI PERMUTATION TEST:
    if(nrow(dual_analysis$bias_summary_grouped) >= 2) {
      cat("\n=== PERMUTATION TEST: NON-BIASED vs OTHER+BALANCED (DUAL AI ONLY) ===\n")
      dual_perm_result <- permutation_test_grouped(dual_ai_data, n_permutations = 1500)
      
      if(!is.null(dual_perm_result$p_value) && !is.na(dual_perm_result$p_value)) {
        sig_symbol <- ifelse(dual_perm_result$p_value < 0.001, "***",
                             ifelse(dual_perm_result$p_value < 0.01, "**",
                                    ifelse(dual_perm_result$p_value < 0.05, "*",
                                           ifelse(dual_perm_result$p_value < 0.1, "†", "ns"))))
        
        cat(sprintf("Dual AI - Non-Biased bias measure: %.4f\n", dual_perm_result$observed_bias_nonbiased))
        cat(sprintf("Dual AI - Other+Balanced bias measure: %.4f\n", dual_perm_result$observed_bias_other))
        cat(sprintf("Dual AI - Observed difference: %.4f\n", dual_perm_result$observed_diff))
        cat(sprintf("Dual AI - P-value: %.4f %s\n", dual_perm_result$p_value, sig_symbol))
        cat(sprintf("Dual AI - Valid permutations: %d\n", dual_perm_result$n_valid_perms))
        
        if(dual_perm_result$p_value < 0.05) {
          if(dual_perm_result$observed_diff < 0) {
            cat("*** Dual AI: Non-Biased shows significantly BETTER bias mitigation (lower bias)\n")
          } else {
            cat("*** Dual AI: Non-Biased shows significantly WORSE bias mitigation (higher bias)\n")
          }
        } else {
          cat("Dual AI: No significant difference between grouped conditions\n")
        }
        
        # Effect size for Dual AI
        pooled_sd_dual <- sd(dual_ai_data$PostPerformance, na.rm = TRUE)
        cohens_d_dual <- dual_perm_result$observed_diff / pooled_sd_dual
        cat(sprintf("Dual AI Effect size (Cohen's d): %.3f\n", cohens_d_dual))
        
      } else {
        cat("Dual AI permutation test failed - check data structure\n")
      }
    }
  }
}

# Effect size calculation
if(exists("perm_result") && !is.na(perm_result$observed_diff)) {
  pooled_sd <- sd(combined_data$PostPerformance, na.rm = TRUE)
  cohens_d <- perm_result$observed_diff / pooled_sd
  cat(sprintf("\nEffect size (Cohen's d): %.3f\n", cohens_d))
  
  effect_interpretation <- case_when(
    abs(cohens_d) < 0.2 ~ "negligible",
    abs(cohens_d) < 0.5 ~ "small", 
    abs(cohens_d) < 0.8 ~ "medium",
    TRUE ~ "large"
  )
  cat(sprintf("Effect size interpretation: %s\n", effect_interpretation))
}

# Simplified analysis function for cases with limited factor variation
simplified_grouped_analysis <- function(data) {
  cat("Running simplified analysis...\n")
  
  # Clean data
  data_clean <- data %>%
    filter(!is.na(PostPerformance), 
           !is.na(PoliBias),
           !is.na(GroupedCondition))
  
  cat("Clean data:", nrow(data_clean), "observations\n")
  
  # Simple calculation of bias measures by grouped condition
  bias_summary <- data_clean %>%
    group_by(GroupedCondition, PoliBias) %>%
    summarise(MeanPost = mean(PostPerformance, na.rm = TRUE),
              N = n(), .groups = 'drop') %>%
    filter(N >= 5) %>%  # Only include groups with sufficient observations
    group_by(GroupedCondition) %>%
    summarise(
      # Calculate bias gaps if we have multiple political groups
      Rep_Dem_Gap = ifelse(all(c("Republican", "Democrat") %in% PoliBias),
                           abs(MeanPost[PoliBias == "Republican"] - MeanPost[PoliBias == "Democrat"]), NA),
      Rep_Neutral_Gap = ifelse(all(c("Republican", "Neutral") %in% PoliBias),
                               abs(MeanPost[PoliBias == "Republican"] - MeanPost[PoliBias == "Neutral"]), NA),
      Dem_Neutral_Gap = ifelse(all(c("Democrat", "Neutral") %in% PoliBias),
                               abs(MeanPost[PoliBias == "Democrat"] - MeanPost[PoliBias == "Neutral"]), NA),
      .groups = 'drop'
    ) %>%
    mutate(
      # Average absolute gap (excluding NAs)
      Average_Abs_Gap = rowMeans(select(., Rep_Dem_Gap, Rep_Neutral_Gap, Dem_Neutral_Gap), na.rm = TRUE),
      N_Comparisons = rowSums(!is.na(select(., Rep_Dem_Gap, Rep_Neutral_Gap, Dem_Neutral_Gap)))
    ) %>%
    filter(N_Comparisons > 0)  # Only keep rows with at least one valid comparison
  
  return(bias_summary)
}

# If main analysis failed, try simplified version
if(nrow(single_ai_data) > 50) {
  if(!exists("single_analysis") || !single_analysis$success) {
    cat("Trying simplified Single AI analysis...\n")
    single_simple <- simplified_grouped_analysis(single_ai_data)
    if(nrow(single_simple) > 0) {
      cat("Simplified Single AI results:\n")
      print(single_simple)
    }
  }
}

if(nrow(dual_ai_data) > 50) {
  if(!exists("dual_analysis") || !dual_analysis$success) {
    cat("Trying simplified Dual AI analysis...\n")
    dual_simple <- simplified_grouped_analysis(dual_ai_data)
    if(nrow(dual_simple) > 0) {
      cat("Simplified Dual AI results:\n")
      print(dual_simple)
    }
  }
}