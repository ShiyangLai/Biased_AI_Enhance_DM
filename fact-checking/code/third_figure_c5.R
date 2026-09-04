# =============================================================================
# Mixed effects bias analysis functions
# =============================================================================
# Fit the mixed effects model
model <- lmer(PostPerformance ~ PrePerformance + 
                BiasSide * PoliBias + UStanceLabel +
                as.factor(NID) +
                (1 | UID), 
              data = repdem_single_ai)

# Get model summary and key parameters
model_summary <- tidy(model, effects = "fixed")
model_sigma <- sigma(model)
model_df <- df.residual(model)
summary(model)$sigma 
performance::r2(model)

# Calculate Hedges' g correction factor
J <- 1 - (3 / (4 * model_df - 1))

# Extract interaction terms
interaction_terms <- model_summary %>%
  dplyr::filter(grepl("BiasSide*PoliBias", term))

# ANOVA to test interaction significance
interaction_test <- anova(model)

# Get marginal means for BiasSide (overall effect)
marginal_means <- emmeans(model, ~ BiasSide)
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

# Get emmeans for BiasSide by PoliBias
emm_by_biased_polibias <- emmeans(model, ~ BiasSide | PoliBias)
emm_summary <- as.data.frame(emm_by_biased_polibias)

# Try to create bias scores manually
# First, check if BiasSide column exists in emm_summary
if("BiasSide" %in% names(emm_summary)) {
  print("BiasSide column found")
  
  # Calculate bias scores for each BiasSide
  bias_scores_step1 <- emm_summary %>%
    group_by(BiasSide) %>%
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
    dplyr::select(BiasSide, BiasScore, BiasScore_SE, n_groups) %>%
    mutate(
      Lower_CI = BiasScore - 1.96 * BiasScore_SE,
      Upper_CI = BiasScore + 1.96 * BiasScore_SE,
      Lower_CI = pmax(Lower_CI, 0, na.rm = TRUE)
    )
  
} else {
  print("BiasSide column NOT found in emm_summary")
  print("Available columns:")
  print(names(emm_summary))
}

# Create effect sizes summary
effect_sizes_summary <- data.frame(
  Comparison = "Same_vs_Opposite",
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

# Rename the first column to BiasSide for easier joining
names(performance_by_group)[1] <- "BiasSide"

performance_by_group <- performance_by_group %>%
  left_join(counts, by = c("BiasSide", "PoliBias"))

# =============================================================================
# Compare Same vs Opposition AI for each political condition pair gap
# =============================================================================
calculate_condition_pair_comparisons_debug <- function(emm_data, model_df = NULL) {
  ## degrees-of-freedom for the t-tests
  if (is.null(model_df)) model_df <- Inf   # supply df.residual(lm) if you want finite df
  
  ## detect “Same / Opposite” and political-bias columns
  bias_col <- names(emm_data)[sapply(emm_data, \(x) any(c("Same","Opposite") %in% x))]
  poli_col <- names(emm_data)[sapply(emm_data, \(x) any(c("Republican","Neutral","Democrat") %in% x))]
  if (length(bias_col)==0 || length(poli_col)==0)
    stop("Columns for bias or political stance not found.")
  bias_col <- bias_col[1];  poli_col <- poli_col[1]
  
  ## pairs to compare
  pairs <- list(c("Republican","Neutral"),
                c("Republican","Democrat"),
                c("Democrat","Neutral"))
  
  results <- data.frame()
  
  for (pr in pairs) {
    c1 <- pr[1];  c2 <- pr[2];  label <- paste(c1,"vs",c2)
    
    same  <- subset(emm_data, emm_data[[bias_col]]=="Same"     & emm_data[[poli_col]] %in% pr)
    opp   <- subset(emm_data, emm_data[[bias_col]]=="Opposite" & emm_data[[poli_col]] %in% pr)
    
    if (nrow(same)<2 || nrow(opp)<2) {
      cat("Skipping", label, "(missing levels)\n")
      next
    }
    
    ## Same-bias gap, SE, p
    s_gap <- abs(diff(same$emmean))
    s_se  <- sqrt(sum(same$SE^2))
    s_t   <- s_gap / s_se
    s_p   <- 2*(1 - pt(abs(s_t), df = model_df))
    
    ## Opposite-bias gap, SE, p
    o_gap <- abs(diff(opp$emmean))
    o_se  <- sqrt(sum(opp$SE^2))
    o_t   <- o_gap / o_se
    o_p   <- 2*(1 - pt(abs(o_t), df = model_df))
    
    ## Difference in gaps, SE, p
    diff_gap <- s_gap - o_gap
    diff_se  <- sqrt(s_se^2 + o_se^2)
    diff_t   <- diff_gap / diff_se
    diff_p   <- 2*(1 - pt(abs(diff_t), df = model_df))
    
    ## print -----------------------------------------------------------
    cat("\n---", label, "---\n",
        sprintf("Same gap = %.3f (SE %.3f)  P-value = %.3f\n",  s_gap,  s_se,  s_p),
        sprintf("Opp gap  = %.3f (SE %.3f)  P-value = %.3f\n",  o_gap, o_se,  o_p),
        sprintf("Gap diff = %.3f (SE %.3f)  P-value = %.3f\n", diff_gap, diff_se, diff_p),
        sep = "")
    
    ## store -----------------------------------------------------------
    results <- rbind(results, data.frame(
      Pair          = label,
      Same_Gap      = s_gap,  Same_SE = s_se,  Same_p = s_p,
      Opposite_Gap  = o_gap,  Opposite_SE = o_se,  Opposite_p = o_p,
      Gap_Diff      = diff_gap, Gap_Diff_SE = diff_se, Gap_Diff_p = diff_p
    ))
  }
  
  invisible(results)   # returned invisibly; assign to keep
}

# Run the debugging version
cat("\n=== RUNNING DEBUG VERSION ===\n")
condition_pair_results_debug <- calculate_condition_pair_comparisons_debug(emm_summary)

cat("\n=== FINAL RESULTS ===\n")
print(condition_pair_results_debug)

# Print comprehensive interpretation
cat("\n=== COMPREHENSIVE BIAS MITIGATION ANALYSIS (Mixed Effects) ===\n")
cat("Political Bias Scores (Average Absolute Pairwise Differences):\n")
for (i in seq_len(nrow(bias_scores))) {
  if (!is.na(bias_scores$BiasScore[i])) {
    
    ## extract
    est   <- bias_scores$BiasScore[i]
    lower <- bias_scores$Lower_CI[i]
    upper <- bias_scores$Upper_CI[i]
    k     <- bias_scores$n_groups[i]           # number of political groups
    
    ## 1. SE from 95 % CI  (uses t_0.975 with df = k-1; for large df the z≈1.96)
    df    <- max(k - 1, 1)                     # avoid df = 0
    se    <- (upper - lower) / (2 * qt(0.975, df))
    
    ## 2. test against 0
    t_stat <- est / se
    p_val  <- 2 * (1 - pt(abs(t_stat), df = df))
    
    ## print
    cat(sprintf("%s AI: %.3f (95%% CI: %.3f to %.3f)  p = %.3f  [%d political groups]\n",
                bias_scores$BiasSide[i], est, lower, upper, p_val, k))
  }
}

# Calculate bias mitigation effect
if(nrow(bias_scores) == 2 && all(!is.na(bias_scores$BiasScore))) {
  biased_idx <- which(bias_scores$BiasSide == "Same")
  nonbiased_idx <- which(bias_scores$BiasSide == "Opposite")
  
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
      cat("Interpretation: Same Bias AI shows significantly higher political bias than Opposite bais AI.\n")
      cat("*** BIAS MITIGATION SUCCESSFUL ***\n")
    } else if(bias_reduction > 0 && p_value >= 0.05) {
      cat("Interpretation: Same bias AI shows higher political bias, but difference is not statistically significant.\n")
    } else if(bias_reduction < 0 && p_value < 0.05) {
      cat("Interpretation: Opposite Bias AI unexpectedly shows higher political bias.\n")
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
