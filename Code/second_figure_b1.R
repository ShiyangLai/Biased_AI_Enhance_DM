set.seed(123) 

# ======================================
# Model specification using BiasedCat
# ======================================
# Create BiasedCat variable
single_ai_processed_$BiasedCat <- ifelse(single_ai_processed_$BiasedType == "Non-Biased", "No Bias",
                                         ifelse(single_ai_processed_$AIStanceLabel %in% c("Strong Republican", "Strong Democrat"),
                                                "Strong Bias",
                                                "Moderate Bias"))

# Check available levels in BiasedCat
cat("Unique values in BiasedCat:", paste(unique(single_ai_processed_$BiasedCat), collapse = ", "), "\n")

# Ensure BiasedCat is a factor with proper levels
single_ai_processed_$BiasedCat <- factor(single_ai_processed_$BiasedCat, 
                                         levels = c("No Bias", "Moderate Bias", "Strong Bias"))

# Create numeric version of AIInterMean
complete_data <- complete_data %>%
  mutate(AIInterMean_numeric = recode(AIInterMean,
                                      "Not meaningful at all" = 1,
                                      "Slightly meaningful" = 2,
                                      "Moderately meaningful" = 3,
                                      "Very meaningful" = 4,
                                      "Extremely meaningful" = 5
  ))

# ------- OLS + Robust SD -------
model_real <- lm(PostPerformance ~ BiasedCat + as.factor(NID) + PrePerformance +
                   UIdeo + UStanceLabel + AICorrectness,
                 data = complete_data)

complete_data$PerceivedImproveCode <- factor(complete_data$PerceivedImproveCode, ordered = TRUE)

model_perceived <- clm(
  PerceivedImproveCode ~ BiasedCat + as.factor(NID) + PrePerformance +
    UIdeo + UStanceLabel + AICorrectness,
  data = complete_data, na.action = na.omit
)

complete_data$AIInterMean_numeric <- factor(complete_data$AIInterMean_numeric, ordered = TRUE)

model_meaningful <- clm(
  AIInterMean_numeric ~ BiasedCat + as.factor(NID) + PrePerformance + UIdeo +
    UStanceLabel + AICorrectness,
  data = complete_data, na.action = na.omit
)

# Calculate clustered standard errors for all three models
vcov_real <- vcovCL(model_real, cluster = complete_data$UID)
vcov_perceived <- vcovCL(model_perceived, cluster = complete_data$UID)
vcov_meaningful <- vcovCL(model_meaningful, cluster = complete_data$UID)

# Get coefficient tests with clustered SEs
clustered_results_real <- coeftest(model_real, vcov = vcov_real)
clustered_results_perceived <- coeftest(model_perceived, vcov = vcov_perceived)
clustered_results_meaningful <- coeftest(model_meaningful, vcov = vcov_meaningful)

cat("=== MODEL RESULTS ===\n")
cat("ACTUAL PERFORMANCE MODEL:\n")
print(clustered_results_real)
summary(model_real)

cat("\nPERCEIVED IMPROVEMENT MODEL:\n")
print(clustered_results_perceived)
summary(model_perceived)
df.residual(model_perceived)
r2(model_perceived)
link_name <- model_perceived$link
resid_sd  <- switch(link_name,
                    "logit"  = pi / sqrt(3),
                    "probit" = 1,
                    "cloglog" = 1,
                    "loglog"  = 1,
                    "cauchit" = 1)     # long tails, scale fixed to 1
resid_sd

cat("\nMEANINGFULNESS MODEL:\n")
print(clustered_results_meaningful)
summary(model_meaningful)
df.residual(model_meaningful)
r2(model_meaningful)
link_name <- model_meaningful$link
resid_sd  <- switch(link_name,
                    "logit"  = pi / sqrt(3),
                    "probit" = 1,
                    "cloglog" = 1,
                    "loglog"  = 1,
                    "cauchit" = 1)     # long tails, scale fixed to 1
resid_sd

# Fit the three models with BiasedCat using mixed effects
# Model 1: Real Performance (Linear Mixed Effects)
model_real <- lmer(PostPerformance ~ BiasedCat + as.factor(NID) + PrePerformance + (1|UID) +
                     UIdeo + UStanceLabel + AICorrectness, 
                   data = complete_data)

# Model 2: Perceived Improvement (Bayesian ordinal mixed models)
complete_data$PerceivedImproveCode <- factor(complete_data$PerceivedImproveCode, ordered = TRUE)
model_perceived <- MCMCglmm(PerceivedImproveCode ~ BiasedCat + as.factor(NID) + PrePerformance + 
                              UIdeo + UStanceLabel + AICorrectness,
                            random = ~UID,
                            family = "ordinal",
                            nitt = 25000, thin = 10, burnin = 5000,
                            data = complete_data[complete.cases(complete_data[ , "AICorrectness"]), ])

# Model 3: Meaningfulness (Bayesian ordinal mixed models)
complete_data$AIInterMean_numeric <- factor(complete_data$AIInterMean_numeric, ordered = TRUE)
model_meaningful <- MCMCglmm(AIInterMean_numeric ~ BiasedCat + as.factor(NID) + PrePerformance +
                               UIdeo + UStanceLabel + AICorrectness,
                             random = ~UID,
                             family = "ordinal",
                             nitt = 25000, thin = 10, burnin = 5000,
                             data = complete_data[complete.cases(complete_data[ , "AICorrectness"]), ])

# Display results
cat("=== MIXED EFFECTS MODEL RESULTS ===\n")

cat("ACTUAL PERFORMANCE MODEL (Linear Mixed Effects):\n")
print(summary(model_real))
summary(model_real)$sigma
df.residual(model_real)
r2(model_real)

cat("PERCEIVED IMPROVEMENT MODEL (Cumulative Link Mixed Model):\n")
print(summary(model_perceived))
se_fixed <- apply(model_perceived$Sol, 2, sd)
se_fixed
sqrt(model_perceived$VCV[ , "units"][1])
X  <- model_perceived$X                 # design matrix for fixed effects
B  <- as.matrix(model_perceived$Sol)            # iterations × coefficients
eta <- X %*% t(B)
VF  <- apply(eta, 2, var)          # fixed-effect variance per draw
VR  <- model_perceived$VCV[ , "UID"]      # random-effect variance per draw
VE <- 1                                  # residual var. (probit link)
R2_marginal    <- VF / (VF + VR + VE)               # fixed only
R2_conditional <- (VF + VR) / (VF + VR + VE)        # fixed + random
cbind(
  Marginal    = c(mean(R2_marginal),
                  quantile(R2_marginal, c(.025, .5, .975))),
  Conditional = c(mean(R2_conditional),
                  quantile(R2_conditional, c(.025, .5, .975)))
)

cat("MEANINGFULNESS MODEL (Cumulative Link Mixed Model):\n")
print(summary(model_meaningful))
se_fixed <- apply(model_meaningful$Sol, 2, sd)
se_fixed
sqrt(model_meaningful$VCV[ , "units"][1])
X  <- model_meaningful$X                 # design matrix for fixed effects
B  <- as.matrix(model_meaningful$Sol)            # iterations × coefficients
eta <- X %*% t(B)
VF  <- apply(eta, 2, var)          # fixed-effect variance per draw
VR  <- model_meaningful$VCV[ , "UID"]      # random-effect variance per draw
VE <- 1                                  # residual var. (probit link)
R2_marginal    <- VF / (VF + VR + VE)               # fixed only
R2_conditional <- (VF + VR) / (VF + VR + VE)        # fixed + random
cbind(
  Marginal    = c(mean(R2_marginal),
                  quantile(R2_marginal, c(.025, .5, .975))),
  Conditional = c(mean(R2_conditional),
                  quantile(R2_conditional, c(.025, .5, .975)))
)

# ======================================================
# Marginal means for BiasedCat and pairwise comparison
# ======================================================
# Function for marginal means, comparisons, and Hedge's g
get_model_results_with_hedges_g <- function(model, model_name, data = complete_data) {
  
  # Helper function for MCMCglmm posterior calculations
  calc_mcmc_means <- function(posterior_samples, data) {
    intercept <- posterior_samples[, "(Intercept)"]
    preperf_effect <- if("PrePerformance" %in% colnames(posterior_samples)) {
      posterior_samples[, "PrePerformance"] * mean(data$PrePerformance, na.rm = TRUE)
    } else { 0 }
    
    # Calculate means for each bias level
    means <- list()
    means[["No Bias"]] <- intercept + preperf_effect
    
    for (level in c("Moderate Bias", "Strong Bias")) {
      coef_name <- paste0("BiasedCat", level)
      if (coef_name %in% colnames(posterior_samples)) {
        means[[level]] <- intercept + posterior_samples[, coef_name] + preperf_effect
      } else {
        means[[level]] <- means[["No Bias"]]  # If coefficient doesn't exist
      }
    }
    return(means)
  }
  
  # Hedge's g correction factor
  hedges_correction <- function(n1, n2) {
    df <- n1 + n2 - 2
    if (df > 0) {
      return(1 - (3 / (4 * df - 1)))
    } else {
      return(1)  # Fallback if df calculation fails
    }
  }
  
  # Calculate sample sizes for each BiasedCat level
  n_by_group <- table(data$BiasedCat)
  
  # Calculate marginal means and comparisons based on model type
  if (class(model)[1] == "MCMCglmm") {
    # MCMCglmm approach
    posterior_samples <- model$Sol
    means <- calc_mcmc_means(posterior_samples, data)
    
    # Use appropriate pooled SD for ordinal models
    if (model$family[1] == "ordinal") {
      # For ordinal models, use empirical SD from observed data
      response_var <- all.vars(model$Fixed$formula)[1]
      observed_values <- as.numeric(data[[response_var]])
      pooled_sd <- sd(observed_values, na.rm = TRUE)
      pooled_sd_samples <- rep(pooled_sd, nrow(posterior_samples))
      cat("Using empirical pooled SD for ordinal model:", round(pooled_sd, 4), "\n")
    } else {
      # For continuous models, use residual variance
      residual_var_samples <- model$VCV[, "units"]
      pooled_sd_samples <- sqrt(residual_var_samples)
      pooled_sd <- mean(pooled_sd_samples)
      cat("Using model residual SD for continuous model:", round(pooled_sd, 4), "\n")
    }
    
    mean_pooled_sd <- mean(pooled_sd_samples)
    
    # Marginal means
    marginal_means <- data.frame(
      BiasedCat = names(means),
      Mean = sapply(means, mean),
      Lower_CI = sapply(means, function(x) quantile(x, 0.025)),
      Upper_CI = sapply(means, function(x) quantile(x, 0.975)),
      row.names = NULL
    )
    
    # Pairwise comparisons with Hedge's g
    comparisons <- data.frame(
      Contrast = character(0),
      Estimate = numeric(0),
      Lower_CI = numeric(0),
      Upper_CI = numeric(0),
      Prob_Greater_Zero = numeric(0),
      Hedges_g = numeric(0),
      Hedges_g_Lower = numeric(0),
      Hedges_g_Upper = numeric(0)
    )
    
    # All pairwise differences
    levels <- names(means)
    for (i in 1:(length(levels)-1)) {
      for (j in (i+1):length(levels)) {
        diff_samples <- means[[j]] - means[[i]]
        
        # Calculate Hedge's g for each posterior sample
        hedges_g_samples <- diff_samples / pooled_sd_samples
        
        # Apply Hedge's correction
        n1 <- n_by_group[levels[i]]
        n2 <- n_by_group[levels[j]]
        correction <- hedges_correction(n1, n2)
        hedges_g_samples <- hedges_g_samples * correction
        
        comparisons <- rbind(comparisons, data.frame(
          Contrast = paste(levels[j], "-", levels[i]),
          Estimate = mean(diff_samples),
          Lower_CI = quantile(diff_samples, 0.025),
          Upper_CI = quantile(diff_samples, 0.975),
          Prob_Greater_Zero = mean(diff_samples > 0),
          Hedges_g = mean(hedges_g_samples),
          Hedges_g_Lower = quantile(hedges_g_samples, 0.025),
          Hedges_g_Upper = quantile(hedges_g_samples, 0.975)
        ))
      }
    }
    
    # Apply FDR correction instead of Bonferroni
    n_comparisons <- nrow(comparisons)
    # For Bayesian, convert "probability of difference > 0" to two-tailed p-value equivalent
    p_values_equiv <- 2 * pmin(comparisons$Prob_Greater_Zero, 1 - comparisons$Prob_Greater_Zero)
    
    # Apply Benjamini-Hochberg FDR correction
    comparisons$FDR_adjusted_p <- p.adjust(p_values_equiv, method = "fdr")
    
  } else {
    # Standard emmeans approach for lmer/clmm models
    emm <- emmeans(model, ~ BiasedCat)
    pairs_result <- pairs(emm, adjust = "fdr")  # Changed from "bonferroni" to "fdr"
    
    # Extract marginal means
    marginal_means <- as.data.frame(emm)
    
    # Extract pairwise comparisons  
    comparisons <- as.data.frame(pairs_result)
    
    # Calculate Hedge's g for standard models
    if (class(model)[1] == "lmerMod") {
      # For lmer models, get residual standard deviation
      pooled_sd <- sigma(model)  # Residual standard deviation
      
      # Add Hedge's g to comparisons
      comparisons$Hedges_g <- numeric(nrow(comparisons))
      comparisons$Hedges_g_Lower <- numeric(nrow(comparisons))
      comparisons$Hedges_g_Upper <- numeric(nrow(comparisons))
      
      for (i in 1:nrow(comparisons)) {
        # Extract group names from contrast
        contrast_parts <- strsplit(as.character(comparisons$contrast[i]), " - ")[[1]]
        group1 <- trimws(contrast_parts[1])
        group2 <- trimws(contrast_parts[2])
        
        # Get sample sizes
        n1 <- n_by_group[group1]
        n2 <- n_by_group[group2]
        
        # Calculate Hedge's g
        cohens_d <- comparisons$estimate[i] / pooled_sd
        correction <- hedges_correction(n1, n2)
        hedges_g <- cohens_d * correction
        
        # Calculate confidence interval for Hedge's g
        se_hedges_g <- sqrt((n1 + n2)/(n1 * n2) + hedges_g^2/(2 * (n1 + n2 - 2)))
        
        comparisons$Hedges_g[i] <- hedges_g
        comparisons$Hedges_g_Lower[i] <- hedges_g - 1.96 * se_hedges_g
        comparisons$Hedges_g_Upper[i] <- hedges_g + 1.96 * se_hedges_g
      }
    } else {
      # For other model types, indicate that Hedge's g calculation is not implemented
      comparisons$Hedges_g <- NA
      comparisons$Hedges_g_Lower <- NA
      comparisons$Hedges_g_Upper <- NA
    }
  }
  
  return(list(
    model_name = model_name,
    marginal_means = marginal_means,
    comparisons = comparisons,
    pooled_sd = if(exists("mean_pooled_sd")) mean_pooled_sd else if(exists("pooled_sd")) pooled_sd else NA
  ))
}

# Apply to all models and store results with Hedge's g
results_with_hedges <- list()

# Process each model
models <- list(
  "real" = list(model = model_real, name = "Actual Performance"),
  "perceived" = list(model = model_perceived, name = "Perceived Improvement"), 
  "meaningful" = list(model = model_meaningful, name = "Meaningfulness")
)

for (model_key in names(models)) {
  if (exists(paste0("model_", model_key))) {
    model_obj <- get(paste0("model_", model_key))
    results_with_hedges[[model_key]] <- get_model_results_with_hedges_g(model_obj, models[[model_key]]$name)
  }
}

# Display results with Hedge's g
cat("=== MARGINAL MEANS BY BiasedCat ===\n")
for (model_key in names(results_with_hedges)) {
  cat("\n", toupper(results_with_hedges[[model_key]]$model_name), ":\n")
  print(results_with_hedges[[model_key]]$marginal_means)
}

cat("\n=== PAIRWISE COMPARISONS WITH HEDGE'S G EFFECT SIZES (FDR corrected) ===\n")
for (model_key in names(results_with_hedges)) {
  cat("\n", toupper(results_with_hedges[[model_key]]$model_name), ":\n")
  result_df <- results_with_hedges[[model_key]]$comparisons
  
  # Format for better display
  if ("Hedges_g" %in% colnames(result_df)) {
    # Round Hedge's g values for cleaner display
    result_df$Hedges_g <- round(result_df$Hedges_g, 3)
    result_df$Hedges_g_Lower <- round(result_df$Hedges_g_Lower, 3)
    result_df$Hedges_g_Upper <- round(result_df$Hedges_g_Upper, 3)
  }
  
  print(result_df)
  
  # Show pooled SD used for calculation
  if (!is.na(results_with_hedges[[model_key]]$pooled_sd)) {
    cat("  Pooled SD used:", round(results_with_hedges[[model_key]]$pooled_sd, 3), "\n")
  }
}

model_sigma <- sigma(model_real)
model_df <- df.residual(model_real)

marginal_means <- emmeans(model_real, ~ BiasedCat)
marginal_comparison <- pairs(marginal_means, adjust = "fdr")

marginal_contrast_summary <- summary(marginal_comparison, infer = TRUE)
marginal_estimate <- marginal_contrast_summary$estimate
marginal_se <- marginal_contrast_summary$SE
marginal_df <- marginal_contrast_summary$df

# Calculate Hedges' g manually
marginal_cohens_d <- marginal_estimate / model_sigma
marginal_hedges_g <- marginal_cohens_d * J
marginal_hedges_se <- (marginal_se / model_sigma) * J

# =================================
# Prepare data for visualization
# =================================
# Function to prepare visualization data from either emmeans or MCMCglmm results
prepare_viz_data <- function(marginal_means_df, model_name) {
  
  # Check if this is emmeans format (has emmean, SE, df columns) or MCMCglmm format (has Mean, Lower_CI, Upper_CI)
  is_emmeans_format <- all(c("emmean", "SE", "df") %in% colnames(marginal_means_df))
  is_mcmc_format <- all(c("Mean", "Lower_CI", "Upper_CI") %in% colnames(marginal_means_df))
  
  cat("Preparing viz data for", model_name, "\n")
  cat("Format detected:", ifelse(is_emmeans_format, "emmeans", ifelse(is_mcmc_format, "MCMCglmm", "unknown")), "\n")
  
  if (is_emmeans_format) {
    # Standard emmeans format
    viz_df <- marginal_means_df %>%
      mutate(
        bias_level = case_when(
          BiasedCat == "No Bias" ~ 0,
          BiasedCat == "Moderate Bias" ~ 1,
          BiasedCat == "Strong Bias" ~ 2,
          TRUE ~ NA_real_
        ),
        bias_label = factor(BiasedCat, levels = c("No Bias", "Moderate Bias", "Strong Bias")),
        # Use existing emmean and CI columns
        point_estimate = emmean,
        ci_lower = lower.CL,
        ci_upper = upper.CL
      )
    
    # Calculate multiple confidence intervals using SE and df
    if ("df" %in% colnames(marginal_means_df) && !any(is.na(marginal_means_df$df))) {
      # Use the first df value (should be same for all rows in balanced design)
      df_value <- marginal_means_df$df[1]
      t_90 <- qt(0.95, df_value)   # 90% CI
      t_95 <- qt(0.975, df_value)  # 95% CI  
      t_99 <- qt(0.995, df_value)  # 99% CI
      
      viz_df <- viz_df %>%
        mutate(
          # 90% CI
          lower.CL_90 = emmean - SE * t_90,
          upper.CL_90 = emmean + SE * t_90,
          # 95% CI
          lower.CL_95 = emmean - SE * t_95,
          upper.CL_95 = emmean + SE * t_95,
          # 99% CI
          lower.CL_99 = emmean - SE * t_99,
          upper.CL_99 = emmean + SE * t_99
        )
    } else {
      # Fallback: assume existing CI is 95% and approximate others
      cat("Warning: No df available, using approximation for multiple CIs\n")
      ci_width <- (marginal_means_df$upper.CL - marginal_means_df$lower.CL) / 2
      se_approx <- ci_width / 1.96  # Approximate SE
      
      viz_df <- viz_df %>%
        mutate(
          # 90% CI
          lower.CL_90 = point_estimate - se_approx * 1.645,
          upper.CL_90 = point_estimate + se_approx * 1.645,
          # 95% CI (use existing)
          lower.CL_95 = ci_lower,
          upper.CL_95 = ci_upper,
          # 99% CI  
          lower.CL_99 = point_estimate - se_approx * 2.576,
          upper.CL_99 = point_estimate + se_approx * 2.576
        )
    }
    
  } else if (is_mcmc_format) {
    # MCMCglmm format - use credible intervals
    viz_df <- marginal_means_df %>%
      mutate(
        bias_level = case_when(
          BiasedCat == "No Bias" ~ 0,
          BiasedCat == "Moderate Bias" ~ 1,
          BiasedCat == "Strong Bias" ~ 2,
          TRUE ~ NA_real_
        ),
        bias_label = factor(BiasedCat, levels = c("No Bias", "Moderate Bias", "Strong Bias")),
        # Use MCMCglmm posterior means and credible intervals
        point_estimate = Mean,
        emmean = Mean,  # For compatibility
        ci_lower = Lower_CI,
        ci_upper = Upper_CI,
        # Assume existing CIs are 95% credible intervals
        lower.CL_95 = Lower_CI,
        upper.CL_95 = Upper_CI
      )
    
    # For MCMCglmm, we'd need the full posterior samples to calculate different CI levels
    # As approximation, scale the 95% CI 
    ci_width_95 <- (viz_df$Upper_CI - viz_df$Lower_CI) / 2
    
    viz_df <- viz_df %>%
      mutate(
        # 90% CI (narrower)
        lower.CL_90 = Mean - ci_width_95 * (1.645/1.96),
        upper.CL_90 = Mean + ci_width_95 * (1.645/1.96),
        # 95% CI (use existing)
        lower.CL_95 = Lower_CI,
        upper.CL_95 = Upper_CI,
        # 99% CI (wider)
        lower.CL_99 = Mean - ci_width_95 * (2.576/1.96),
        upper.CL_99 = Mean + ci_width_95 * (2.576/1.96)
      )
    
    cat("Note: CIs for MCMCglmm are approximated from 95% credible intervals\n")
    
  } else {
    stop("Unknown marginal means format - expected either emmeans or MCMCglmm format")
  }
  
  return(viz_df)
}

# Apply to all models using the new results structure
if (exists("results_with_hedges")) {
  
  # Real Performance
  if ("real" %in% names(results_with_hedges)) {
    bias_real_df <- prepare_viz_data(results_with_hedges$real$marginal_means, "Actual Performance")
    bias_real_multi <- bias_real_df  # Already has multiple CIs
  }
  
  # Perceived Improvement  
  if ("perceived" %in% names(results_with_hedges)) {
    bias_perceived_df <- prepare_viz_data(results_with_hedges$perceived$marginal_means, "Perceived Improvement")
    bias_perceived_multi <- bias_perceived_df  # Already has multiple CIs
  }
  
  # Meaningfulness
  if ("meaningful" %in% names(results_with_hedges)) {
    bias_meaningful_df <- prepare_viz_data(results_with_hedges$meaningful$marginal_means, "Meaningfulness") 
    bias_meaningful_multi <- bias_meaningful_df  # Already has multiple CIs
  }
  
} else {
  cat("Warning: results_with_hedges not found, trying individual emm objects...\n")
  
  # Fallback to individual emmeans objects if they exist
  if (exists("emm_real")) {
    bias_real_df <- prepare_viz_data(as.data.frame(emm_real), "Actual Performance")
    bias_real_multi <- bias_real_df
  }
  
  if (exists("emm_perceived")) {
    bias_perceived_df <- prepare_viz_data(as.data.frame(emm_perceived), "Perceived Improvement")
    bias_perceived_multi <- bias_perceived_df
  }
  
  if (exists("emm_meaningful")) {
    bias_meaningful_df <- prepare_viz_data(as.data.frame(emm_meaningful), "Meaningfulness")
    bias_meaningful_multi <- bias_meaningful_df
  }
}

# Display summary of prepared data
cat("\n=== VISUALIZATION DATA SUMMARY ===\n")

if (exists("bias_real_multi")) {
  cat("Actual Performance data:\n")
  print(bias_real_multi[, c("bias_label", "point_estimate", "lower.CL_95", "upper.CL_95")])
}

if (exists("bias_perceived_multi")) {
  cat("\nPerceived Improvement data:\n") 
  print(bias_perceived_multi[, c("bias_label", "point_estimate", "lower.CL_95", "upper.CL_95")])
}

if (exists("bias_meaningful_multi")) {
  cat("\nMeaningfulness data:\n")
  print(bias_meaningful_multi[, c("bias_label", "point_estimate", "lower.CL_95", "upper.CL_95")])
}

# Quick verification function
verify_viz_data <- function(df, name) {
  required_cols <- c("bias_level", "bias_label", "point_estimate", "lower.CL_90", "upper.CL_90", 
                     "lower.CL_95", "upper.CL_95", "lower.CL_99", "upper.CL_99")
  
  missing_cols <- setdiff(required_cols, colnames(df))
  if (length(missing_cols) > 0) {
    cat("Warning:", name, "missing columns:", paste(missing_cols, collapse = ", "), "\n")
  } else {
    cat(name, "data ready for visualization ✓\n")
  }
}

# Verify all datasets
if (exists("bias_real_multi")) verify_viz_data(bias_real_multi, "Actual Performance")
if (exists("bias_perceived_multi")) verify_viz_data(bias_perceived_multi, "Perceived Improvement") 
if (exists("bias_meaningful_multi")) verify_viz_data(bias_meaningful_multi, "Meaningfulness")

cat("\nVisualization data preparation complete!\n")


# ==================================
# Visualization setup
# ==================================
# Define colors for all three outcomes
color_actual <- "#006400"      # Dark green
color_actual_90 <- "#228B22"   # Forest green
color_actual_95 <- "#66C266"   # Light forest green
color_actual_99 <- "#BBE5BB"   # Soft mint green

color_perceived <- "#4B0082"    # Indigo
color_perceived_90 <- "#9370DB" # Medium orchid
color_perceived_95 <- "#B88BD8" # Lavender
color_perceived_99 <- "#D6C0E5" # Soft lilac

color_meaningful <- "#D2691E"    # Chocolate (dark orange-brown)
color_meaningful_90 <- "#FF8C00" # Dark orange
color_meaningful_95 <- "#FFA500" # Orange  
color_meaningful_99 <- "#FFB347" # Peach (light but visible orange)

# Define colors for each AI type (keep the original colors for bias categories)
bias_cat_colors <- c("No Bias" = "#4E79A7", "Moderate Bias" = "#F28E2B", "Strong Bias" = "#E15759")

# Define box widths for different confidence levels
box_width_base <- 0.1
box_width_90 <- box_width_base * 1.4   # Narrowest (90% CI)
box_width_95 <- box_width_base * 1.0   # Medium (95% CI)
box_width_99 <- box_width_base * 0.7   # Widest (99% CI)

# Add box coordinates for all confidence levels (for all three datasets)
add_box_coordinates <- function(df) {
  df %>%
    mutate(
      # 90% CI boxes
      xmin_90 = bias_level - box_width_90,
      xmax_90 = bias_level + box_width_90,
      # 95% CI boxes
      xmin_95 = bias_level - box_width_95,
      xmax_95 = bias_level + box_width_95,
      # 99% CI boxes
      xmin_99 = bias_level - box_width_99,
      xmax_99 = bias_level + box_width_99
    )
}

bias_real_multi <- add_box_coordinates(bias_real_multi)
bias_perceived_multi <- add_box_coordinates(bias_perceived_multi)
bias_meaningful_multi <- add_box_coordinates(bias_meaningful_multi)

# ==================================
# Create plot
# ==================================
# Top plot - Actual Performance
p_actual <- ggplot() +
  # 99% CI boxes (widest, lightest color) - BACK LAYER
  geom_rect(data = bias_real_multi,
            aes(xmin = xmin_99, xmax = xmax_99, ymin = lower.CL_99, ymax = upper.CL_99),
            fill = color_actual_99, alpha = 1, color = color_actual_99, linewidth = 0.) +
  
  # 95% CI boxes (medium width, medium color) - MIDDLE LAYER
  geom_rect(data = bias_real_multi,
            aes(xmin = xmin_95, xmax = xmax_95, ymin = lower.CL_95, ymax = upper.CL_95),
            fill = color_actual_95, alpha = 1, color = color_actual_95, linewidth = 0.) +
  
  # 90% CI boxes (narrowest, darkest color) - FRONT LAYER
  geom_rect(data = bias_real_multi,
            aes(xmin = xmin_90, xmax = xmax_90, ymin = lower.CL_90, ymax = upper.CL_90),
            fill = color_actual_90, alpha = 1, color = color_actual_90, linewidth = 0.) +
  
  # Lines and points
  geom_line(data = bias_real_multi,
            aes(x = bias_level, y = emmean),
            color = color_actual, linewidth = 1.2) +
  geom_point(data = bias_real_multi, 
             aes(x = bias_level, y = emmean), 
             color = color_actual, size = 3.5, shape = 20, stroke = 1.) +
  
  scale_y_continuous(
    name = "Post-Interaction Performance",
    labels = scales::number_format(accuracy = 0.01),
    limits = c(0.57, 0.7)
  ) +
  
  scale_x_continuous(
    breaks = c(0, 1, 2),
    limits = c(-0.35, 2.35),
    labels = c("No Bias", "Moderate Bias", "Strong Bias"),
    name = "AI Bias Magnitude"
  ) +
  
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.line = element_blank(),
    text = element_text(family = "Avenir", color = "black"),
    axis.title.x = element_text(family = "Avenir", size = 9, margin = margin(t = 8)),
    axis.title.y = element_text(family = "Avenir", size = 9, color = "black", margin = margin(r = 8)),
    axis.text.x = element_text(family = "Avenir", size = 8, color = "black"),
    axis.text.y = element_text(family = "Avenir", size = 8, color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 0.4),
    axis.ticks.length = unit(2.5, "pt"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    plot.margin = margin(t = 10, r = 15, b = 5, l = 10)
  )

# Middle plot - Perceived Improvement
p_perceived <- ggplot() +
  # 99% CI boxes
  geom_rect(data = bias_perceived_multi,
            aes(xmin = xmin_99, xmax = xmax_99, ymin = lower.CL_99, ymax = upper.CL_99),
            fill = color_perceived_99, alpha = 1, color = color_perceived_99, linewidth = 0.) +
  
  # 95% CI boxes
  geom_rect(data = bias_perceived_multi,
            aes(xmin = xmin_95, xmax = xmax_95, ymin = lower.CL_95, ymax = upper.CL_95),
            fill = color_perceived_95, alpha = 1, color = color_perceived_95, linewidth = 0.) +
  
  # 90% CI boxes
  geom_rect(data = bias_perceived_multi,
            aes(xmin = xmin_90, xmax = xmax_90, ymin = lower.CL_90, ymax = upper.CL_90),
            fill = color_perceived_90, alpha = 1, color = color_perceived_90, linewidth = 0.) +
  
  # Lines and points
  geom_line(data = bias_perceived_multi,
            aes(x = bias_level, y = emmean),
            color = color_perceived, linewidth = 1.2) +
  geom_point(data = bias_perceived_multi, 
             aes(x = bias_level, y = emmean), 
             color = color_perceived, size = 3.5, shape = 18, stroke = 1.) +
  
  scale_y_continuous(
    name = "Perceived Improvement",
    labels = scales::number_format(accuracy = 0.01),
    limits = c(1.5, 3.5)
  ) +
  
  scale_x_continuous(
    breaks = c(0, 1, 2),
    labels = c("No Bias", "Moderate Bias", "Strong Bias"),
    name = "AI Bias Magnitude",
    limits = c(-0.35, 2.35)
  ) +
  
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.line = element_blank(),
    text = element_text(family = "Avenir", color = "black"),
    axis.title.x = element_text(family = "Avenir", size = 9, margin = margin(t = 8)),
    axis.title.y = element_text(family = "Avenir", size = 9, color = "black", margin = margin(r = 8)),
    axis.text.x = element_text(family = "Avenir", size = 8, color = "black"),
    axis.text.y = element_text(family = "Avenir", size = 8, color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 0.4),
    axis.ticks.length = unit(2.5, "pt"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    plot.margin = margin(t = 15, r = 15, b = 10, l = 10)
  )

# Bottom plot - Meaningfulness
p_meaningful <- ggplot() +
  # 99% CI boxes
  geom_rect(data = bias_meaningful_multi,
            aes(xmin = xmin_99, xmax = xmax_99, ymin = lower.CL_99, ymax = upper.CL_99),
            fill = color_meaningful_99, alpha = 1, color = color_meaningful_99, linewidth = 0.) +
  
  # 95% CI boxes
  geom_rect(data = bias_meaningful_multi,
            aes(xmin = xmin_95, xmax = xmax_95, ymin = lower.CL_95, ymax = upper.CL_95),
            fill = color_meaningful_95, alpha = 1, color = color_meaningful_95, linewidth = 0.) +
  
  # 90% CI boxes
  geom_rect(data = bias_meaningful_multi,
            aes(xmin = xmin_90, xmax = xmax_90, ymin = lower.CL_90, ymax = upper.CL_90),
            fill = color_meaningful_90, alpha = 1, color = color_meaningful_90, linewidth = 0.) +
  
  # Lines and points
  geom_line(data = bias_meaningful_multi,
            aes(x = bias_level, y = emmean),
            color = color_meaningful, linewidth = 1.2) +
  geom_point(data = bias_meaningful_multi, 
             aes(x = bias_level, y = emmean), 
             color = color_meaningful, size = 2.5, shape = 17, stroke = 1.) +
  
  scale_y_continuous(
    name = "Interaction Meaningfulness",
    labels = scales::number_format(accuracy = 0.01),
    limits = c(1.5, 3.2)
  ) +
  
  scale_x_continuous(
    name = "AI Bias Magnitude",
    breaks = c(0, 1, 2),
    labels = c("No Bias", "Moderate Bias", "Strong Bias"),
    limits = c(-0.35, 2.35)
  ) +
  
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.line = element_blank(),
    text = element_text(family = "Avenir", color = "black"),
    axis.title.x = element_text(family = "Avenir", size = 9, margin = margin(t = 8)),
    axis.title.y = element_text(family = "Avenir", size = 9, color = "black", margin = margin(r = 8)),
    axis.text.x = element_text(family = "Avenir", size = 8, color = "black"),
    axis.text.y = element_text(family = "Avenir", size = 8, color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 0.4),
    axis.ticks.length = unit(2.5, "pt"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    plot.margin = margin(t = 15, r = 15, b = 10, l = 10)
  )

# Combine all three plots vertically
p_combined <- p_actual / p_perceived / p_meaningful

# Display the combined plot
print(p_combined)
