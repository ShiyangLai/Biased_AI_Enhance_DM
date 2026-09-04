set.seed(123) 

single_ai_processed_ <- single_ai_processed_ %>% 
  mutate(WillRecommendAI_Numeric = factor(WillRecommendAI, 
                                      levels = c("Very unlikely",
                                                 "Unlikely", 
                                                 "Neutral",
                                                 "Likely",
                                                 "Very likely"),
                                      labels = 1:5) %>%
           as.numeric()) 


single_ai_processed$BiasedType2 <- ifelse(single_ai_processed$AIStanceLabel_S == "Default", "Non-Biased",
                                          ifelse(single_ai_processed$AIStanceLabel_S == "Neutral", "Neutralized",
                                                 "Biased"))

single_ai_processed_$WillRecommendAI_Numeric <- factor(single_ai_processed_$WillRecommendAI_Numeric, ordered = TRUE)

rec_model <- clm(
  WillRecommendAI_Numeric ~ BiasedCat + as.factor(NID) + PrePerformance + UIdeo + UStanceLabel + AICorrectness,
  data = single_ai_processed_, na.action = na.omit
)

vcov_rec<- vcovCL(rec_model, cluster = single_ai_processed_$UID)
clustered_results_rec <- coeftest(rec_model, vcov = vcov_rec)

print(clustered_results_rec)
summary(rec_model)
df.residual(rec_model)
performance::r2(rec_model)
link_name <- rec_model$link
resid_sd  <- switch(link_name,
                    "logit"  = pi / sqrt(3),
                    "probit" = 1,
                    "cloglog" = 1,
                    "loglog"  = 1,
                    "cauchit" = 1)     # long tails, scale fixed to 1
resid_sd

# MCMCglmm hard-errors on NA in ANY fixed predictor (not just AICorrectness --
# UIdeo has NA rows once blank strings are converted in preprcessing.R), so
# filter complete cases over the full model column set.
mcmc_cols <- c("WillRecommendAI_Numeric", "BiasedCat", "NID", "PrePerformance",
               "UIdeo", "UStanceLabel", "AICorrectness", "UID")
rec_data <- single_ai_processed_[complete.cases(single_ai_processed_[, mcmc_cols]), ]

rec_model <- MCMCglmm(WillRecommendAI_Numeric ~ BiasedCat + as.factor(NID) +
                        PrePerformance + UIdeo + UStanceLabel + AICorrectness,
                      random = ~UID,
                      family = "ordinal",
                      nitt = 25000, thin = 10, burnin = 5000,
                      data = rec_data)

print(summary(rec_model))
se_fixed <- apply(rec_model$Sol, 2, sd)
se_fixed
sqrt(rec_model$VCV[ , "units"][1])
X  <- rec_model$X                 # design matrix for fixed effects
B  <- as.matrix(rec_model$Sol)            # iterations × coefficients
eta <- X %*% t(B)
VF  <- apply(eta, 2, var)          # fixed-effect variance per draw
VR  <- rec_model$VCV[ , "UID"]      # random-effect variance per draw
VE <- 1                                  # residual var. (probit link)
R2_marginal    <- VF / (VF + VR + VE)               # fixed only
R2_conditional <- (VF + VR) / (VF + VR + VE)        # fixed + random
cbind(
  Marginal    = c(mean(R2_marginal),
                  quantile(R2_marginal, c(.025, .5, .975))),
  Conditional = c(mean(R2_conditional),
                  quantile(R2_conditional, c(.025, .5, .975)))
)


# Get reference values more safely
if(is.factor(single_ai_processed_$NID)) {
  ref_nid <- levels(single_ai_processed_$NID)[1]
} else {
  ref_nid <- unique(single_ai_processed_$NID)[1]
}

ref_performance <- mean(single_ai_processed_$PrePerformance, na.rm = TRUE)

print(paste("Reference NID:", ref_nid))
print(paste("Reference PrePerformance:", ref_performance))

# Create reference grid using actual unique values
bias_cats <- c("No Bias", "Moderate Bias", "Strong Bias")

# Check if these categories exist in your data
actual_bias_cats <- unique(single_ai_processed_$BiasedCat)
print("Actual bias categories in data:")
print(actual_bias_cats)

# Use actual categories from your data
ref_grid <- data.frame(
  BiasedCat = actual_bias_cats,
  NID = rep(ref_nid, length(actual_bias_cats)),
  PrePerformance = rep(ref_performance, length(actual_bias_cats))
)

# Convert to factors if needed
if(is.factor(single_ai_processed_$BiasedCat)) {
  ref_grid$BiasedCat <- factor(ref_grid$BiasedCat, levels = levels(single_ai_processed_$BiasedCat))
}
if(is.factor(single_ai_processed_$NID)) {
  ref_grid$NID <- factor(ref_grid$NID, levels = levels(single_ai_processed_$NID))
}

print("Reference grid:")
print(ref_grid)

# Alternative approach: Create a proper reference dataset
print("Original data structure:")
print("BiasedCat levels:")
print(table(single_ai_processed_$BiasedCat))
print("NID structure:")
print(class(single_ai_processed_$NID))
if(is.factor(single_ai_processed_$NID)) {
  print("NID levels:")
  print(levels(single_ai_processed_$NID))
  print("NID table:")
  print(table(single_ai_processed_$NID))
} else {
  print("NID unique values:")
  print(table(single_ai_processed_$NID))
}

# Get reference values
ref_performance <- mean(single_ai_processed_$PrePerformance, na.rm = TRUE)

# Handle NID properly based on its type
if(is.factor(single_ai_processed_$NID)) {
  # If NID is a factor, use the first level or most common level
  nid_table <- table(single_ai_processed_$NID)
  most_common_nid <- names(nid_table)[which.max(nid_table)]
  print(paste("Most common NID (factor):", most_common_nid))
} else {
  # If NID is not a factor, convert to factor first
  single_ai_processed_$NID <- as.factor(single_ai_processed_$NID)
  nid_table <- table(single_ai_processed_$NID)
  most_common_nid <- names(nid_table)[which.max(nid_table)]
  print(paste("Most common NID (converted to factor):", most_common_nid))
}

print(paste("Reference PrePerformance:", ref_performance))

# Create a reference dataset with one row per BiasedCat level
bias_levels <- unique(single_ai_processed_$BiasedCat)

print("Bias levels found:")
print(bias_levels)

# Create reference dataset maintaining all factor levels
# The fitted model also carries UIdeo, UStanceLabel and AICorrectness. They
# were previously omitted here, so X_ref came out narrower than the posterior
# and the matrix product below failed. Hold them at reference values (first
# factor level, covariate mean), as in second_figure_b1.R: these terms are
# common to every arm and cancel in the between-arm contrasts.
ref_data <- data.frame(
  BiasedCat = bias_levels,
  NID = rep(most_common_nid, length(bias_levels)),
  PrePerformance = rep(ref_performance, length(bias_levels)),
  UIdeo = rep(levels(factor(rec_data$UIdeo))[1], length(bias_levels)),
  UStanceLabel = rep(levels(factor(rec_data$UStanceLabel))[1], length(bias_levels)),
  AICorrectness = rep(mean(rec_data$AICorrectness, na.rm = TRUE), length(bias_levels))
)
ref_data$UIdeo        <- factor(ref_data$UIdeo,        levels = levels(factor(rec_data$UIdeo)))
ref_data$UStanceLabel <- factor(ref_data$UStanceLabel, levels = levels(factor(rec_data$UStanceLabel)))

# Ensure factor levels match original data
if(is.factor(single_ai_processed_$BiasedCat)) {
  ref_data$BiasedCat <- factor(ref_data$BiasedCat, levels = levels(single_ai_processed_$BiasedCat))
} else {
  # If BiasedCat isn't a factor, make it one
  single_ai_processed_$BiasedCat <- as.factor(single_ai_processed_$BiasedCat)
  ref_data$BiasedCat <- factor(ref_data$BiasedCat, levels = levels(single_ai_processed_$BiasedCat))
}

ref_data$NID <- factor(ref_data$NID, levels = levels(single_ai_processed_$NID))

print("Reference dataset:")
print(ref_data)
print("BiasedCat levels in reference data:")
print(levels(ref_data$BiasedCat))
print("NID levels in reference data:")
print(levels(ref_data$NID))

# Check for any NA values
print("Any NAs in reference data:")
print(sapply(ref_data, function(x) any(is.na(x))))

# Create model matrix for all bias levels at once
X_ref <- model.matrix(~ BiasedCat + as.factor(NID) + PrePerformance +
                        UIdeo + UStanceLabel + AICorrectness, data = ref_data)

print("Reference design matrix dimensions:")
print(dim(X_ref))
print("Design matrix:")
print(X_ref)

# Extract fixed effects posterior samples
fixed_effects <- rec_model$Sol

print("Fixed effects dimensions:")
print(dim(fixed_effects))

# Guard: the design matrix must line up with the posterior, column for column.
stopifnot(identical(colnames(X_ref), colnames(fixed_effects)))

# Calculate linear predictors for each bias level
lin_pred <- X_ref %*% t(fixed_effects)

# Convert to matrix format (rows = posterior samples, cols = bias levels)
posterior_preds <- t(lin_pred)
colnames(posterior_preds) <- bias_levels

print("Posterior predictions matrix dimensions:")
print(dim(posterior_preds))
print("Bias levels:")
print(colnames(posterior_preds))

# Calculate means and credible intervals from posterior samples
marginal_means <- apply(posterior_preds, 2, mean)
marginal_lower <- apply(posterior_preds, 2, quantile, 0.025)
marginal_upper <- apply(posterior_preds, 2, quantile, 0.975)

results_manual <- data.frame(
  BiasedCat = names(marginal_means),
  Mean = marginal_means,
  Lower_CI = marginal_lower,
  Upper_CI = marginal_upper
)

print("Manual Marginal Means:")
print(results_manual)

# PAIRWISE COMPARISONS
# Get the column indices for each bias level
bias_names <- colnames(posterior_preds)
print("Available bias categories for comparisons:")
print(bias_names)

# Create all pairwise comparisons
n_bias <- length(bias_names)
pairwise_results <- data.frame()

for(i in 1:(n_bias-1)) {
  for(j in (i+1):n_bias) {
    # Calculate difference
    diff_values <- posterior_preds[, j] - posterior_preds[, i]
    
    # Create comparison name
    comparison_name <- paste(bias_names[j], "-", bias_names[i])
    
    # Calculate statistics
    estimate <- mean(diff_values)
    lower_ci <- quantile(diff_values, 0.025)
    upper_ci <- quantile(diff_values, 0.975)
    p_value <- 2 * min(mean(diff_values > 0), mean(diff_values < 0))
    
    # Add to results
    pairwise_results <- rbind(pairwise_results, data.frame(
      Comparison = comparison_name,
      Estimate = estimate,
      Lower_CI = lower_ci,
      Upper_CI = upper_ci,
      p_value = p_value
    ))
  }
}

# Apply FDR adjustment
pairwise_results$p_value_fdr <- p.adjust(pairwise_results$p_value, method = "fdr")

# Add significance indicators
pairwise_results$significant_fdr <- pairwise_results$p_value_fdr < 0.05

print("Pairwise Comparisons:")
print(pairwise_results)

# HEDGES' G EFFECT SIZES
print("\n=== Calculating Hedges' g Effect Sizes ===")

# For Bayesian models, we need to estimate the pooled SD from the posterior
# Extract residual variance from the model (for ordinal models, this is often set to 1)
# But we'll use the posterior variance components

# Get variance components
if("units" %in% names(rec_model$VCV)) {
  residual_var <- rec_model$VCV[, "units"]
} else {
  # For ordinal models, residual variance is often fixed at pi^2/3
  residual_var <- rep(pi^2/3, nrow(rec_model$Sol))
  print("Using default residual variance for ordinal model: pi^2/3")
}

# Calculate pooled SD for each posterior sample
pooled_sd <- sqrt(residual_var)

print(paste("Mean pooled SD:", round(mean(pooled_sd), 4)))

# Calculate sample sizes for each bias group
bias_ns <- table(single_ai_processed_$BiasedCat)
print("Sample sizes by bias group:")
print(bias_ns)

# Calculate Hedges' g for each pairwise comparison
hedges_results <- data.frame()

bias_names <- colnames(posterior_preds)
n_bias <- length(bias_names)

for(i in 1:(n_bias-1)) {
  for(j in (i+1):n_bias) {
    # Get sample sizes
    n1 <- bias_ns[bias_names[i]]
    n2 <- bias_ns[bias_names[j]]
    
    # Calculate difference in means (already computed)
    diff_values <- posterior_preds[, j] - posterior_preds[, i]
    
    # Calculate Hedges' g for each posterior sample
    cohens_d <- diff_values / pooled_sd
    
    # Apply bias correction for Hedges' g
    correction_factor <- 1 - (3 / (4 * (n1 + n2 - 2) - 1))
    hedges_g <- cohens_d * correction_factor
    
    # Create comparison name
    comparison_name <- paste(bias_names[j], "-", bias_names[i])
    
    # Calculate statistics for Hedges' g
    g_mean <- mean(hedges_g)
    g_lower <- quantile(hedges_g, 0.025)
    g_upper <- quantile(hedges_g, 0.975)
    
    # Effect size interpretation
    abs_g <- abs(g_mean)
    if(abs_g < 0.2) {
      interpretation <- "negligible"
    } else if(abs_g < 0.5) {
      interpretation <- "small"
    } else if(abs_g < 0.8) {
      interpretation <- "medium"
    } else {
      interpretation <- "large"
    }
    
    # Add to results
    hedges_results <- rbind(hedges_results, data.frame(
      Comparison = comparison_name,
      Hedges_g = g_mean,
      g_Lower_CI = g_lower,
      g_Upper_CI = g_upper,
      n1 = n1,
      n2 = n2,
      Interpretation = interpretation,
      stringsAsFactors = FALSE
    ))
  }
}

print("Hedges' g Effect Sizes:")
print(hedges_results)

# Create a summary table combining statistical tests and effect sizes
combined_results <- merge(pairwise_results, hedges_results, by = "Comparison")

print("\n=== Combined Results: Statistical Tests and Effect Sizes ===")
print(combined_results[, c("Comparison", "Estimate", "Lower_CI", "Upper_CI", 
                           "p_value_fdr", "significant_fdr", "Hedges_g", 
                           "g_Lower_CI", "g_Upper_CI", "Interpretation")])
