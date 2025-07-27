set.seed(123)

repdem_single_ai$BiasSide <- ifelse(((repdem_single_ai$AIStanceLabel_S == "Republican") &
                                       (repdem_single_ai$UStanceLabel_S == "Democrat")) |
                                      ((repdem_single_ai$AIStanceLabel_S == "Democrat") &
                                         (repdem_single_ai$UStanceLabel_S == "Republican")), "Opposite", "Same")

table(repdem_single_ai$BiasSide)

repdem_single_ai$BiasSide <-factor(repdem_single_ai$BiasSide, levels = c("Same", "Opposite"))

repdem_single_ai$PerceivedImproveCode <- factor(repdem_single_ai$PerceivedImproveCode, ordered = TRUE)

repdem_single_ai <- repdem_single_ai %>%
  mutate(AIInterMean_numeric = recode(AIInterMean,
                                      "Not meaningful at all" = 1,
                                      "Slightly meaningful" = 2,
                                      "Moderately meaningful" = 3,
                                      "Very meaningful" = 4,
                                      "Extremely meaningful" = 5
  ))

repdem_single_ai$AIInterMean_numeric <- factor(repdem_single_ai$AIInterMean_numeric, ordered = TRUE)

per_model <- clm(
  PerceivedImproveCode ~ BiasSide + as.factor(NID) + PrePerformance +
    UStanceLabel, # + UIdeo + AICorrectness
  data = repdem_single_ai, na.action = na.omit
)
vcov_per <- vcovCL(per_model, cluster = repdem_single_ai$UID)
clustered_results_per <- coeftest(per_model, vcov = vcov_per)
print(clustered_results_per)
summary(per_model)
df.residual(per_model)
r2(per_model)

per_model <- MCMCglmm(WillRecommendAI_Numeric ~ BiasSide + as.factor(NID) +
                        PrePerformance + as.factor(UStanceLabel),
                      random = ~UID,
                      family = "ordinal",
                      nitt = 25000, thin = 10, burnin = 5000,
                      data = repdem_single_ai)

summary(per_model)
se_fixed <- apply(per_model$Sol, 2, sd)
se_fixed
sqrt(per_model$VCV[ , "units"][1])
X  <- per_model$X                 # design matrix for fixed effects
B  <- as.matrix(per_model$Sol)            # iterations × coefficients
eta <- X %*% t(B)
VF  <- apply(eta, 2, var)          # fixed-effect variance per draw
VR  <- per_model$VCV[ , "UID"]      # random-effect variance per draw
VE <- 1                                  # residual var. (probit link)
R2_marginal    <- VF / (VF + VR + VE)               # fixed only
R2_conditional <- (VF + VR) / (VF + VR + VE)        # fixed + random
cbind(
  Marginal    = c(mean(R2_marginal),
                  quantile(R2_marginal, c(.025, .5, .975))),
  Conditional = c(mean(R2_conditional),
                  quantile(R2_conditional, c(.025, .5, .975)))
)

# =============================================
# Manual marginal means calculation for MCMCglm
# ==============================================
calculate_marginal_means_mcmc <- function(model, data, variable = "BiasSide") {
  
  # Get posterior samples
  post_samples <- model$Sol
  n_samples <- nrow(post_samples)
  
  # Create prediction datasets for each level of BiasSide
  levels_bias <- c("Opposite", "Same")
  
  # Set reference values for other predictors
  ref_performance <- mean(data$PrePerformance, na.rm = TRUE)
  ref_nid <- names(sort(table(data$NID), decreasing = TRUE))[6]  # Most common NID
  
  # Create prediction data
  pred_data_opposite <- data.frame(
    BiasSide = "Opposite",
    PrePerformance = ref_performance,
    NID = ref_nid
  )
  
  pred_data_same <- data.frame(
    BiasSide = "Same", 
    PrePerformance = ref_performance,
    NID = ref_nid
  )
  
  # Create design matrices
  # Note: Need to match the model matrix structure
  formula_fixed <- ~ BiasSide + as.factor(NID) + PrePerformance
  
  # Create full dataset for model matrix
  full_data <- rbind(pred_data_opposite, pred_data_same)
  full_data$BiasSide <- factor(full_data$BiasSide, levels = c("Opposite", "Same"))
  full_data$NID <- factor(full_data$NID, levels = levels(factor(data$NID)))
  
  X <- model.matrix(formula_fixed, data = full_data)
  
  # Calculate linear predictors for each posterior sample
  linear_preds <- X %*% t(post_samples)
  
  # For ordinal models, we work on the latent scale
  # The linear predictor gives us the latent variable means
  
  # Calculate summary statistics
  marginal_means <- data.frame(
    BiasSide = c("Opposite", "Same"),
    emmean = rowMeans(linear_preds),
    SE = apply(linear_preds, 1, sd),
    lower.CL = apply(linear_preds, 1, quantile, 0.025),
    upper.CL = apply(linear_preds, 1, quantile, 0.975),
    lower.CL_90 = apply(linear_preds, 1, quantile, 0.05),
    upper.CL_90 = apply(linear_preds, 1, quantile, 0.95)
  )
  
  return(list(
    marginal_means = marginal_means,
    posterior_samples = linear_preds
  ))
}

# Calculate marginal means
marginal_results <- calculate_marginal_means_mcmc(per_model, repdem_single_ai)
emm_bias_df <- marginal_results$marginal_means
emm_bias_df

# ================================
# Pairwise comparison analysis
# ================================
calculate_pairwise_mcmc <- function(posterior_samples) {
  
  # Calculate difference: Same - Opposite (row 2 - row 1)
  diff_samples <- posterior_samples[2, ] - posterior_samples[1, ]
  
  # Summary statistics for the difference
  difference <- mean(diff_samples)
  se_diff <- sd(diff_samples)
  lower_ci <- quantile(diff_samples, 0.025)
  upper_ci <- quantile(diff_samples, 0.975)
  
  # Bayesian p-value (probability that difference > 0)
  p_positive <- mean(diff_samples > 0)
  p_bayesian <- 2 * min(p_positive, 1 - p_positive)  # Two-tailed equivalent
  
  # Credible interval doesn't contain 0?
  significant <- !(lower_ci <= 0 & upper_ci >= 0)
  
  results <- data.frame(
    contrast = "Same - Opposite",
    estimate = round(difference, 4),
    SE = round(se_diff, 4),
    lower.CL = round(lower_ci, 4),
    upper.CL = round(upper_ci, 4),
    p_bayesian = round(p_bayesian, 4),
    prob_positive = round(p_positive, 4),
    significant = significant
  )
  
  return(list(
    summary = results,
    posterior_diff = diff_samples
  ))
}

# Calculate pairwise comparison
pairwise_results <- calculate_pairwise_mcmc(marginal_results$posterior_samples)
bias_pairwise_df <- pairwise_results$summary

# Add significance indicator
bias_pairwise_df$Sig <- ifelse(bias_pairwise_df$significant, "**", "")

# Bayesian effect size
calculate_bayesian_effect_size <- function(model, pairwise_result) {
  
  # Extract variance components from posterior
  vcv_samples <- model$VCV
  
  # Random effect variance (UID)
  sigma_u_samples <- vcv_samples[, "UID"]
  
  # Residual variance 
  sigma_e_samples <- vcv_samples[, "units"]
  
  # Total variance samples
  sigma_total_samples <- sqrt(sigma_u_samples + sigma_e_samples)
  
  # Effect size samples (difference / total SD)
  effect_size_samples <- pairwise_result$posterior_diff / sigma_total_samples
  
  # Summary statistics
  effect_size_mean <- mean(effect_size_samples)
  effect_size_se <- sd(effect_size_samples)
  effect_size_lower <- quantile(effect_size_samples, 0.025)
  effect_size_upper <- quantile(effect_size_samples, 0.975)
  
  # Interpretation
  interpretation <- ifelse(abs(effect_size_mean) < 0.2, "negligible",
                           ifelse(abs(effect_size_mean) < 0.5, "small",
                                  ifelse(abs(effect_size_mean) < 0.8, "medium", "large")))
  
  results <- data.frame(
    Comparison = "Same - Opposite",
    Effect_Size = round(effect_size_mean, 4),
    SE = round(effect_size_se, 4),
    Lower_CrI = round(effect_size_lower, 4),
    Upper_CrI = round(effect_size_upper, 4),
    Interpretation = interpretation,
    Prob_Positive = round(mean(effect_size_samples > 0), 4)
  )
  
  return(results)
}

# Calculate Bayesian effect size
effect_size_result <- calculate_bayesian_effect_size(per_model, pairwise_results)

# Add value level indicator for plotting
emm_bias_df <- emm_bias_df %>%
  mutate(value_level = ifelse(emmean == max(emmean), "Higher", "Lower"))

# Define colors - darker for higher value, lighter for lower value (purple theme)
color_higher <- "#4B0082"  # Dark purple for higher value
color_lower <- "#DDA0DD"   # Light purple for lower value

# B88BD8 FF8C00
p_bias_side <- ggplot(emm_bias_df, aes(x = BiasSide, y = emmean, fill = value_level)) +
  geom_col(fill = "#FF8C00", alpha = 0.7, width = 0.6) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), 
                width = 0.2, color = "black", linewidth = 0.5) +
  geom_point(shape = 17, 
             size = 2., color = "black", fill = "white") +
  geom_text(aes(y = upper.CL + max(upper.CL, na.rm = TRUE) * 0.07, 
                label = round(emmean, 3)), 
            size = 3.7, family = "Avenir") +
  scale_fill_manual(values = c("Higher" = color_higher, "Lower" = color_lower),
                    guide = "none") +  # Remove legend since it's self-explanatory
  scale_y_continuous(
    name = "Interaction Meaningfulness",
    labels = scales::number_format(accuracy = 0.01),
    # limits = c(min(emm_bias_df$lower.CL) * 0.95, max(emm_bias_df$upper.CL) * 1.05),
    expand = c(0, 0)
  ) +
  coord_cartesian(ylim = c(1., 4.8)) +
  scale_x_discrete(
    name = "AI Bias Direction",
    labels = c("Opposite" = "Opposition Bias", "Same" = "Echo-Chamber Bias")
  ) +
  
  theme_classic() +
  theme(
    text = element_text(family = "Avenir"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.line = element_blank(),
    axis.title.x = element_text(family = "Avenir", size = 12, margin = margin(t = 12)),
    axis.title.y = element_text(family = "Avenir", size = 12, color = "black", margin = margin(r = 12)),
    axis.text.x = element_text(family = "Avenir", size = 10, color = "black"),
    axis.text.y = element_text(family = "Avenir", size = 10, color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 0.4),
    axis.ticks.length = unit(3, "pt"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    plot.margin = margin(t = 15, r = 20, b = 15, l = 15),
    plot.title = element_text(family = "Avenir", size = 14, hjust = 0.5, margin = margin(b = 20))
  )

# Display the plot
print(p_bias_side)

# Optional: Create a summary table for easy interpretation
summary_table <- emm_bias_df %>%
  select(BiasSide, emmean, SE, lower.CL, upper.CL) %>%
  mutate(
    emmean = round(emmean, 3),
    SE = round(SE, 3),
    lower.CL = round(lower.CL, 3),
    upper.CL = round(upper.CL, 3),
    CI_95 = paste0("[", lower.CL, ", ", upper.CL, "]")
  ) %>%
  select(BiasSide, emmean, SE, CI_95)

cat("\n=== SUMMARY TABLE ===\n")
print(summary_table)

# Effect size calculation
effect_size <- abs(bias_pairwise$Difference[1])
pooled_se <- mean(emm_bias_df$SE)
cohens_d <- effect_size / pooled_se

cat(paste("\n=== EFFECT SIZE ===\n"))
cat(paste("Absolute difference:", round(effect_size, 4), "\n"))
cat(paste("Cohen's d (approximate):", round(cohens_d, 3), "\n"))
cat(paste("Interpretation:", 
          if(cohens_d < 0.2) "Negligible effect" 
          else if(cohens_d < 0.5) "Small effect"
          else if(cohens_d < 0.8) "Medium effect"
          else "Large effect", "\n"))




