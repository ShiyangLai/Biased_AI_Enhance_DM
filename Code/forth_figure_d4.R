# 1. Process single_ai_processed data
single_ai_data <- single_ai_processed %>%
  dplyr::select(all_of(columns_to_keep_1)) %>%
  mutate(
    ExperimentType = case_when(
      # Non-Biased cases
      AIStanceLabel_S == "Default" ~ "Single_AI_Non_Biased",
      AIStanceLabel_S == "Neutral" ~ "Single_AI_Non_Biased_Exp",
      # Balanced: User and AI on opposite political sides
      (UStanceLabel_S == "Democrat" & AIStanceLabel_S == "Republican") |
        (UStanceLabel_S == "Republican" & AIStanceLabel_S == "Democrat") ~ "Single_AI_Opposition",
      # All other cases
      TRUE ~ "Single_AI_Echo_Chamber"
    )
  )

single_ai_data <- single_ai_data[single_ai_data$ExperimentType != "Single_AI_Echo_Chamber", ]
single_ai_data$AI1Correctness <- single_ai_data$AICorrectness
single_ai_data$AI2Correctness <- single_ai_data$AICorrectness

# 2. Process df2 data
dual_ai_data <- df2 %>%
  dplyr::select(all_of(columns_to_keep_2)) %>%
  rename(AIStanceLabel_S = AI_Combo_Numeric) %>%
  mutate(
    ExperimentType = case_when(
      # Non-Biased cases
      AI1StanceLabel_S == "Default" & AI2StanceLabel_S == "Default" ~ "Dual_AI_Non_Biased",
      AI1StanceLabel_S == "Neutral" & AI2StanceLabel_S == "Neutral" ~ "Dual_AI_Non_Biased_Exp",
      # Balanced: User stance is between AI1 and AI2 stances
      AI1StanceCode != AI2StanceCode &
        UStanceCode > pmin(AI1StanceCode, AI2StanceCode) &
        UStanceCode < pmax(AI1StanceCode, AI2StanceCode) ~ "Dual_AI_Balanced",
      # Opposition: User stance is different from the two AI stances direction
      (UStanceCode != 0) & 
        (AI1StanceCode != 0) & (AI2StanceCode != 0) &
        (sign(AI1StanceCode) == -sign(UStanceCode)) & 
        (sign(AI2StanceCode) == -sign(UStanceCode)) ~ "Dual_AI_Opposition",
      TRUE ~ "Dual_AI_Other"
    )
  )

dual_ai_data <- dual_ai_data[dual_ai_data$ExperimentType != "Dual_AI_Other", ]
dual_ai_data$AICorrectness <- (dual_ai_data$AI1Correctness + dual_ai_data$AI2Correctness)/2

# 3. Combine all data
combined_data <- bind_rows(
  single_ai_data,
  dual_ai_data
)

# 4. Convert ExperimentType to factor for regression analysis
combined_data$ExperimentType <- factor(combined_data$ExperimentType, 
                                       levels = c("Single_AI_Non_Biased",
                                                  # "Single_AI_Non_Biased_Exp",
                                                  # "Single_AI_Echo_Chamber",
                                                  "Single_AI_Biased",
                                                  # "Single_AI_Opposition",
                                                  "Dual_AI_Non_Biased",
                                                  # "Dual_AI_Non_Biased_Exp",
                                                  "Dual_AI_Opposition",
                                                  "Dual_AI_Balanced"))

# 5. Display summary statistics
print("Distribution of ExperimentType:")
print(table(combined_data$ExperimentType))

print("\nCross-tabulation of ExperimentType by User Stance:")
print(table(combined_data$ExperimentType, combined_data$UStanceLabel, useNA = "ifany"))

print("\nBreakdown by BiasedType within each ExperimentType:")
print(table(combined_data$ExperimentType, combined_data$BiasedType, useNA = "ifany"))

model_full <- lm(
  ConvLength ~ ExperimentType + PrePerformance +
    as.factor(NID) + as.factor(UStanceLabel) + UIdeo + AICorrectness,
  data = combined_data,
  na.action = na.omit
)
# Calculate clustered variance-covariance matrix by UID
vcov_clustered_full <- vcovCL(model_full, cluster = combined_data$UID)

# Get coefficients with clustered standard errors
clustered_results_full <- coeftest(model_full, vcov = vcov_clustered_full)
print(clustered_results_full)
summary(model_full)
summary(model_full)$sigma

# Fit the model
model_full_mixed <- lmer(
  ConvLength ~ ExperimentType + PrePerformance +
    as.factor(NID) + as.factor(UStanceLabel) + UIdeo + AICorrectness +
    (1 | UID),
  data = combined_data,
  na.action = na.omit
)

summary(model_full_mixed)
summary(model_full_mixed)$sigma
df.residual(model_full_mixed)
r2(model_full_mixed)

# =====================================
# Visualization
# =====================================

# Get estimated marginal means for ExperimentType
emm_results <- emmeans(model_full_mixed, ~ ExperimentType)

# Convert to data frame for plotting
plot_data <- as.data.frame(emm_results)

# Rename columns to match plotting expectations
plot_data <- plot_data %>%
  rename(estimate = emmean,
         std_error = SE,
         lower_ci = lower.CL,
         upper_ci = upper.CL) %>%
  mutate(treatments = as.character(ExperimentType))

# Create formal labels
plot_data$formal_label <- case_when(
  plot_data$treatments == "Single_AI_Non_Biased" ~ "Single AI\nNon-Biased",
  plot_data$treatments == "Single_AI_Biased" ~ "Single AI\nBiased",
  # plot_data$treatments == "Single_AI_Non_Biased_Exp" ~ "Single AI\nNeutralized", 
  # plot_data$treatments == "Single_AI_Opposition" ~ "Single AI\nOpposition",
  plot_data$treatments == "Dual_AI_Non_Biased" ~ "Dual AI\nNon-Biased",
  # plot_data$treatments == "Dual_AI_Non_Biased_Exp" ~ "Dual AI\nNeutralized",
  plot_data$treatments == "Dual_AI_Opposition" ~ "Dual AI\nOpposition",
  plot_data$treatments == "Dual_AI_Balanced" ~ "Dual AI\nBalanced",
  TRUE ~ as.character(plot_data$treatments)
)

# Ensure proper factor ordering for formal labels (reversed for horizontal plot)
plot_data$formal_label <- factor(plot_data$formal_label, 
                                 levels = rev(c("Single AI\nNon-Biased",
                                                "Single AI\nBiased",
                                                # "Single AI\nNeutralized",
                                                # "Single AI\nOpposition",
                                                "Dual AI\nNon-Biased",
                                                # "Dual AI\nNeutralized",
                                                "Dual AI\nOpposition",
                                                "Dual AI\nBalanced")))

# Reorder data according to formal_label factor levels
plot_data <- plot_data[order(plot_data$formal_label), ]

print("Data order verification:")
print(plot_data[, c("formal_label", "estimate")])

# =====================================
# Value-based color mapping (BROWN THEME)
# =====================================

# Define brown color palette
color_perceived <- "#654321"    # Dark brown
color_perceived_90 <- "#8B4513" # Saddle brown
color_perceived_95 <- "#A0522D" # Sienna
color_perceived_99 <- "#D2B48C" # Tan

# Create color gradient based on estimate values
min_val <- min(plot_data$estimate)
max_val <- max(plot_data$estimate)

# Normalize values to 0-1 range
plot_data$normalized_value <- (plot_data$estimate - min_val) / (max_val - min_val)

# Create color mapping function
get_color_for_value <- function(norm_val) {
  colors <- c(color_perceived_99, color_perceived_95, color_perceived_90, color_perceived)
  breaks <- c(0, 0.33, 0.66, 1.0)
  
  if (norm_val <= breaks[2]) {
    # Between lightest and second lightest
    ratio <- norm_val / breaks[2]
    colorRampPalette(c(colors[1], colors[2]))(100)[round(ratio * 99) + 1]
  } else if (norm_val <= breaks[3]) {
    # Between second lightest and second darkest
    ratio <- (norm_val - breaks[2]) / (breaks[3] - breaks[2])
    colorRampPalette(c(colors[2], colors[3]))(100)[round(ratio * 99) + 1]
  } else {
    # Between second darkest and darkest
    ratio <- (norm_val - breaks[3]) / (breaks[4] - breaks[3])
    colorRampPalette(c(colors[3], colors[4]))(100)[round(ratio * 99) + 1]
  }
}

# Apply color mapping
plot_data$fill_color <- sapply(plot_data$normalized_value, get_color_for_value)

# Calculate appropriate x-axis limits
x_min <- min(plot_data$lower_ci) * 0.95
x_max <- max(plot_data$upper_ci) * 1.05

# =====================================
# Create plot
# =====================================

# Create horizontal bar plot
p_horizontal <- ggplot(plot_data, aes(y = formal_label, x = estimate)) +
  geom_col(aes(fill = fill_color), color = "black", width = 0.7, linewidth = 0) +
  geom_errorbar(aes(xmin = lower_ci, xmax = upper_ci), 
                width = 0.3, linewidth = 0.5, color = "black") +
  
  # Add value labels to the right of error bars
  geom_text(aes(x = upper_ci + (x_max - x_min) * 0.05, 
                label = round(estimate, 1)), 
            hjust = 0, family = "Avenir", size = 3, color = "black") +
  
  # Use manual fill scale
  scale_fill_identity() +
  
  # Set x-axis
  scale_x_continuous(
    expand = c(0, 0), 
    labels = scales::number_format(accuracy = 0.1)
  ) +
  
  # Use coord_cartesian to set limits
  coord_cartesian(xlim = c(x_min, x_max * 2)) +
  
  labs(y = "Experimental Condition", 
       x = "Conversation Length") +
  
  theme_classic() +
  theme(
    text = element_text(family = "Avenir", color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.line = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(family = "Avenir", size = 9, color = "black"),
    axis.title.y = element_blank(),
    axis.title.x = element_text(family = "Avenir", size = 12, color = "black",
                                margin = margin(t = 12)),
    axis.ticks = element_line(color = "black", linewidth = 0.4),
    axis.ticks.length = unit(3, "pt"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    plot.margin = margin(t = 15, r = 40, b = 15, l = 15),
    legend.position = "none"
  )

print(p_horizontal)



# =====================================
# Pairwise comparison
# =====================================

# Get estimated marginal means for ExperimentType (if not already computed)
emm_results <- emmeans(model_full_mixed, ~ ExperimentType)

# Perform all pairwise comparisons with FDR adjustment
pairwise_comparisons <- pairs(emm_results, adjust = "fdr")

# Convert to data frame for easier manipulation
comparisons <- as.data.frame(pairwise_comparisons)

# Calculate sample sizes for each treatment group
n_by_group <- table(combined_data$ExperimentType)

# Get pooled standard deviation (residual SD from lmer model)
pooled_sd <- sigma(model_full_mixed)

# Hedges' correction factor function
hedges_correction <- function(n1, n2) {
  df <- n1 + n2 - 2
  if (df > 0) {
    return(1 - (3 / (4 * df - 1)))
  } else {
    return(1)  # Fallback if df calculation fails
  }
}

# Clean up the comparison names and add significance symbols + Hedges' g
comparisons <- comparisons %>%
  # Rename columns for clarity
  rename(
    Comparison = contrast,
    Difference = estimate,
    SE = SE,
    df = df,
    t_stat = t.ratio,
    p_value = p.value
  ) %>%
  # Add adjusted p-values (already computed by emmeans with adjust="fdr")
  mutate(
    p_adj = p_value,  # emmeans already applied FDR adjustment
    # Add significance symbols
    Significance = case_when(
      p_adj < 0.001 ~ "***",
      p_adj < 0.01 ~ "**", 
      p_adj < 0.05 ~ "*",
      p_adj < 0.1 ~ "†",
      TRUE ~ "ns"
    ),
    # Round numeric columns for cleaner display
    Difference = round(Difference, 4),
    SE = round(SE, 4),
    t_stat = round(t_stat, 3),
    p_value = round(p_value, 4),
    p_adj = round(p_adj, 4)
  )

# Calculate Hedges' g for each comparison
comparisons$Hedges_g <- numeric(nrow(comparisons))
comparisons$Hedges_g_Lower <- numeric(nrow(comparisons))
comparisons$Hedges_g_Upper <- numeric(nrow(comparisons))

for (i in 1:nrow(comparisons)) {
  # Extract group names from contrast
  contrast_parts <- strsplit(as.character(comparisons$Comparison[i]), " - ")[[1]]
  group1 <- trimws(contrast_parts[1])
  group2 <- trimws(contrast_parts[2])
  
  # Get sample sizes
  n1 <- n_by_group[group1]
  n2 <- n_by_group[group2]
  
  # Calculate Cohen's d
  cohens_d <- comparisons$Difference[i] / pooled_sd
  
  # Apply Hedges' correction
  correction <- hedges_correction(n1, n2)
  hedges_g <- cohens_d * correction
  
  # Calculate confidence interval for Hedges' g
  # Standard error for Hedges' g
  se_hedges_g <- sqrt((n1 + n2)/(n1 * n2) + hedges_g^2/(2 * (n1 + n2 - 2)))
  
  # Store results
  comparisons$Hedges_g[i] <- round(hedges_g, 4)
  comparisons$Hedges_g_Lower[i] <- round(hedges_g - 1.96 * se_hedges_g, 4)
  comparisons$Hedges_g_Upper[i] <- round(hedges_g + 1.96 * se_hedges_g, 4)
}

# Print results
cat("\n=== EMMEANS PAIRWISE COMPARISONS WITH HEDGES' G ===\n")
cat("FDR-adjusted p-values and effect sizes\n")
cat("*** p < 0.001, ** p < 0.01, * p < 0.05, † p < 0.1, ns = not significant\n")
cat("Pooled SD (residual):", round(pooled_sd, 4), "\n\n")

# Display main results
print(comparisons[, c("Comparison", "Difference", "SE", "t_stat", "p_adj", "Hedges_g", "Significance")])

# =====================================
# Detailed results table
# =====================================

cat("\n=== DETAILED RESULTS WITH CONFIDENCE INTERVALS ===\n")
detailed_results <- comparisons %>%
  select(Comparison, Difference, SE, p_adj, Hedges_g, Hedges_g_Lower, Hedges_g_Upper, Significance) %>%
  arrange(p_adj)

print(detailed_results)

# =====================================
# Summary statistics
# =====================================

cat("\n=== SUMMARY ===\n")
cat("Total comparisons:", nrow(comparisons), "\n")
cat("Significant (FDR p < 0.05):", sum(comparisons$p_adj < 0.05), "\n")
cat("Large effect sizes (|g| ≥ 0.8):", sum(abs(comparisons$Hedges_g) >= 0.8), "\n")
cat("Medium+ effect sizes (|g| ≥ 0.5):", sum(abs(comparisons$Hedges_g) >= 0.5), "\n")
cat("Small+ effect sizes (|g| ≥ 0.2):", sum(abs(comparisons$Hedges_g) >= 0.2), "\n")

# =====================================
# Effect size distribution
# =====================================

cat("\n=== EFFECT SIZE DISTRIBUTION ===\n")
effect_size_table <- table(comparisons$Effect_Size)
print(effect_size_table)

cat("\nHedges' g range: [", round(min(comparisons$Hedges_g), 3), ", ", round(max(comparisons$Hedges_g), 3), "]\n")

# =====================================
# Top significance difference 
# =====================================

cat("\n=== TOP 5 SIGNIFICANT DIFFERENCES ===\n")
top_significant <- comparisons %>%
  filter(p_adj < 0.05) %>%
  arrange(p_adj) %>%
  head(5) %>%
  select(Comparison, Difference, p_adj, Hedges_g, Effect_Size)

if(nrow(top_significant) > 0) {
  print(top_significant)
} else {
  cat("No significant differences found after FDR correction.\n")
}

# =====================================
# Largest effect sizes
# =====================================

cat("\n=== TOP 5 LARGEST EFFECT SIZES ===\n")
top_effects <- comparisons %>%
  arrange(desc(abs(Hedges_g))) %>%
  head(5) %>%
  select(Comparison, Difference, Hedges_g, Effect_Size, p_adj, Significance)

print(top_effects)


cnts <- table(single_ai_processed$NID)      # replace df2 with your data frame, if different

# Convert to numeric, then compute summary stats
mean_cnts <- mean(as.numeric(cnts))
sd_cnts   <- sd(as.numeric(cnts))

# Print the results
mean_cnts
sd_cnts


##################################
# Engagement score analysis
##################################
dimensions <- c("EngagementBehavioral", "EngagementCognitive", "EngagementEmotional", 
                "EngagementAutonomy", "EngagementSocialPresence")

dimension_names <- c("Behavioral", "Cognitive", "Emotional", "Autonomy", "Social Presence")

# Set reference level for ExperimentType (using Single_AI_Non_Biased as reference)
combined_data$ExperimentType <- relevel(as.factor(combined_data$ExperimentType), 
                                        ref = "Single_AI_Non_Biased")

# Create comprehensive results table
engagement_results <- data.frame()
all_models <- list()  # Store models for regression tables

for(i in 1:length(dimensions)) {
  cat(sprintf("\n--- %s Engagement ---\n", dimension_names[i]))
  
  # Mixed effects model with UID random intercept and NID fixed effect
  engagement_model <- lmer(
    formula = paste(dimensions[i], "~ ExperimentType + as.factor(NID) + PrePerformance + (1|UID) + as.factor(UStanceLabel)"),
    data = combined_data  # Updated to use combined_data instead of single_ai_processed_
  )
  
  # Store model for later regression table
  all_models[[dimension_names[i]]] <- engagement_model
  
  # Get model summary
  model_summary <- summary(engagement_model)
  
  # Extract results using tidy
  tidy_results <- tidy(engagement_model, effects = "fixed", conf.int = TRUE)
  
  # Get ExperimentType coefficients (excluding intercept and other variables)
  experiment_coefs <- tidy_results[grepl("^ExperimentType", tidy_results$term), ]
  
  # Calculate means for each ExperimentType group
  exp_means <- combined_data %>%
    group_by(ExperimentType) %>%
    summarise(
      Mean = mean(.data[[dimensions[i]]], na.rm = TRUE),
      N = sum(!is.na(.data[[dimensions[i]]]))
    ) %>%
    arrange(ExperimentType)
  
  # Print means for each group
  cat("  Group Means:\n")
  for(j in 1:nrow(exp_means)) {
    cat(sprintf("    %s: M = %.3f (N = %d)\n", 
                exp_means$ExperimentType[j], exp_means$Mean[j], exp_means$N[j]))
  }
  
  # Print coefficients (differences from reference group)
  cat("\n  Regression Coefficients (vs. Single_AI_Non_Biased):\n")
  if(nrow(experiment_coefs) > 0) {
    for(j in 1:nrow(experiment_coefs)) {
      coef_name <- gsub("ExperimentType", "", experiment_coefs$term[j])
      cat(sprintf("    %s: β = %.3f ± %.3f, t(%.1f) = %.3f, p = %.4f %s\n",
                  coef_name,
                  experiment_coefs$estimate[j],
                  experiment_coefs$std.error[j],
                  experiment_coefs$df[j],
                  experiment_coefs$statistic[j],
                  experiment_coefs$p.value[j],
                  case_when(
                    experiment_coefs$p.value[j] < 0.01 ~ "***",
                    experiment_coefs$p.value[j] < 0.05 ~ "**", 
                    experiment_coefs$p.value[j] < 0.1 ~ "*",
                    TRUE ~ ""
                  )))
      cat(sprintf("         95%% CI: [%.3f, %.3f]\n", 
                  experiment_coefs$conf.low[j], experiment_coefs$conf.high[j]))
    }
  }
  
  # Store results for summary table
  for(j in 1:nrow(experiment_coefs)) {
    coef_name <- gsub("ExperimentType", "", experiment_coefs$term[j])
    engagement_results <- rbind(engagement_results, data.frame(
      Dimension = dimension_names[i],
      Comparison = paste("vs. Single_AI_Non_Biased"),
      ExperimentType = coef_name,
      Coefficient = round(experiment_coefs$estimate[j], 3),
      Std_Error = round(experiment_coefs$std.error[j], 3),
      df = round(experiment_coefs$df[j], 1),
      t_statistic = round(experiment_coefs$statistic[j], 3),
      p_value = round(experiment_coefs$p.value[j], 4),
      CI_lower = round(experiment_coefs$conf.low[j], 3),
      CI_upper = round(experiment_coefs$conf.high[j], 3),
      Significance = case_when(
        experiment_coefs$p.value[j] < 0.01 ~ "***",
        experiment_coefs$p.value[j] < 0.05 ~ "**", 
        experiment_coefs$p.value[j] < 0.1 ~ "*",
        TRUE ~ ""
      )
    ))
  }
  
  # Print random effects variance
  random_effects_var <- as.data.frame(VarCorr(engagement_model))
  uid_var <- random_effects_var[random_effects_var$grp == "UID", "vcov"]
  residual_var <- random_effects_var[random_effects_var$grp == "Residual", "vcov"]
  icc <- uid_var / (uid_var + residual_var)
  cat(sprintf("  ICC (UID): %.3f\n", icc))
  
  # Model fit statistics
  cat(sprintf("  AIC: %.2f, BIC: %.2f\n", AIC(engagement_model), BIC(engagement_model)))
}

# Print comprehensive summary table
cat("\n=== ENGAGEMENT RESULTS SUMMARY ===\n")
print(engagement_results, digits = 3)

# Multiple comparison adjustments
if(nrow(engagement_results) > 0) {
  # Apply adjustments within each dimension
  engagement_results <- engagement_results %>%
    group_by(Dimension) %>%
    mutate(
      p_bonferroni_within = round(p.adjust(p_value, method = "bonferroni"), 4),
      p_fdr_within = round(p.adjust(p_value, method = "fdr"), 4)
    ) %>%
    ungroup() %>%
    mutate(
      p_bonferroni_overall = round(p.adjust(p_value, method = "bonferroni"), 4),
      p_fdr_overall = round(p.adjust(p_value, method = "fdr"), 4)
    )
  
  cat("\n=== MULTIPLE COMPARISON ADJUSTMENTS ===\n")
  print(engagement_results[, c("Dimension", "ExperimentType", "p_value", 
                               "p_bonferroni_within", "p_fdr_within",
                               "p_bonferroni_overall", "p_fdr_overall")], digits = 3)
  
  # Summary statistics
  n_total_comparisons <- nrow(engagement_results)
  n_significant_raw <- sum(engagement_results$p_value < 0.05)
  n_significant_bonf_within <- sum(engagement_results$p_bonferroni_within < 0.05)
  n_significant_fdr_within <- sum(engagement_results$p_fdr_within < 0.05)
  n_significant_bonf_overall <- sum(engagement_results$p_bonferroni_overall < 0.05)
  n_significant_fdr_overall <- sum(engagement_results$p_fdr_overall < 0.05)
  
  cat(sprintf("\nSUMMARY:\n"))
  cat(sprintf("- Total comparisons: %d\n", n_total_comparisons))
  cat(sprintf("- Significant (uncorrected p < 0.05): %d (%.1f%%)\n", 
              n_significant_raw, n_significant_raw/n_total_comparisons*100))
  cat(sprintf("- Significant (Bonferroni within-dimension): %d (%.1f%%)\n", 
              n_significant_bonf_within, n_significant_bonf_within/n_total_comparisons*100))
  cat(sprintf("- Significant (FDR within-dimension): %d (%.1f%%)\n", 
              n_significant_fdr_within, n_significant_fdr_within/n_total_comparisons*100))
  cat(sprintf("- Significant (Bonferroni overall): %d (%.1f%%)\n", 
              n_significant_bonf_overall, n_significant_bonf_overall/n_total_comparisons*100))
  cat(sprintf("- Significant (FDR overall): %d (%.1f%%)\n", 
              n_significant_fdr_overall, n_significant_fdr_overall/n_total_comparisons*100))
}

# CREATE REGRESSION TABLES
cat("\n=== REGRESSION TABLES ===\n")

# Function to create a formatted regression table for each dimension
create_regression_table <- function(model, dimension_name) {
  cat(sprintf("\n--- %s Regression Results ---\n", dimension_name))
  
  # Extract fixed effects
  fixed_effects <- summary(model)$coefficients
  
  # Format table
  cat(sprintf("%-35s %8s %8s %8s %8s\n", "Variable", "Coef.", "SE", "t-value", "p-value"))
  cat(paste(rep("-", 70), collapse = ""), "\n")
  
  for(i in 1:nrow(fixed_effects)) {
    var_name <- rownames(fixed_effects)[i]
    # Clean up variable names for display
    if(grepl("ExperimentType", var_name)) {
      var_name <- paste("  ", gsub("ExperimentType", "", var_name))
    } else if(grepl("as.factor\\(NID\\)", var_name)) {
      var_name <- paste("  NID:", gsub("as.factor\\(NID\\)", "", var_name))
    } else if(grepl("as.factor\\(UStanceLabel\\)", var_name)) {
      var_name <- paste("  Stance:", gsub("as.factor\\(UStanceLabel\\)", "", var_name))
    }
    
    cat(sprintf("%-35s %8.3f %8.3f %8.3f %8.4f %s\n",
                var_name,
                fixed_effects[i, "Estimate"],
                fixed_effects[i, "Std. Error"],
                fixed_effects[i, "t value"],
                fixed_effects[i, "Pr(>|t|)"],
                case_when(
                  fixed_effects[i, "Pr(>|t|)"] < 0.01 ~ "***",
                  fixed_effects[i, "Pr(>|t|)"] < 0.05 ~ "**", 
                  fixed_effects[i, "Pr(>|t|)"] < 0.1 ~ "*",
                  TRUE ~ ""
                )))
  }
  
  # Model statistics
  cat(paste(rep("-", 70), collapse = ""), "\n")
  random_effects_var <- as.data.frame(VarCorr(model))
  uid_var <- random_effects_var[random_effects_var$grp == "UID", "vcov"]
  residual_var <- random_effects_var[random_effects_var$grp == "Residual", "vcov"]
  icc <- uid_var / (uid_var + residual_var)
  
  cat(sprintf("N observations: %d\n", nrow(model@frame)))
  cat(sprintf("N groups (UID): %d\n", length(unique(model@frame$`(1 | UID)`))))
  cat(sprintf("ICC: %.3f\n", icc))
  cat(sprintf("AIC: %.2f\n", AIC(model)))
  cat(sprintf("BIC: %.2f\n", BIC(model)))
  cat("Significance: *** p<0.01, ** p<0.05, * p<0.1\n")
}

# Generate regression tables for each dimension
for(i in 1:length(dimension_names)) {
  if(dimension_names[i] %in% names(all_models)) {
    create_regression_table(all_models[[dimension_names[i]]], dimension_names[i])
  }
}

# ========================================
# Visualization
# ========================================
# Define colors for 7 ExperimentType categories
experiment_colors <- c(
  "Single_AI_Non_Biased" = "#1f77b4",        # Blue
  "Single_AI_Non_Biased_Exp" = "#ff7f0e",    # Orange  
  "Single_AI_Opposition" = "#2ca02c",         # Green
  "Dual_AI_Non_Biased" = "#d62728",          # Red
  "Dual_AI_Non_Biased_Exp" = "#9467bd",      # Purple
  "Dual_AI_Opposition" = "#8c564b",          # Brown
  "Dual_AI_Balanced" = "#e377c2"             # Pink
)

# Alternative color scheme (colorbrewer Set2)
experiment_colors_alt <- c(
  "Single_AI_Non_Biased" = "#66c2a5",
  "Single_AI_Non_Biased_Exp" = "#fc8d62", 
  "Single_AI_Opposition" = "#8da0cb",
  "Dual_AI_Non_Biased" = "#e78ac3",
  "Dual_AI_Non_Biased_Exp" = "#a6d854",
  "Dual_AI_Opposition" = "#ffd92f",
  "Dual_AI_Balanced" = "#e5c494"
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

# ========================================
# 2. Engagement visualization
# ========================================

# Create engagement data for visualization
if(nrow(engagement_results) > 0) {
  
  # Calculate means for each ExperimentType and dimension
  engagement_means <- combined_data %>%
    select(ExperimentType, all_of(dimensions)) %>%
    pivot_longer(cols = all_of(dimensions), names_to = "Dimension_raw", values_to = "Score") %>%
    mutate(
      Dimension = case_when(
        Dimension_raw == "EngagementBehavioral" ~ "Behavioral",
        Dimension_raw == "EngagementCognitive" ~ "Cognitive", 
        Dimension_raw == "EngagementEmotional" ~ "Emotional",
        Dimension_raw == "EngagementAutonomy" ~ "Autonomy",
        Dimension_raw == "EngagementSocialPresence" ~ "Social Presence",
        TRUE ~ gsub("Engagement", "", Dimension_raw)
      )
    ) %>%
    group_by(ExperimentType, Dimension) %>%
    summarise(
      Mean_Score = mean(Score, na.rm = TRUE),
      SE = sd(Score, na.rm = TRUE) / sqrt(sum(!is.na(Score))),
      .groups = 'drop'
    )
  
  # OPTION A: Heatmap visualization
  engagement_heatmap <- ggplot(engagement_means, 
                               aes(x = Dimension, y = ExperimentType, fill = Mean_Score)) +
    geom_tile(color = "white", size = 0.5) +
    geom_text(aes(label = round(Mean_Score, 2)), 
              color = "white", size = 3, family = "Avenir") +
    scale_fill_gradient2(low = "#2166ac", mid = "#f7f7f7", high = "#b2182b",
                         midpoint = mean(engagement_means$Mean_Score),
                         name = "Mean\nScore") +
    labs(
      x = "Engagement Dimension",
      y = "Experiment Type",
      title = "Engagement Scores by Experiment Type and Dimension"
    ) +
    nature_theme +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text(size = 8),
      legend.position = "right"
    )
  
  print(engagement_heatmap)
  
  # OPTION B: Grouped bar chart
  engagement_barplot <- ggplot(engagement_means, 
                               aes(x = Dimension, y = Mean_Score, 
                                   fill = ExperimentType)) +
    geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
    geom_errorbar(aes(ymin = Mean_Score - SE, ymax = Mean_Score + SE),
                  position = position_dodge(width = 0.9), width = 0.25) +
    scale_fill_manual(values = experiment_colors) +
    labs(
      x = "Engagement Dimension",
      y = "Mean Score",
      fill = "Experiment Type",
      title = "Engagement Scores by Dimension and Experiment Type"
    ) +
    nature_theme +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 7)
    ) +
    guides(fill = guide_legend(nrow = 2))
  
  print(engagement_barplot)
  
  # OPTION C: Faceted line plot
  engagement_lineplot <- ggplot(engagement_means, 
                                aes(x = Dimension, y = Mean_Score, 
                                    color = ExperimentType, group = ExperimentType)) +
    geom_line(size = 1, alpha = 0.8) +
    geom_point(size = 2.5, alpha = 0.9) +
    scale_color_manual(values = experiment_colors) +
    labs(
      x = "Engagement Dimension",
      y = "Mean Score",
      color = "Experiment Type",
      title = "Engagement Profiles by Experiment Type"
    ) +
    nature_theme +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 7)
    ) +
    guides(color = guide_legend(nrow = 2))
  
  print(engagement_lineplot)
  
  # OPTION D: Simplified radar plot (showing only a subset or comparing groups)
  # Group Single_AI vs Dual_AI for cleaner visualization
  engagement_grouped <- engagement_means %>%
    mutate(
      AI_Type = ifelse(grepl("Single_AI", ExperimentType), "Single_AI", "Dual_AI")
    ) %>%
    group_by(AI_Type, Dimension) %>%
    summarise(Mean_Score = mean(Mean_Score), .groups = 'drop')
  
  # Calculate radar coordinates for grouped data
  n_dimensions <- length(unique(engagement_grouped$Dimension))
  angles <- seq(0, 2*pi, length.out = n_dimensions + 1)[1:n_dimensions]
  
  engagement_radar_grouped <- engagement_grouped %>%
    arrange(AI_Type, Dimension) %>%
    group_by(AI_Type) %>%
    mutate(
      angle = angles,
      # Normalize scores for better visualization
      Score_normalized = (Mean_Score - min(engagement_grouped$Mean_Score)) / 
        (max(engagement_grouped$Mean_Score) - min(engagement_grouped$Mean_Score)) * 5 + 1,
      x = Score_normalized * cos(angle),
      y = Score_normalized * sin(angle)
    ) %>%
    ungroup()
  
  # Simplified radar plot colors
  radar_colors_simple <- c("Single_AI" = "#1f77b4", "Dual_AI" = "#ff7f0e")
  
  radar_plot_grouped <- ggplot(engagement_radar_grouped, 
                               aes(x = x, y = y, color = AI_Type, fill = AI_Type, group = AI_Type)) +
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
             xend = 7 * cos(angles), yend = 7 * sin(angles),
             color = "gray60", size = 0.3) +
    
    # Add the radar polygon
    geom_polygon(alpha = 0.3, size = 1.2) +
    geom_point(size = 3, alpha = 0.8) +
    
    # Add dimension labels
    annotate("text", x = 7.5 * cos(angles), y = 7.5 * sin(angles),
             label = unique(engagement_grouped$Dimension),
             hjust = 0.5, vjust = 0.5,
             size = 3.2, family = "Avenir") +
    
    scale_color_manual(values = radar_colors_simple) +
    scale_fill_manual(values = radar_colors_simple) +
    coord_fixed() +
    xlim(-9, 9) + ylim(-9, 9) +
    labs(title = "Engagement Profiles: Single AI vs Dual AI",
         color = "AI Type", fill = "AI Type") +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.text = element_text(family = "Avenir", size = 8),
      plot.title = element_text(family = "Avenir", size = 10, hjust = 0.5),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  print(radar_plot_grouped)
  
  # OPTION E: Full radar plot with all 7 categories (may be crowded)
  engagement_radar_full <- engagement_means %>%
    arrange(ExperimentType, Dimension) %>%
    group_by(ExperimentType) %>%
    mutate(
      angle = rep(angles, length(unique(engagement_means$ExperimentType)))[1:n()],
      Score_normalized = (Mean_Score - min(engagement_means$Mean_Score)) / 
        (max(engagement_means$Mean_Score) - min(engagement_means$Mean_Score)) * 4 + 1,
      x = Score_normalized * cos(angle),
      y = Score_normalized * sin(angle)
    ) %>%
    ungroup()
  
  radar_plot_full <- ggplot(engagement_radar_full, 
                            aes(x = x, y = y, color = ExperimentType, 
                                fill = ExperimentType, group = ExperimentType)) +
    # Add concentric circles and axes (same as above)
    annotate("path", 
             x = 2 * cos(seq(0, 2*pi, length.out = 100)), 
             y = 2 * sin(seq(0, 2*pi, length.out = 100)),
             color = "gray80", size = 0.3) +
    annotate("path", 
             x = 4 * cos(seq(0, 2*pi, length.out = 100)), 
             y = 4 * sin(seq(0, 2*pi, length.out = 100)),
             color = "gray80", size = 0.3) +
    annotate("segment", x = 0, y = 0, 
             xend = 6 * cos(angles), yend = 6 * sin(angles),
             color = "gray60", size = 0.3) +
    
    # Add polygons with transparency
    geom_polygon(alpha = 0.15, size = 0.8) +
    geom_point(size = 2, alpha = 0.7) +
    
    # Add dimension labels
    annotate("text", x = 6.5 * cos(angles), y = 6.5 * sin(angles),
             label = unique(engagement_means$Dimension),
             hjust = 0.5, vjust = 0.5,
             size = 3, family = "Avenir") +
    
    scale_color_manual(values = experiment_colors) +
    scale_fill_manual(values = experiment_colors) +
    coord_fixed() +
    xlim(-8, 8) + ylim(-8, 8) +
    labs(title = "Engagement Profiles: All Experiment Types",
         color = "Experiment Type", fill = "Experiment Type") +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.text = element_text(family = "Avenir", size = 7),
      legend.title = element_text(family = "Avenir", size = 8),
      plot.title = element_text(family = "Avenir", size = 10, hjust = 0.5),
      plot.margin = margin(10, 10, 10, 10)
    ) +
    guides(color = guide_legend(nrow = 2, override.aes = list(alpha = 1)),
           fill = guide_legend(nrow = 2, override.aes = list(alpha = 1)))
  
  print(radar_plot_full)
}