# Define columns to keep for both dataframes
# NOTE: BiasedType dropped — single_ai_processed does not have it, and it is not
# used downstream. The Biased/Default split is derived from AIStanceLabel_S below.
columns_to_keep_1 <- c("PerceivedImproveCode", "PostPerformance", "PrePerformance",
                       "NID", "UStanceLabel", "AIStanceLabel_S", "UID",
                       "UStanceLabel_S", "ConvLength", "PoliBias", "AICorrectness",
                       "PostCorrect", "PreCorrect", "PostConfCode", "PreConfCode",
                       "AIInterMean", "UIdeo", "EngagementBehavioral",
                       "EngagementCognitive", "EngagementEmotional",
                       "EngagementAutonomy", "EngagementSocialPresence",
                       "PerceivedAIRole")

# NOTE: AI_Combo_Numeric dropped — it is not produced by any R preprocessing (it came
# from a notebook-enriched df2) and is unused here; the dual split uses the stance codes.
columns_to_keep_2 <- c("PerceivedImproveCode", "PostPerformance", "PrePerformance",
                       "NID", "UStanceLabel", "UID",
                       "AI1StanceCode", "AI2StanceCode", "UStanceCode", "ConvLength",
                       "PoliBias", "AI1StanceLabel_S", "AI2StanceLabel_S",
                       "AI1Correctness", "AI2Correctness", "AIInterMean",
                       "PostCorrect", "PreCorrect", "PostConfCode", "PreConfCode", "UIdeo",
                       "EngagementBehavioral",
                       "EngagementCognitive", "EngagementEmotional",
                       "EngagementAutonomy", "EngagementSocialPresence",
                       "PerceivedAIRole")

# 1. Process single_ai_processed data
single_ai_data <- single_ai_processed %>%
  dplyr::select(all_of(unique(columns_to_keep_1))) %>%
  mutate(
    ExperimentType = case_when(
      # Default cases
      AIStanceLabel_S == "Default" ~ "Single_AI_Non_Biased",
      # Biased = any partisan single-AI arm. Derived from AIStanceLabel_S because
      # single_ai_processed has no BiasedType (and BiasedType is now Default/Rep/Dem).
      AIStanceLabel_S %in% c("Republican", "Democrat") ~ "Single_AI_Biased",
      # AIStanceLabel_S == "Neutral" ~ "Single_AI_Non_Biased_Exp",
      # Balanced: User and AI on opposite political sides
      # (UStanceLabel_S == "Democrat" & AIStanceLabel_S == "Republican") |
      #   (UStanceLabel_S == "Republican" & AIStanceLabel_S == "Democrat") ~ "Single_AI_Opposition",
      # All other cases
      TRUE ~ "Single_AI_Other"
    )
  )

single_ai_data <- single_ai_data[single_ai_data$ExperimentType != "Single_AI_Other", ]
single_ai_data$AI1Correctness <- single_ai_data$AICorrectness
single_ai_data$AI2Correctness <- single_ai_data$AICorrectness

# Build the enriched dual-AI frame from df2 with the dual preprocessing function
# (parallel to single_ai_processed <- process_single_ai_data(df1)). This adds
# PostPerformance/PrePerformance, AI1/AI2StanceCode, UStanceCode, AI1/AI2StanceLabel_S.
# Assumes df2 already carries the engagement/conversation columns, as df1 does.
df2_filled <- process_dual_ai_data(df2)

# 2. Process df2 data
dual_ai_data <- df2_filled %>%
  dplyr::select(all_of(unique(columns_to_keep_2))) %>%
  mutate(
    ExperimentType = case_when(
      # Default cases
      AI1StanceLabel_S == "Default" & AI2StanceLabel_S == "Default" ~ "Dual_AI_Non_Biased",
      # AI1StanceLabel_S == "Neutral" & AI2StanceLabel_S == "Neutral" ~ "Dual_AI_Non_Biased_Exp",
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
# AIStanceLabel_S exists only on the single side (a text label used above); the dual
# side no longer carries it, so bind_rows just fills NA for dual rows. Not used
# downstream -- the models/plots key off ExperimentType.
combined_data <- bind_rows(
  single_ai_data,
  dual_ai_data
)

combined_data$PerceivedImproveCode <- as.numeric(combined_data$PerceivedImproveCode)

# 4. Convert ExperimentType to factor for regression analysis
combined_data$ExperimentType <- factor(combined_data$ExperimentType, 
                                       levels = c("Single_AI_Non_Biased",
                                                  # "Single_AI_Non_Biased_Exp",
                                                  # "Single_AI_Opposition",
                                                  "Single_AI_Biased",
                                                  "Dual_AI_Non_Biased",
                                                  # "Dual_AI_Non_Biased_Exp",
                                                  "Dual_AI_Opposition",
                                                  "Dual_AI_Balanced"))


# 6.1 Run the OLS + clustered SD model
model_full <- lm(
  PostPerformance ~ ExperimentType + PrePerformance +
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

# 6. Run the mixed-effects model
model_full_mixed <- lmer(
  PostPerformance ~ ExperimentType + PrePerformance +
    as.factor(NID) + as.factor(UStanceLabel) + 
    (1 | UID),
  data = combined_data,
  na.action = na.omit
)

summary(model_full_mixed)
summary(model_full_mixed)$sigma
df.residual(model_full_mixed)
performance::r2(model_full_mixed)   # namespaced: fixest::r2 masks performance::r2

# emmeans df limits: with a few thousand observations the defaults (3000) disable df
# computation, which renames CI columns to asymp.LCL/asymp.UCL and the test stat to
# z.ratio -- breaking the rename()s below. Raise the limits (Satterthwaite = fast, finite df).
emm_options(lmer.df = "satterthwaite", lmerTest.limit = 20000, pbkrtest.limit = 20000)

# Get estimated marginal means for ExperimentType
# emmeans automatically averages over the other covariates in the model
emm_results <- emmeans(model_full_mixed, ~ ExperimentType)

# Convert to data frame for plotting
plot_data <- as.data.frame(emm_results, lmerTest.limit = 20000)

# Rename columns to match plotting expectations and add treatment names
plot_data <- plot_data %>%
  rename(estimate = emmean,
         std_error = SE,
         lower_ci = lower.CL,
         upper_ci = upper.CL) %>%
  mutate(treatments = as.character(ExperimentType))

# Create formal labels
plot_data$formal_label <- case_when(
  plot_data$treatments == "Single_AI_Non_Biased" ~ "Single AI\nDefault",
  plot_data$treatments == "Single_AI_Biased" ~ "Single AI\nBiased",
  # plot_data$treatments == "Single_AI_Non_Biased_Exp" ~ "Single AI\nNeutralized",
  # plot_data$treatments == "Single_AI_Opposition" ~ "Single AI\nOpposition",
  plot_data$treatments == "Dual_AI_Non_Biased" ~ "Dual AI\nDefault",
  # plot_data$treatments == "Dual_AI_Non_Biased_Exp" ~ "Dual AI\nNeutralized",
  plot_data$treatments == "Dual_AI_Opposition" ~ "Dual AI\nOpposition",
  plot_data$treatments == "Dual_AI_Balanced" ~ "Dual AI\nBalanced",
  TRUE ~ as.character(plot_data$treatments)
)

plot_data$formal_label <- factor(plot_data$formal_label, 
                                 levels = c("Single AI\nDefault",
                                            "Single AI\nBiased",
                                            # "Single AI\nNeutralized",
                                            # "Single AI\nOpposition",
                                            "Dual AI\nDefault",
                                            # "Dual AI\nNeutralized",
                                            "Dual AI\nOpposition",
                                            "Dual AI\nBalanced"))

# =====================================
# Value-biased color mapping
# =====================================
# Define color palette
color_actual <- "#006400"      # Dark green
color_actual_90 <- "#228B22"   # Forest green
color_actual_95 <- "#66C266"   # Light forest green
color_actual_99 <- "#BBE5BB"   # Soft mint green

# Create color gradient based on values
min_val <- min(plot_data$estimate)
max_val <- max(plot_data$estimate)

# Normalize values to 0-1 range
plot_data$normalized_value <- (plot_data$estimate - min_val) / (max_val - min_val)

# Create color mapping function
get_color_for_value <- function(norm_val) {
  colors <- c(color_actual_99, color_actual_95, color_actual_90, color_actual)
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

# Calculate appropriate y-axis limits
y_min <- min(plot_data$lower_ci) * 0.95
y_max <- max(plot_data$upper_ci) * 1.05

# =====================================
# Create plot
# =====================================
p_vertical <- ggplot(plot_data, aes(x = formal_label, y = estimate)) +
  geom_col(aes(fill = fill_color), color = "black", width = 0.65, linewidth = 0) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), 
                width = 0.3, linewidth = 0.5, color = "black") +
  
  # Add value labels at the top of each bar
  geom_text(aes(y = upper_ci + (y_max - y_min) * 0.05, label = round(estimate, 3)), 
            vjust = 0, family = "Avenir", size = 3, color = "black") +
  geom_point(data = plot_data, 
             aes(x = formal_label, y = estimate), 
             color = "black", size = 3., shape = 20) +
  # Use manual fill scale
  scale_fill_identity() +
  
  # Set y-axis
  scale_y_continuous(
    expand = c(0, 0), 
    labels = scales::number_format(accuracy = 0.01)
  ) +
  
  # Use coord_cartesian to set limits
  coord_cartesian(ylim = c(0.55, 0.75)) +
  
  labs(x = "Experimental Condition", 
       y = "Post-Interaction Performance") +
  
  theme_classic() +
  theme(
    text = element_text(family = "Avenir", color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.line = element_blank(),
    axis.text.x = element_text(family = "Avenir", size = 9, color = "black", 
                               margin = margin(t = 8), angle = 0),
    axis.text.y = element_text(family = "Avenir", size = 9, color = "black"),
    axis.title.x = element_text(family = "Avenir", size = 12, color = "black", 
                                margin = margin(t = 12)),
    axis.title.y = element_text(family = "Avenir", size = 12, color = "black", 
                                margin = margin(r = 15)),
    axis.ticks = element_line(color = "black", linewidth = 0.4),
    axis.ticks.length = unit(3, "pt"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    plot.margin = margin(t = 15, r = 30, b = 15, l = 15),
    legend.position = "none"
  )

print(p_vertical)

# Calculate appropriate x-axis limits
x_min <- min(plot_data$lower_ci) * 0.95
x_max <- max(plot_data$upper_ci) * 1.05

# =====================================
# Create horizontal plot
# =====================================
p_horizontal <- ggplot(plot_data, aes(y = formal_label, x = estimate)) +
  geom_col(aes(fill = fill_color), color = "black", width = 0.65, linewidth = 0) +
  geom_errorbar(aes(xmin = lower_ci, xmax = upper_ci), 
                width = 0.3, linewidth = 0.5, color = "black") +
  
  # Add value labels to the right of each bar
  geom_text(aes(x = upper_ci + (x_max - x_min) * 0.05, label = round(estimate, 3)), 
            hjust = 0, family = "Avenir", size = 3, color = "black") +
  geom_point(data = plot_data, 
             aes(y = formal_label, x = estimate), 
             color = "black", size = 3., shape = 20) +
  
  # Use manual fill scale
  scale_fill_identity() +
  
  # Reverse the order of bars on y-axis
  scale_y_discrete(limits = rev) +
  # Set x-axis
  scale_x_continuous(
    expand = c(0, 0), 
    labels = scales::number_format(accuracy = 0.01)
  ) +
  
  # Use coord_cartesian to set limits (horizontal now)
  coord_cartesian(xlim = c(0.55, 0.85)) +
  
  labs(y = "Experimental Condition", 
       x = "Post-Interaction Performance") +
  
  theme_classic() +
  theme(
    text = element_text(family = "Avenir", color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.line = element_blank(),
    # axis.text.y = element_text(family = "Avenir", size = 9, color = "black", 
    #                            margin = margin(r = 8)),
    axis.text.y = element_blank(),
    axis.text.x = element_text(family = "Avenir", size = 9, color = "black"),
    # axis.title.y = element_text(family = "Avenir", size = 12, color = "black", 
    #                             margin = margin(r = 15)),
    axis.title.y = element_blank(),
    axis.title.x = element_text(family = "Avenir", size = 12, color = "black", 
                                margin = margin(t = 12)),
    axis.ticks = element_line(color = "black", linewidth = 0.4),
    axis.ticks.length = unit(3, "pt"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    plot.margin = margin(t = 15, r = 30, b = 15, l = 15),
    legend.position = "none"
  )

print(p_horizontal)

# =================================
# Comparison analysis
# =================================
emm_options(pbkrtest.limit = 20000)

# Get estimated marginal means for ExperimentType
emm_results <- emmeans(model_full_mixed, ~ ExperimentType)

# Perform all pairwise comparisons with FDR adjustment
pairwise_comparisons <- pairs(emm_results, adjust = "fdr")

# Get estimated marginal means for ExperimentType (if not already computed)
emm_results <- emmeans(model_full_mixed, ~ ExperimentType)

# Perform all pairwise comparisons with FDR adjustment
pairwise_comparisons <- pairs(emm_results, adjust = "fdr", infer = TRUE)

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
# Detalized results table
# =====================================

cat("\n=== DETAILED RESULTS WITH CONFIDENCE INTERVALS ===\n")
detailed_results <- comparisons %>%
  dplyr::select(Comparison, Difference, SE, p_adj, Hedges_g, Hedges_g_Lower, Hedges_g_Upper, Significance) %>%
  arrange(p_adj)

print(detailed_results)
