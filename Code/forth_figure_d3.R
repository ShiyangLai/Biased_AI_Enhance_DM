# Fit mixed effects model with random effects for participants
bias_model <- lmer(PostPerformance ~ PrePerformance + ExperimentType * PoliBias + 
                     as.factor(UStanceLabel) + as.factor(NID) +
                     (1 | UID), 
                   data = combined_data)

# Get emmeans with the mixed model
emm_by_group <- emmeans(bias_model, ~ ExperimentType * PoliBias)

performance_by_group <- as.data.frame(emm_by_group) %>%
  rename(MeanImprovement = emmean) %>%
  mutate(Count = NA) # We'll add counts separately

counts <- combined_data %>%
  group_by(ExperimentType, PoliBias) %>%
  summarise(Count = n(), .groups = 'drop')

performance_by_group <- performance_by_group %>%
  left_join(counts, by = c("ExperimentType", "PoliBias"))

emm_by_bias_type <- emmeans(bias_model, ~ PoliBias | ExperimentType)

# Calculate gaps with proper mixed model SEs
rep_vs_dem_gap <- contrast(emm_by_bias_type, list("Rep-Dem" = c(-1, 0, 1)), by = "ExperimentType")
rep_vs_neutral_gap <- contrast(emm_by_bias_type, list("Rep-Neutral" = c(0, -1, 1)), by = "ExperimentType")
dem_vs_neutral_gap <- contrast(emm_by_bias_type, list("Dem-Neutral" = c(1, -1, 0)), by = "ExperimentType")

# Convert gap results to dataframe format
rep_vs_dem_df <- as.data.frame(rep_vs_dem_gap)
rep_vs_neutral_df <- as.data.frame(rep_vs_neutral_gap)
dem_vs_neutral_df <- as.data.frame(dem_vs_neutral_gap)

# Create gaps summary dataframe
gaps_df <- data.frame(
  ExperimentType = rep_vs_dem_df$ExperimentType,
  Rep_vs_Dem_Gap = rep_vs_dem_df$estimate,
  Rep_vs_Dem_SE = rep_vs_dem_df$SE,
  Rep_vs_Dem_pvalue = rep_vs_dem_df$p.value,
  Rep_vs_Neutral_Gap = rep_vs_neutral_df$estimate,
  Rep_vs_Neutral_SE = rep_vs_neutral_df$SE,
  Rep_vs_Neutral_pvalue = rep_vs_neutral_df$p.value,
  Dem_vs_Neutral_Gap = dem_vs_neutral_df$estimate,
  Dem_vs_Neutral_SE = dem_vs_neutral_df$SE,
  Dem_vs_Neutral_pvalue = dem_vs_neutral_df$p.value
) %>%
  mutate(
    Overall_Bias_Magnitude = abs(Rep_vs_Dem_Gap)
  )

# Also create the wide format for compatibility
bias_gaps_wide <- performance_by_group %>%
  dplyr::select(ExperimentType, PoliBias, MeanImprovement) %>%
  pivot_wider(names_from = PoliBias, values_from = MeanImprovement) %>%
  left_join(gaps_df, by = "ExperimentType")

# Pairwise comparisons within PoliBias groups
pairwise_within_polibias <- pairs(emm_by_bias_type, by = "PoliBias")

# Calculate Average Absolute Political Bias metric
avg_abs_bias <- gaps_df %>%
  mutate(
    # Calculate average absolute political bias across all three comparisons
    Avg_Abs_Political_Bias = (abs(Rep_vs_Dem_Gap) + abs(Rep_vs_Neutral_Gap) + abs(Dem_vs_Neutral_Gap)) / 3,
    
    # More appropriate SE calculation for mixed models
    Avg_Abs_SE = sqrt((Rep_vs_Dem_SE^2 + Rep_vs_Neutral_SE^2 + Dem_vs_Neutral_SE^2) / 3),
    
    # Numeric position for plotting
    bias_side_numeric = case_when(
      ExperimentType == "Single_AI_Non_Biased" ~ 1,
      ExperimentType == "Single_AI_Biased" ~ 2,
      # ExperimentType == "Single_AI_Opposition" ~ 3,
      ExperimentType == "Dual_AI_Non_Biased" ~ 3,
      # ExperimentType == "Dual_AI_Non_Biased_Exp" ~ 5,
      ExperimentType == "Dual_AI_Opposition" ~ 4,
      ExperimentType == "Dual_AI_Balanced" ~ 5,
      TRUE ~ NA_real_
    ),
    
    # Calculate degrees of freedom for mixed model
    df = length(unique(combined_data$UID)) - 1
  )

# =====================================
# Visualization
# =====================================

avg_abs_bias$formal_label <- case_when(
  avg_abs_bias$ExperimentType == "Single_AI_Non_Biased" ~ "Single AI\nNon-Biased",
  avg_abs_bias$ExperimentType == "Single_AI_Biased" ~ "Single AI\nBiased",
  # avg_abs_bias$ExperimentType == "Single_AI_Non_Biased_Exp" ~ "Single AI\nNeutralized",
  # avg_abs_bias$ExperimentType == "Single_AI_Opposition" ~ "Single AI\nOpposition",
  avg_abs_bias$ExperimentType == "Dual_AI_Non_Biased" ~ "Dual AI\nNon-Biased",
  # avg_abs_bias$ExperimentType == "Dual_AI_Non_Biased_Exp" ~ "Dual AI\nNeutralized",
  avg_abs_bias$ExperimentType == "Dual_AI_Balanced" ~ "Dual AI\nBalanced",
  avg_abs_bias$ExperimentType == "Dual_AI_Opposition" ~ "Dual AI\nOpposition",
  TRUE ~ as.character(avg_abs_bias$ExperimentType)
)

# Order the labels (reverse for horizontal plot)
unique_labels <- unique(avg_abs_bias$formal_label)
avg_abs_bias$formal_label <- factor(avg_abs_bias$formal_label, levels = rev(unique_labels))

# Define color palette
base_color <- "#535e3c"  
color_90 <- "#747d63"      
color_95 <- "#909683"      
color_99 <- "#d0d4c8"

# Create color gradient based on bias values
min_val <- min(avg_abs_bias$Avg_Abs_Political_Bias)
max_val <- max(avg_abs_bias$Avg_Abs_Political_Bias)
avg_abs_bias$normalized_value <- (avg_abs_bias$Avg_Abs_Political_Bias - min_val) / (max_val - min_val)

# Color mapping function
get_color_for_bias <- function(norm_val) {
  colors <- c(color_99, color_95, color_90, base_color)
  breaks <- c(0, 0.33, 0.66, 1.0)
  
  if (norm_val <= breaks[2]) {
    ratio <- norm_val / breaks[2]
    colorRampPalette(c(colors[1], colors[2]))(100)[round(ratio * 99) + 1]
  } else if (norm_val <= breaks[3]) {
    ratio <- (norm_val - breaks[2]) / (breaks[3] - breaks[2])
    colorRampPalette(c(colors[2], colors[3]))(100)[round(ratio * 99) + 1]
  } else {
    ratio <- (norm_val - breaks[3]) / (breaks[4] - breaks[3])
    colorRampPalette(c(colors[3], colors[4]))(100)[round(ratio * 99) + 1]
  }
}

avg_abs_bias$fill_color <- sapply(avg_abs_bias$normalized_value, get_color_for_bias)

# Calculate confidence intervals (using t-distribution)
alpha <- 0.05
t_critical <- qt(1 - alpha/2, df = avg_abs_bias$df[1])
avg_abs_bias$bias_LCL <- avg_abs_bias$Avg_Abs_Political_Bias - t_critical * avg_abs_bias$Avg_Abs_SE
avg_abs_bias$bias_UCL <- avg_abs_bias$Avg_Abs_Political_Bias + t_critical * avg_abs_bias$Avg_Abs_SE

# Calculate x-axis limits
x_min <- min(avg_abs_bias$bias_LCL) * 0.95
x_max <- max(avg_abs_bias$bias_UCL) * 1.05

# Create horizontal bar plot for political bias
p_bias <- ggplot(avg_abs_bias, aes(y = formal_label, x = Avg_Abs_Political_Bias)) +
  geom_col(aes(fill = fill_color), color = "black", width = 0.7, linewidth = 0) +
  geom_errorbar(aes(xmin = bias_LCL, xmax = bias_UCL), 
                width = 0.3, linewidth = 0.5, color = "black") +
  
  # Add value labels
  geom_text(aes(x = bias_UCL + (x_max - x_min) * 0.05, 
                label = round(Avg_Abs_Political_Bias, 3)), 
            hjust = 0, family = "Avenir", size = 3, color = "black") +
  
  # Add points
  geom_point(aes(y = formal_label, x = Avg_Abs_Political_Bias), 
             color = "black", size = 2, shape = 15) +
  
  # Use manual fill scale
  scale_fill_identity() +
  
  # Set x-axis
  scale_x_continuous(
    expand = c(0, 0), 
    labels = scales::number_format(accuracy = 0.01)
  ) +
  
  coord_cartesian(xlim = c(0, 0.5)) +
  
  labs(y = "Experimental Condition", 
       x = "Post-Interaction Performance Gap\nAcross Rep/Neu/Dem News") +
  
  theme_classic() +
  theme(
    text = element_text(family = "Avenir", color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.line = element_blank(),
    # axis.text.y = element_text(family = "Avenir", size = 9, color = "black",
    #                            margin = margin(r = 5)),
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

print(p_bias)


# =============================================================================
# OLS with robust standard errors bias analysis
# =============================================================================

# Step 1: Fit OLS model
model <- lm(PostPerformance ~ PrePerformance + 
              ExperimentType * PoliBias + 
              as.factor(NID) + UStanceLabel, 
            data = combined_data, na.action = na.omit)
summary(model)

# Step 2: Calculate cluster-robust standard errors
robust_vcov <- vcovCL(model, cluster = combined_data$UID)

# Step 3: Get emmeans with robust SEs
emm_by_biased_polibias <- emmeans(model, ~ ExperimentType | PoliBias, vcov = robust_vcov,
                                  rg.limit = 20000)
emm_summary <- as.data.frame(emm_by_biased_polibias)

print("=== EMMEANS BY GROUP ===")
print(emm_summary)

# Step 4: Calculate bias scores for each BiasedType
calculate_bias_scores <- function(emm_data) {
  bias_scores <- emm_data %>%
    group_by(ExperimentType) %>%
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
    dplyr::select(ExperimentType, BiasScore, BiasScore_SE, n_groups) %>%
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

# Step 5: Multiple t-tests comparing all groups with Single_AI_Non_Biased (FDR corrected)

# Define reference group
reference_group <- "Single_AI_Non_Biased"

# Get all other groups
other_groups <- bias_scores$ExperimentType[bias_scores$ExperimentType != reference_group]

# Check if reference group exists
ref_idx <- which(bias_scores$ExperimentType == reference_group)

if(length(ref_idx) != 1) {
  cat("ERROR: Reference group 'Single_AI_Non_Biased' not found or duplicated\n")
  cat("Available groups:\n")
  print(bias_scores$ExperimentType)
} else if(length(other_groups) == 0) {
  cat("ERROR: No other groups found for comparison\n")
} else {
  
  # Get reference group data
  ref_bias <- bias_scores$BiasScore[ref_idx]
  ref_se <- bias_scores$BiasScore_SE[ref_idx]
  
  # Initialize storage for results
  results <- data.frame(
    Comparison = character(),
    Group_BiasScore = numeric(),
    Group_SE = numeric(),
    Difference = numeric(),
    Diff_SE = numeric(),
    t_stat = numeric(),
    p_value = numeric(),
    CI_lower = numeric(),
    CI_upper = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Perform all comparisons
  cat(sprintf("\n=== MULTIPLE COMPARISONS: ALL GROUPS vs %s ===\n", reference_group))
  cat(sprintf("Reference group (%s): %.4f (SE = %.4f)\n\n", reference_group, ref_bias, ref_se))
  
  for(group in other_groups) {
    
    # Get comparison group data
    comp_idx <- which(bias_scores$ExperimentType == group)
    
    if(length(comp_idx) == 1) {
      comp_bias <- bias_scores$BiasScore[comp_idx]
      comp_se <- bias_scores$BiasScore_SE[comp_idx]
      
      # Calculate difference (comparison group - reference group)
      bias_difference <- comp_bias - ref_bias
      
      # Calculate SE for the difference
      diff_se <- sqrt(comp_se^2 + ref_se^2)
      
      # Calculate t-statistic
      t_stat <- bias_difference / diff_se
      
      # Use model df as approximation
      df <- df.residual(model)
      p_value <- 2 * (1 - pt(abs(t_stat), df = df))
      
      # Calculate confidence intervals (uncorrected)
      ci_lower <- bias_difference - qt(0.975, df) * diff_se
      ci_upper <- bias_difference + qt(0.975, df) * diff_se
      
      # Store results
      results <- rbind(results, data.frame(
        Comparison = paste(group, "vs", reference_group),
        Group_BiasScore = comp_bias,
        Group_SE = comp_se,
        Difference = bias_difference,
        Diff_SE = diff_se,
        t_stat = t_stat,
        p_value = p_value,
        CI_lower = ci_lower,
        CI_upper = ci_upper,
        stringsAsFactors = FALSE
      ))
      
    } else {
      cat(sprintf("WARNING: Group '%s' not found or duplicated\n", group))
    }
  }
  
  # Apply FDR correction
  if(nrow(results) > 0) {
    results$p_value_fdr <- p.adjust(results$p_value, method = "fdr")
    
    # Print detailed results
    cat("=== DETAILED RESULTS (before FDR correction) ===\n")
    for(i in 1:nrow(results)) {
      cat(sprintf("\n%s:\n", results$Comparison[i]))
      cat(sprintf("  Group bias score: %.4f (SE = %.4f)\n", results$Group_BiasScore[i], results$Group_SE[i]))
      cat(sprintf("  Difference: %.4f\n", results$Difference[i]))
      cat(sprintf("  SE of difference: %.4f\n", results$Diff_SE[i]))
      cat(sprintf("  t-statistic: %.4f\n", results$t_stat[i]))
      cat(sprintf("  p-value (uncorrected): %.4f\n", results$p_value[i]))
      cat(sprintf("  95%% CI: [%.4f, %.4f]\n", results$CI_lower[i], results$CI_upper[i]))
    }
    
    # Print FDR corrected results
    cat("\n\n=== FDR CORRECTED RESULTS ===\n")
    cat(sprintf("Number of comparisons: %d\n", nrow(results)))
    cat(sprintf("FDR correction applied using Benjamini-Hochberg method\n\n"))
    
    # Sort by FDR-corrected p-value
    results_sorted <- results[order(results$p_value_fdr), ]
    
    cat("Results sorted by FDR-corrected p-value:\n")
    for(i in 1:nrow(results_sorted)) {
      significance <- ifelse(results_sorted$p_value_fdr[i] < 0.05, "***", "")
      direction <- ifelse(results_sorted$Difference[i] > 0, "HIGHER", "LOWER")
      
      cat(sprintf("%d. %s %s\n", i, results_sorted$Comparison[i], significance))
      cat(sprintf("   Difference: %.4f (Group is %.4f %s than reference)\n", 
                  results_sorted$Difference[i], abs(results_sorted$Difference[i]), direction))
      cat(sprintf("   p-value (uncorrected): %.4f\n", results_sorted$p_value[i]))
      cat(sprintf("   p-value (FDR): %.4f\n", results_sorted$p_value_fdr[i]))
      
      # Effect size
      effect_size <- abs(results_sorted$Difference[i]) / sqrt((results_sorted$Group_SE[i]^2 + ref_se^2)/2)
      cat(sprintf("   Effect size: %.3f ", effect_size))
      if(effect_size < 0.2) {
        cat("(Small)\n")
      } else if(effect_size < 0.5) {
        cat("(Small to Medium)\n") 
      } else if(effect_size < 0.8) {
        cat("(Medium to Large)\n")
      } else {
        cat("(Large)\n")
      }
      cat("\n")
    }
    
    # Summary of significant results
    significant_results <- results_sorted[results_sorted$p_value_fdr < 0.05, ]
    
    cat("=== SUMMARY ===\n")
    if(nrow(significant_results) > 0) {
      cat(sprintf("*** %d out of %d comparisons are significant after FDR correction (α = 0.05) ***\n\n", 
                  nrow(significant_results), nrow(results)))
      
      for(i in 1:nrow(significant_results)) {
        direction <- ifelse(significant_results$Difference[i] > 0, "significantly HIGHER", "significantly LOWER")
        cat(sprintf("• %s has %s bias than %s (p_FDR = %.4f)\n", 
                    gsub(" vs.*", "", significant_results$Comparison[i]), 
                    direction, reference_group, significant_results$p_value_fdr[i]))
      }
    } else {
      cat("*** No comparisons are significant after FDR correction ***\n")
      cat("This suggests no meaningful differences between groups and the reference condition.\n")
    }
    
    # Create results table for export
    cat("\n=== RESULTS TABLE ===\n")
    results_table <- results_sorted[, c("Comparison", "Group_BiasScore", "Group_SE", "Difference", "Diff_SE", "p_value", "p_value_fdr")]
    names(results_table) <- c("Comparison", "Group_BiasScore", "Group_SE", "Difference", "Diff_SE", "p_uncorrected", "p_FDR")
    print(results_table, row.names = FALSE, digits = 4)
    
  } else {
    cat("ERROR: No valid comparisons could be performed\n")
  }
}


# =============================================================================
# Mixed Effects Model with random intercept for UID bias analysis
# =============================================================================

# Step 1: Fit mixed effects model with random intercept for UID
model <- lmer(PostPerformance ~ PrePerformance + C(NID) + AICorrectness + UIdeo +
                ExperimentType * PoliBias + UStanceLabel + (1|UID), 
              data = combined_data, na.action = na.omit)
summary(model)
summary(model)$sigma
df.residual(model)
r2(model)

# Step 2: Get emmeans (no need for robust SEs as clustering handled by random effects)
emm_by_biased_polibias <- emmeans(model, ~ ExperimentType | PoliBias,
                                  rg.limit = 20000)
emm_summary <- as.data.frame(emm_by_biased_polibias)

print("=== EMMEANS BY GROUP ===")
print(emm_summary)

# Step 3: Calculate bias scores for each BiasedType
calculate_bias_scores <- function(emm_data) {
  bias_scores <- emm_data %>%
    group_by(ExperimentType) %>%
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
    dplyr::select(ExperimentType, BiasScore, BiasScore_SE, n_groups) %>%
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


# Step 4: Multiple t-tests comparing all groups with Single_AI_Non_Biased (FDR corrected)

# Define reference group
reference_group <- "Single_AI_Non_Biased"

# Get all other groups
other_groups <- bias_scores$ExperimentType[bias_scores$ExperimentType != reference_group]

# Check if reference group exists
ref_idx <- which(bias_scores$ExperimentType == reference_group)

if(length(ref_idx) != 1) {
  cat("ERROR: Reference group 'Single_AI_Non_Biased' not found or duplicated\n")
  cat("Available groups:\n")
  print(bias_scores$ExperimentType)
} else if(length(other_groups) == 0) {
  cat("ERROR: No other groups found for comparison\n")
} else {
  
  # Get reference group data
  ref_bias <- bias_scores$BiasScore[ref_idx]
  ref_se <- bias_scores$BiasScore_SE[ref_idx]
  
  # Initialize storage for results
  results <- data.frame(
    Comparison = character(),
    Group_BiasScore = numeric(),
    Group_SE = numeric(),
    Difference = numeric(),
    Diff_SE = numeric(),
    t_stat = numeric(),
    p_value = numeric(),
    CI_lower = numeric(),
    CI_upper = numeric(),
    Hedges_g_bias = numeric(),
    Hedges_g_raw = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Perform all comparisons
  cat(sprintf("\n=== MULTIPLE COMPARISONS: ALL GROUPS vs %s ===\n", reference_group))
  cat(sprintf("Reference group (%s): %.4f (SE = %.4f)\n\n", reference_group, ref_bias, ref_se))
  
  for(group in other_groups) {
    
    # Get comparison group data
    comp_idx <- which(bias_scores$ExperimentType == group)
    
    if(length(comp_idx) == 1) {
      comp_bias <- bias_scores$BiasScore[comp_idx]
      comp_se <- bias_scores$BiasScore_SE[comp_idx]
      
      # Calculate difference (comparison group - reference group)
      bias_difference <- comp_bias - ref_bias
      
      # Calculate SE for the difference
      diff_se <- sqrt(comp_se^2 + ref_se^2)
      
      # Calculate t-statistic
      t_stat <- bias_difference / diff_se
      
      # Use Satterthwaite approximation for degrees of freedom in mixed models
      # Alternative: use a large df approximation if Satterthwaite not available
      tryCatch({
        # Try to get more accurate df using lmerTest package if available
        if(requireNamespace("lmerTest", quietly = TRUE)) {
          # Use large df approximation as conservative approach
          df <- 1000
        } else {
          # Use large df approximation
          df <- 1000
        }
      }, error = function(e) {
        df <- 1000  # Conservative large df approximation
      })
      
      p_value <- 2 * (1 - pt(abs(t_stat), df = df))
      
      # Calculate confidence intervals (uncorrected)
      ci_lower <- bias_difference - qt(0.975, df) * diff_se
      ci_upper <- bias_difference + qt(0.975, df) * diff_se
      
      # Calculate Hedges' g effect sizes
      # Method 1: Using emmeans SEs (for bias score differences)
      pooled_se <- sqrt((comp_se^2 + ref_se^2)/2)
      cohens_d <- abs(bias_difference) / pooled_se
      j_correction <- 1 - (3 / (4 * df - 1))
      hedges_g_bias <- cohens_d * j_correction
      
      # Method 2: Using raw data
      hedges_g_raw <- NA
      tryCatch({
        group_data <- combined_data[combined_data$ExperimentType == group & !is.na(combined_data$PostPerformance), ]
        ref_data <- combined_data[combined_data$ExperimentType == reference_group & !is.na(combined_data$PostPerformance), ]
        
        if(nrow(group_data) > 1 && nrow(ref_data) > 1) {
          m1 <- mean(group_data$PostPerformance, na.rm = TRUE)
          m2 <- mean(ref_data$PostPerformance, na.rm = TRUE)
          sd1 <- sd(group_data$PostPerformance, na.rm = TRUE)
          sd2 <- sd(ref_data$PostPerformance, na.rm = TRUE)
          n1 <- nrow(group_data)
          n2 <- nrow(ref_data)
          
          pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
          cohens_d_raw <- abs(m1 - m2) / pooled_sd
          df_raw <- n1 + n2 - 2
          j_correction_raw <- 1 - (3 / (4 * df_raw - 1))
          hedges_g_raw <- cohens_d_raw * j_correction_raw
        }
      }, error = function(e) {
        hedges_g_raw <- NA
      })
      
      # Store results
      results <- rbind(results, data.frame(
        Comparison = paste(group, "vs", reference_group),
        Group_BiasScore = comp_bias,
        Group_SE = comp_se,
        Difference = bias_difference,
        Diff_SE = diff_se,
        t_stat = t_stat,
        p_value = p_value,
        CI_lower = ci_lower,
        CI_upper = ci_upper,
        Hedges_g_bias = hedges_g_bias,
        Hedges_g_raw = hedges_g_raw,
        stringsAsFactors = FALSE
      ))
      
    } else {
      cat(sprintf("WARNING: Group '%s' not found or duplicated\n", group))
    }
  }
  
  # Apply FDR correction
  if(nrow(results) > 0) {
    results$p_value_fdr <- p.adjust(results$p_value, method = "fdr")
    
    # Print detailed results
    cat("=== DETAILED RESULTS (before FDR correction) ===\n")
    for(i in 1:nrow(results)) {
      cat(sprintf("\n%s:\n", results$Comparison[i]))
      cat(sprintf("  Group bias score: %.4f (SE = %.4f)\n", results$Group_BiasScore[i], results$Group_SE[i]))
      cat(sprintf("  Difference: %.4f\n", results$Difference[i]))
      cat(sprintf("  SE of difference: %.4f\n", results$Diff_SE[i]))
      cat(sprintf("  t-statistic: %.4f\n", results$t_stat[i]))
      cat(sprintf("  p-value (uncorrected): %.4f\n", results$p_value[i]))
      cat(sprintf("  95%% CI: [%.4f, %.4f]\n", results$CI_lower[i], results$CI_upper[i]))
      cat(sprintf("  Hedges' g (bias scores): %.4f\n", results$Hedges_g_bias[i]))
      if(!is.na(results$Hedges_g_raw[i])) {
        cat(sprintf("  Hedges' g (raw data): %.4f\n", results$Hedges_g_raw[i]))
      } else {
        cat("  Hedges' g (raw data): N/A\n")
      }
    }
    
    # Print FDR corrected results
    cat("\n\n=== FDR CORRECTED RESULTS ===\n")
    cat(sprintf("Number of comparisons: %d\n", nrow(results)))
    cat(sprintf("FDR correction applied using Benjamini-Hochberg method\n\n"))
    
    # Sort by FDR-corrected p-value
    results_sorted <- results[order(results$p_value_fdr), ]
    
    cat("Results sorted by FDR-corrected p-value:\n")
    for(i in 1:nrow(results_sorted)) {
      significance <- ifelse(results_sorted$p_value_fdr[i] < 0.05, "***", "")
      direction <- ifelse(results_sorted$Difference[i] > 0, "HIGHER", "LOWER")
      
      cat(sprintf("%d. %s %s\n", i, results_sorted$Comparison[i], significance))
      cat(sprintf("   Difference: %.4f (Group is %.4f %s than reference)\n", 
                  results_sorted$Difference[i], abs(results_sorted$Difference[i]), direction))
      cat(sprintf("   p-value (uncorrected): %.4f\n", results_sorted$p_value[i]))
      cat(sprintf("   p-value (FDR): %.4f\n", results_sorted$p_value_fdr[i]))
      
      # Effect size calculations - now using pre-calculated values
      hedges_g_bias <- results_sorted$Hedges_g_bias[i]
      hedges_g_raw <- results_sorted$Hedges_g_raw[i]
      
      cat(sprintf("   Hedges' g (bias scores): %.3f ", hedges_g_bias))
      if(!is.na(hedges_g_raw)) {
        cat(sprintf("   Hedges' g (raw data): %.3f ", hedges_g_raw))
      } else {
        cat("   Hedges' g (raw data): N/A ")
      }
      
      # Interpret effect size (using bias score Hedges' g)
      if(hedges_g_bias < 0.2) {
        cat("(Small)\n")
      } else if(hedges_g_bias < 0.5) {
        cat("(Small to Medium)\n") 
      } else if(hedges_g_bias < 0.8) {
        cat("(Medium to Large)\n")
      } else {
        cat("(Large)\n")
      }
      cat("\n")
    }
    
    # Summary of significant results
    significant_results <- results_sorted[results_sorted$p_value_fdr < 0.05, ]
    
    cat("=== SUMMARY ===\n")
    if(nrow(significant_results) > 0) {
      cat(sprintf("*** %d out of %d comparisons are significant after FDR correction (α = 0.05) ***\n\n", 
                  nrow(significant_results), nrow(results)))
      
      for(i in 1:nrow(significant_results)) {
        direction <- ifelse(significant_results$Difference[i] > 0, "significantly HIGHER", "significantly LOWER")
        cat(sprintf("• %s has %s bias than %s (p_FDR = %.4f)\n", 
                    gsub(" vs.*", "", significant_results$Comparison[i]), 
                    direction, reference_group, significant_results$p_value_fdr[i]))
      }
    } else {
      cat("*** No comparisons are significant after FDR correction ***\n")
      cat("This suggests no meaningful differences between groups and the reference condition.\n")
    }
    
    # Create results table for export
    cat("\n=== RESULTS TABLE ===\n")
    results_table <- results_sorted[, c("Comparison", "Group_BiasScore", "Group_SE", "Difference", "Diff_SE", "Hedges_g_bias", "Hedges_g_raw", "p_value", "p_value_fdr")]
    names(results_table) <- c("Comparison", "Group_BiasScore", "Group_SE", "Difference", "Diff_SE", "Hedges_g_bias", "Hedges_g_raw", "p_uncorrected", "p_FDR")
    print(results_table, row.names = FALSE, digits = 4)
    
  } else {
    cat("ERROR: No valid comparisons could be performed\n")
  }
}

