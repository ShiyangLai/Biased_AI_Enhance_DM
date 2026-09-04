# =============================================================================
# a2 analysis, but with the AI treatment arm SEPARATED into three groups
# (Default, Democrat, Republican) instead of collapsed to Biased / Non-Biased.
#
# NOTE: only the *treatment* dimension (BiasedType) is separated. PoliBias is
# still the NEWS political category (Republican / Neutral / Democrat), and the
# "BiasScore" is still each arm's post-interaction performance gap across those
# three news categories. Reference arm = "Default"; bar order = Default, Dem, Rep.
# =============================================================================

# Calculate performance improvement for each observation
single_ai_processed_ <- single_ai_processed_ %>%
  mutate(PerformanceImprovement = PostPerformance - PrePerformance)

# Three-level treatment arm (Default = reference). Neutral treatment (if any) is
# excluded, matching the a1 separated analysis.
single_ai_processed_ <- single_ai_processed_ %>%
  dplyr::filter(AIStanceLabel_S %in% c("Default", "Democrat", "Republican")) %>%
  mutate(BiasedType = factor(
    case_when(
      AIStanceLabel_S == "Default"    ~ "Default",
      AIStanceLabel_S == "Democrat"   ~ "Democrat",
      AIStanceLabel_S == "Republican" ~ "Republican"
    ),
    levels = c("Default", "Democrat", "Republican")
  ))

# =============================================================================
# Mixed effects bias analysis functions
# =============================================================================
# Fit the mixed effects model
model <- lmer(PostPerformance ~ PrePerformance +
                BiasedType * PoliBias + UStanceLabel + #UIdeo + AICorrectness +
                # as.factor(NID) +
                (1 | UID),
              data = single_ai_processed_)

# Get model summary and key parameters
model_summary <- tidy(model, effects = "fixed")
model_sigma <- sigma(model)
model_df <- df.residual(model)
summary(model)$sigma
# performance::r2(model)

# Calculate Hedges' g correction factor (J)
J <- 1 - (3 / (4 * model_df - 1))

# Extract interaction terms
interaction_terms <- model_summary %>%
  dplyr::filter(grepl("BiasedType.*PoliBias", term))

# ANOVA to test interaction significance
interaction_test <- anova(model)

# Get marginal means for BiasedType (overall effect); pairs() now returns all
# pairwise contrasts among the three arms.
marginal_means <- emmeans(model, ~ BiasedType)
marginal_comparison <- pairs(marginal_means)
marginal_contrast_summary <- summary(marginal_comparison, infer = TRUE)

# Extract marginal effect parameters (vectors: one per pairwise contrast)
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

# Create effect sizes summary (one row per pairwise contrast among the arms)
effect_sizes_summary <- data.frame(
  Comparison = marginal_contrast_summary$contrast,
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

# `counts` (per-cell sample sizes) is optional here — performance_by_group is not
# used by the figure below, so only join if `counts` is available.
if(exists("counts")) {
  performance_by_group <- performance_by_group %>%
    left_join(counts, by = c("BiasedType", "PoliBias"))
}

# =============================================================================
# Helper: all pairwise comparisons of bias scores among the treatment arms
# (generalizes the original 2-group Biased-vs-Non-Biased t-test to 3 arms)
# =============================================================================
pairwise_bias_comparison <- function(bias_scores, model_df) {
  valid <- bias_scores[!is.na(bias_scores$BiasScore), ]
  res <- data.frame()
  if(nrow(valid) < 2) return(res)
  arms <- as.character(valid$BiasedType)
  for(i in 1:(nrow(valid) - 1)) {
    for(j in (i + 1):nrow(valid)) {
      diff <- valid$BiasScore[i] - valid$BiasScore[j]
      se   <- sqrt(valid$BiasScore_SE[i]^2 + valid$BiasScore_SE[j]^2)
      t_stat <- diff / se
      p_value <- 2 * (1 - pt(abs(t_stat), df = model_df))
      res <- rbind(res, data.frame(
        Comparison = paste(arms[i], "vs", arms[j]),
        BiasScore_1 = valid$BiasScore[i],
        BiasScore_2 = valid$BiasScore[j],
        Difference = diff,
        SE = se,
        t_stat = t_stat,
        df = model_df,
        p_value = p_value,
        CI_Lower = diff - qt(0.975, model_df) * se,
        CI_Upper = diff + qt(0.975, model_df) * se,
        Significant = ifelse(p_value < 0.05, "***", ifelse(p_value < 0.1, "*", ""))
      ))
    }
  }
  res
}

# =============================================================================
# Compare each partisan arm's NEWS-category gap against the Default arm's, for
# each news-condition pair (generalizes the original Biased-vs-Non-Biased gap fn)
# =============================================================================
calculate_condition_pair_comparisons_debug <- function(emm_data, model_df = NULL,
                                                        reference_arm = "Default") {
  if(is.null(model_df)) model_df <- df.residual(model)

  # Identify columns
  biased_col <- if("BiasedType" %in% names(emm_data)) "BiasedType" else names(emm_data)[1]
  polibias_col <- intersect(c("PoliBias", "Political", "Condition"), names(emm_data))[1]
  if(is.na(polibias_col)) { cat("ERROR: Cannot find PoliBias column\n"); return(data.frame()) }

  arms <- setdiff(unique(as.character(emm_data[[biased_col]])), reference_arm)

  # Define news-condition pairs
  condition_pairs <- list(
    c("Republican", "Neutral"),
    c("Republican", "Democrat"),
    c("Democrat", "Neutral")
  )

  # Gap within one arm for a given news-condition pair
  arm_gap <- function(arm, cond1, cond2) {
    d <- emm_data[emm_data[[biased_col]] == arm, ]
    if(!(cond1 %in% d[[polibias_col]] && cond2 %in% d[[polibias_col]])) return(NULL)
    g  <- abs(d$emmean[d[[polibias_col]] == cond1] - d$emmean[d[[polibias_col]] == cond2])
    se <- sqrt(d$SE[d[[polibias_col]] == cond1]^2 + d$SE[d[[polibias_col]] == cond2]^2)
    list(gap = g, se = se)
  }

  results_df <- data.frame()
  for(arm in arms) {
    for(pair in condition_pairs) {
      cond1 <- pair[1]; cond2 <- pair[2]
      a <- arm_gap(arm, cond1, cond2)
      r <- arm_gap(reference_arm, cond1, cond2)
      if(is.null(a) || is.null(r)) next

      gap_difference <- a$gap - r$gap
      gap_diff_se <- sqrt(a$se^2 + r$se^2)
      t_stat <- gap_difference / gap_diff_se
      p_value <- 2 * (1 - pt(abs(t_stat), df = model_df))

      results_df <- rbind(results_df, data.frame(
        Treatment_Arm = arm,
        Condition_Pair = paste(cond1, "vs", cond2),
        Arm_Gap = a$gap,
        Default_Gap = r$gap,
        Gap_Difference = gap_difference,
        SE = gap_diff_se,
        t_stat = t_stat,
        p_value = p_value,
        CI_Lower = gap_difference - 1.96 * gap_diff_se,
        CI_Upper = gap_difference + 1.96 * gap_diff_se,
        Significant = ifelse(p_value < 0.05, "***", ifelse(p_value < 0.1, "*", ""))
      ))
    }
  }
  return(results_df)
}

cat("\n=== NEWS-CATEGORY GAP: each partisan arm vs Default ===\n")
condition_pair_results_debug <- calculate_condition_pair_comparisons_debug(emm_summary)
print(condition_pair_results_debug)


# =============================================================================
# Visualization
# =============================================================================
# Check if bias_scores exists and has data
if(exists("bias_scores") && nrow(bias_scores) > 0 && any(!is.na(bias_scores$BiasScore))) {

  # Define colors (Default = gray baseline, Democrat = blue, Republican = red)
  bias_colors <- c(
    "Default"    = "#79706E",
    "Democrat"   = "#4E79A7",
    "Republican" = "#E15759"
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

  # Create comprehensive bias mitigation plot (one bar per treatment arm)
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
    scale_fill_manual(values = bias_colors,
                      breaks = c("Republican", "Default", "Democrat")) +
    # Bar order (visualization only; model reference stays "Default")
    scale_x_discrete(limits = c("Republican", "Default", "Democrat")) +
    scale_y_continuous(labels = label_number(accuracy = 0.01),
                       expand = expansion(mult = c(0, 0.25)),
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

  # Pairwise bias-mitigation effects among the treatment arms
  cat("\n=== PAIRWISE BIAS-SCORE COMPARISONS (Mixed Effects) ===\n")
  mixed_pairwise <- pairwise_bias_comparison(bias_scores, df.residual(model))
  if(nrow(mixed_pairwise) > 0) {
    for(i in 1:nrow(mixed_pairwise)) {
      cat(sprintf("%s: diff = %.3f (95%% CI: %.3f to %.3f, p = %.3f) %s\n",
                  mixed_pairwise$Comparison[i],
                  mixed_pairwise$Difference[i],
                  mixed_pairwise$CI_Lower[i],
                  mixed_pairwise$CI_Upper[i],
                  mixed_pairwise$p_value[i],
                  mixed_pairwise$Significant[i]))
    }
  }

  # Also show the overall effect sizes from the marginal comparison
  if(exists("effect_sizes_summary")) {
    cat("\nOverall Effect Sizes (Hedges' g) by contrast:\n")
    print(effect_sizes_summary[, c("Comparison", "Hedges_g",
                                   "Hedges_g_Lower_CI", "Hedges_g_Upper_CI")])
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
  dplyr::filter(grepl("BiasedType.*PoliBias", term))

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

# =============================================================================
# Pairwise comparison of bias scores among treatment arms (OLS + robust SE)
# =============================================================================
ols_pairwise <- pairwise_bias_comparison(bias_scores, df.residual(model))

if(nrow(ols_pairwise) > 0) {
  cat("\n=== PAIRWISE BIAS-SCORE COMPARISONS (OLS, cluster-robust) ===\n")
  for(i in 1:nrow(ols_pairwise)) {
    cat(sprintf("\n--- %s ---\n", ols_pairwise$Comparison[i]))
    cat(sprintf("Difference: %.4f\n", ols_pairwise$Difference[i]))
    cat(sprintf("SE of difference: %.4f\n", ols_pairwise$SE[i]))
    cat(sprintf("t-statistic: %.4f (df = %d)\n", ols_pairwise$t_stat[i], ols_pairwise$df[i]))
    cat(sprintf("p-value: %.4f\n", ols_pairwise$p_value[i]))
    cat(sprintf("95%% CI: [%.4f, %.4f]\n", ols_pairwise$CI_Lower[i], ols_pairwise$CI_Upper[i]))
    if(ols_pairwise$p_value[i] < 0.05) {
      cat("*** SIGNIFICANT difference in political bias between these arms ***\n")
    } else {
      cat("NOT SIGNIFICANT\n")
    }
  }
} else {
  cat("ERROR: Need at least 2 arms with valid bias scores for comparison\n")
  if(exists("bias_scores")) {
    cat("Current bias_scores:\n")
    print(bias_scores)
  }
}

