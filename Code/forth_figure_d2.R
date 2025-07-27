# Load required library
library(dplyr)
library(ggplot2)
library(sandwich)
library(lmtest)
library(lme4)
library(lmerTest) # For p-values
library(scales) # For color functions
library(ordinal)
library(MCMCglmm)
set.seed(123)

combined_data <- combined_data %>%
  mutate(AIInterMean_numeric = factor(AIInterMean, 
                                      levels = c("Not meaningful at all",
                                                 "Slightly meaningful", 
                                                 "Moderately meaningful",
                                                 "Very meaningful",
                                                 "Extremely meaningful"),
                                      labels = 1:5) %>%
           as.numeric())

# 6. Run the mixed-effects model
combined_data$PerceivedImproveCode <- factor(combined_data$PerceivedImproveCode, ordered = TRUE)
combined_data$AIInterMean_numeric <- factor(combined_data$AIInterMean_numeric, ordered=TRUE)

# =====================================
# FIT MODEL
# =====================================
model_clm <- clm(AIInterMean_numeric ~ ExperimentType + as.factor(NID) + PrePerformance +
                   UStanceLabel + AICorrectness + UIdeo,
                 data = combined_data, na.action = na.omit)
vcov_clm <- vcovCL(model_clm, cluster = combined_data$UID)
clustered_results_clm <- coeftest(model_clm, vcov = vcov_clm)
print(clustered_results_clm)
# summary(model_clm)
df.residual(model_clm)
r2(model_clm)

model_clm <- MCMCglmm(AIInterMean_numeric ~ ExperimentType + as.factor(NID) +
                        PrePerformance + UIdeo + AICorrectness +
                        as.factor(UStanceLabel),
                            random = ~UID,
                            family = "ordinal",
                            nitt = 25000, thin = 10, burnin = 5000,
                            data = combined_data[complete.cases(combined_data[ , "AICorrectness"]), ]
                            )

print(summary(model_clm))
se_fixed <- apply(model_clm$Sol, 2, sd)
se_fixed
sqrt(model_clm$VCV[ , "units"][1])
X  <- model_clm$X                 # design matrix for fixed effects
B  <- as.matrix(model_clm$Sol)            # iterations × coefficients
eta <- X %*% t(B)
VF  <- apply(eta, 2, var)          # fixed-effect variance per draw
VR  <- model_clm$VCV[ , "UID"]      # random-effect variance per draw
VE <- 1                                  # residual var. (probit link)
R2_marginal    <- VF / (VF + VR + VE)               # fixed only
R2_conditional <- (VF + VR) / (VF + VR + VE)        # fixed + random
cbind(
  Marginal    = c(mean(R2_marginal),
                  quantile(R2_marginal, c(.025, .5, .975))),
  Conditional = c(mean(R2_conditional),
                  quantile(R2_conditional, c(.025, .5, .975)))
)

# =====================================
# MANUAL MARGINAL MEANS CALCULATION FOR MCMCGLMM
# =====================================

# Get posterior samples from the model
posterior_samples <- model_clm$Sol

# Get covariate means for averaging (marginal means approach)
mean_preperformance <- mean(combined_data$PrePerformance, na.rm = TRUE)
# Get most common NID level (or you could average over all levels)
mode_nid <- names(sort(table(combined_data$NID), decreasing = TRUE))[1]

# Get column names for reference
col_names <- colnames(posterior_samples)
print("Available columns in posterior:")
print(col_names)

# Create a data frame for all treatments
# treatments <- c("Single_AI_Non_Biased", "Single_AI_Non_Biased_Exp", "Single_AI_Opposition", 
#                 "Dual_AI_Non_Biased", "Dual_AI_Non_Biased_Exp",
#                 "Dual_AI_Opposition", "Dual_AI_Balanced")
treatments <- c("Single_AI_Non_Biased", "Single_AI_Biased",
                "Dual_AI_Non_Biased",
                "Dual_AI_Opposition", "Dual_AI_Balanced")

# Initialize results data frame
plot_data <- data.frame(
  ExperimentType = treatments,
  posterior_mean = numeric(length(treatments)),
  posterior_sd = numeric(length(treatments)),
  lower_ci = numeric(length(treatments)),
  upper_ci = numeric(length(treatments)),
  stringsAsFactors = FALSE
)

# Calculate marginal means for each treatment
for(i in 1:length(treatments)) {
  treatment <- treatments[i]
  
  # Start with intercept (cut point for ordinal model)
  # For MCMCglmm ordinal, we typically use the first cut point as baseline
  if("(Intercept)" %in% col_names) {
    linear_predictor <- posterior_samples[, "(Intercept)"]
  } else {
    # If no intercept column, start with zeros
    linear_predictor <- rep(0, nrow(posterior_samples))
  }
  
  # Add treatment effect
  if(treatment != "Single_AI_Non_Biased") {
    col_name <- paste0("ExperimentType", treatment)
    if(col_name %in% col_names) {
      linear_predictor <- linear_predictor + posterior_samples[, col_name]
    } else {
      warning(paste("Treatment", treatment, "not found in model"))
    }
  }
  # Note: baseline treatment (Single_AI_Non_Biased) gets no additional effect
  
  # Add PrePerformance effect (averaged at mean)
  if("PrePerformance" %in% col_names) {
    linear_predictor <- linear_predictor + posterior_samples[, "PrePerformance"] * mean_preperformance
  }
  
  # Add NID effect (averaged at mode)
  nid_col_name <- paste0("as.factor(NID)", mode_nid)
  if(nid_col_name %in% col_names) {
    linear_predictor <- linear_predictor + posterior_samples[, nid_col_name]
  }
  
  # These are marginal means on the linear predictor scale
  # For ordinal models, this represents the latent variable scale
  
  # Calculate posterior summary statistics
  plot_data$posterior_mean[i] <- mean(linear_predictor)
  plot_data$posterior_sd[i] <- sd(linear_predictor)
  plot_data$lower_ci[i] <- quantile(linear_predictor, 0.025)
  plot_data$upper_ci[i] <- quantile(linear_predictor, 0.975)
}

# =====================================
# CREATE FORMAL LABELS
# =====================================

plot_data$formal_label <- case_when(
  plot_data$ExperimentType == "Single_AI_Non_Biased" ~ "Single AI\nNon-Biased",
  plot_data$ExperimentType == "Single_AI_Biased" ~ "Single AI\nBiased",
  # plot_data$ExperimentType == "Single_AI_Non_Biased_Exp" ~ "Single AI\nNeutralized",
  # plot_data$ExperimentType == "Single_AI_Opposition" ~ "Single AI\nOpposition",
  plot_data$ExperimentType == "Dual_AI_Non_Biased" ~ "Dual AI\nNon-Biased",
  # plot_data$ExperimentType == "Dual_AI_Non_Biased_Exp" ~ "Dual AI\nNeutralized",
  plot_data$ExperimentType == "Dual_AI_Balanced" ~ "Dual AI\nBalanced",
  plot_data$ExperimentType == "Dual_AI_Opposition" ~ "Dual AI\nOpposition",
  TRUE ~ as.character(plot_data$ExperimentType)
)

# Order the labels (reverse for horizontal plot)
unique_labels <- unique(plot_data$formal_label)
plot_data$formal_label <- factor(plot_data$formal_label, levels = rev(unique_labels))

# =====================================
# VALUE-BASED COLOR MAPPING
# =====================================

# Define color palette
color_perceived <- "#D2691E"    # Chocolate (dark orange-brown)
color_perceived_90 <- "#FF8C00" # Dark orange
color_perceived_95 <- "#FFA500" # Orange
color_perceived_99 <- "#FFB347" # Peach (light but visible orange)
# color_perceived <- "#4B0082"    # Indigo
# color_perceived_90 <- "#9370DB" # Medium orchid
# color_perceived_95 <- "#B88BD8" # Lavender
# color_perceived_99 <- "#D6C0E5" # Soft lilac

# Create color gradient based on posterior mean values
min_val <- min(plot_data$posterior_mean)
max_val <- max(plot_data$posterior_mean)
plot_data$normalized_value <- (plot_data$posterior_mean - min_val) / (max_val - min_val)

# Color mapping function
get_color_for_value <- function(norm_val) {
  colors <- c(color_perceived_99, color_perceived_95, color_perceived_90, color_perceived)
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

plot_data$fill_color <- sapply(plot_data$normalized_value, get_color_for_value)

# =====================================
# CREATE VISUALIZATION
# =====================================

# Calculate x-axis limits
x_min <- min(plot_data$lower_ci) * 0.95
x_max <- max(plot_data$upper_ci) * 1.05

# Create horizontal bar plot
p_bayesian <- ggplot(plot_data, aes(y = formal_label, x = posterior_mean)) +
  geom_col(aes(fill = fill_color), color = "black", width = 0.7, linewidth = 0) +
  geom_errorbar(aes(xmin = lower_ci, xmax = upper_ci), 
                width = 0.3, linewidth = 0.5, color = "black") +
  
  # Add value labels
  geom_text(aes(x = upper_ci + (x_max - x_min) * 0.05, 
                label = round(posterior_mean, 3)), 
            hjust = 0, family = "Avenir", size = 3, color = "black") +
  
  # Add points
  geom_point(aes(y = formal_label, x = posterior_mean), 
             color = "black", size = 2, shape = 17) +
  
  # Use manual fill scale
  scale_fill_identity() +
  
  # Set x-axis
  scale_x_continuous(
    expand = c(0, 0), 
    labels = scales::number_format(accuracy = 0.01)
  ) +
  
  coord_cartesian(xlim = c(1, 8)) +
  
  labs(y = "Experimental Condition", 
       x = "Interaction Meaningfulness") +
  
  # geom_vline(xintercept = 0, linetype = "dotted", color = "black", linewidth = 0.5) +
  theme_classic() +
  theme(
    text = element_text(family = "Avenir", color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.line = element_blank(),
    # axis.text.y = element_text(family = "Avenir", size = 9, color = "black",
    #                            margin = margin(r = 5)),
    axis.text.y = element_blank(),
    axis.text.x = element_text(family = "Avenir", size = 9, color = "black"),
    axis.title.y = element_blank(),
    # axis.title.y = element_text(family = "Avenir", size = 12, color = "black",
    #                             margin = margin(r = 15)),
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

print(p_bayesian)

# =========================================
# Manual pairwise comparison with Hedges' g
# =========================================

# First, we need to reconstruct the marginal means for each posterior sample
# to get the full posterior distribution for comparisons

# Create matrix to store marginal means for each treatment and posterior sample
n_samples <- nrow(posterior_samples)
n_treatments <- length(treatments)
marginal_means_matrix <- matrix(NA, nrow = n_samples, ncol = n_treatments)
colnames(marginal_means_matrix) <- treatments

# Calculate marginal means for each posterior sample
for(i in 1:length(treatments)) {
  treatment <- treatments[i]
  
  # Start with intercept
  if("(Intercept)" %in% col_names) {
    linear_predictor <- posterior_samples[, "(Intercept)"]
  } else {
    linear_predictor <- rep(0, n_samples)
  }
  
  # Add treatment effect
  if(treatment != "Single_AI_Non_Biased") {
    col_name <- paste0("ExperimentType", treatment)
    if(col_name %in% col_names) {
      linear_predictor <- linear_predictor + posterior_samples[, col_name]
    }
  }
  
  # Add PrePerformance effect
  if("PrePerformance" %in% col_names) {
    linear_predictor <- linear_predictor + posterior_samples[, "PrePerformance"] * mean_preperformance
  }
  
  # Add NID effect
  nid_col_name <- paste0("as.factor(NID)", mode_nid)
  if(nid_col_name %in% col_names) {
    linear_predictor <- linear_predictor + posterior_samples[, nid_col_name]
  }
  
  marginal_means_matrix[, i] <- linear_predictor
}

# Calculate pooled standard deviation for effect size calculation
# For ordinal models, use empirical SD from observed data (following your function)
if("PerceivedImproveCode" %in% names(combined_data)) {
  response_var <- "PerceivedImproveCode"
} else {
  # Try to extract from model formula
  response_var <- all.vars(model_clm$Fixed$formula)[1]
}

observed_values <- as.numeric(combined_data[[response_var]])
pooled_sd <- sd(observed_values, na.rm = TRUE)
pooled_sd_samples <- rep(pooled_sd, n_samples)

cat("Using empirical pooled SD from observed ordinal data:", round(pooled_sd, 4), "\n")

# Create pairwise comparison results
comparisons_list <- list()
comparison_count <- 0

for(i in 1:(n_treatments-1)) {
  for(j in (i+1):n_treatments) {
    comparison_count <- comparison_count + 1
    
    treatment1 <- treatments[i]
    treatment2 <- treatments[j]
    
    # Calculate posterior difference on latent scale (correct approach)
    diff_samples <- marginal_means_matrix[, i] - marginal_means_matrix[, j]
    
    # Calculate statistics
    mean_diff <- mean(diff_samples)
    se_diff <- sd(diff_samples)
    
    # Calculate Bayesian "p-value" (probability of difference being > 0)
    if(mean_diff > 0) {
      p_value <- mean(diff_samples < 0) * 2  # Two-tailed
    } else {
      p_value <- mean(diff_samples > 0) * 2  # Two-tailed
    }
    p_value <- min(p_value, 1)  # Cap at 1
    
    # Calculate Hedges' g following your function approach
    # First calculate Cohen's d for each sample
    cohens_d_samples <- diff_samples / pooled_sd_samples
    
    # Apply Hedges' correction factor
    # Get sample sizes for the two groups being compared
    n1 <- sum(combined_data$ExperimentType == treatment1, na.rm = TRUE)
    n2 <- sum(combined_data$ExperimentType == treatment2, na.rm = TRUE)
    df <- n1 + n2 - 2
    
    if(df > 0) {
      correction_factor <- 1 - (3 / (4 * df - 1))
    } else {
      correction_factor <- 1  # Fallback
    }
    
    hedges_g_samples <- cohens_d_samples * correction_factor
    
    # Calculate summary statistics for Hedges' g
    hedges_g_mean <- mean(hedges_g_samples)
    hedges_g_lower <- quantile(hedges_g_samples, 0.025)
    hedges_g_upper <- quantile(hedges_g_samples, 0.975)
    
    # Calculate 95% credible interval for difference
    ci_lower <- quantile(diff_samples, 0.025)
    ci_upper <- quantile(diff_samples, 0.975)
    
    comparisons_list[[comparison_count]] <- data.frame(
      Group1 = treatment1,
      Group2 = treatment2,
      Difference = round(mean_diff, 4),
      SE = round(se_diff, 4),
      CI_lower = round(ci_lower, 4),
      CI_upper = round(ci_upper, 4),
      p_value = round(p_value, 4),
      Hedges_g = round(hedges_g_mean, 4),
      Hedges_g_Lower = round(hedges_g_lower, 4),
      Hedges_g_Upper = round(hedges_g_upper, 4),
      n1 = n1,
      n2 = n2,
      stringsAsFactors = FALSE
    )
  }
}

# Combine all comparisons
comparisons_df <- do.call(rbind, comparisons_list)

# Apply FDR correction
comparisons_df$p_adj_fdr <- round(p.adjust(comparisons_df$p_value, method = "fdr"), 4)

# Add significance symbols
comparisons_df$Significance <- case_when(
  comparisons_df$p_adj_fdr < 0.001 ~ "***",
  comparisons_df$p_adj_fdr < 0.01 ~ "**",
  comparisons_df$p_adj_fdr < 0.05 ~ "*",
  comparisons_df$p_adj_fdr < 0.1 ~ "†",
  TRUE ~ "ns"
)

# Add effect size interpretation
comparisons_df$Effect_Size <- case_when(
  abs(comparisons_df$Hedges_g) < 0.2 ~ "Negligible",
  abs(comparisons_df$Hedges_g) < 0.5 ~ "Small", 
  abs(comparisons_df$Hedges_g) < 0.8 ~ "Medium",
  TRUE ~ "Large"
)

# Print results
cat("\n=== PAIRWISE COMPARISONS WITH HEDGES' G ===\n")
cat("FDR-corrected p-values and effect sizes\n")
cat("*** p < 0.001, ** p < 0.01, * p < 0.05, † p < 0.1, ns = not significant\n\n")

# Create a clean results table with 95% confidence intervals
results_table <- comparisons_df %>%
  dplyr::select(Group1, Group2, Difference, CI_lower, CI_upper, SE, p_adj_fdr, 
                Hedges_g, Hedges_g_Lower, Hedges_g_Upper, Effect_Size, Significance) %>%
  arrange(p_adj_fdr)

print(results_table)

# Create a more readable summary with formatted CI
cat("\n=== SUMMARY WITH FORMATTED CONFIDENCE INTERVALS ===\n")
summary_table <- results_table %>%
  mutate(
    `95% CI` = paste0("[", CI_lower, ", ", CI_upper, "]"),
    `Hedges' g [95% CI]` = paste0(Hedges_g, " [", Hedges_g_Lower, ", ", Hedges_g_Upper, "]")
  ) %>%
  dplyr::select(Group1, Group2, Difference, `95% CI`, p_adj_fdr, `Hedges' g [95% CI]`, Effect_Size, Significance)

print(summary_table)
