# ==============================================================================
# second_figure_b2.R — AI correctness by bias magnitude + perceived AI role.
# Self-contained: derives every input it needs if not already in the session.
# ==============================================================================
library(dplyr)
library(ggplot2)
library(scales)     # label_percent, number_format
library(ordinal)    # clm
library(sandwich)   # vcovCL
library(lmtest)     # coeftest
library(emmeans)

# Base data: single_ai_processed comes from preprcessing.R
if (!exists("single_ai_processed")) {
  cat("single_ai_processed not found - sourcing preprcessing.R\n")
  source("preprcessing.R")
}
# Neutral-treatment rows excluded, as elsewhere in the pipeline
if (!exists("single_ai_processed_")) {
  single_ai_processed_ <- single_ai_processed[single_ai_processed$AIStanceLabel_S != "Neutral", ]
}

# Bias-magnitude categories (same rule as second_figure_b1.R; keyed off
# AIStanceLabel_S because BiasedType now holds Default/Republican/Democrat)
make_biased_cat <- function(df) {
  factor(ifelse(df$AIStanceLabel_S == "Default", "Default",
                ifelse(df$AIStanceLabel %in% c("Strong Republican", "Strong Democrat"),
                       "Strong Bias", "Moderate Bias")),
         levels = c("Default", "Moderate Bias", "Strong Bias"))
}
single_ai_processed_$BiasedCat <- make_biased_cat(single_ai_processed_)
if (!exists("complete_data")) {
  cat("complete_data not found - deriving from single_ai_processed_\n")
  complete_data <- single_ai_processed_
}
complete_data$BiasedCat <- make_biased_cat(complete_data)

# Check available data
cat("Unique values in BiasedCat:", paste(unique(complete_data$BiasedCat), collapse = ", "), "\n")
cat("AICorrectness summary:\n")
print(summary(complete_data$AICorrectness))

# Ensure BiasedCat is properly ordered as a factor
complete_data$BiasedCat <- factor(complete_data$BiasedCat, 
                                  levels = c("Default", "Moderate Bias", "Strong Bias"))

# Check the factor levels
cat("BiasedCat levels:", paste(levels(complete_data$BiasedCat), collapse = ", "), "\n")

# Fit the AI correctness model
model_ai <- lm(AICorrectness ~ BiasedCat + as.factor(NID), 
               data = complete_data)

# Partisan-arm subset (was `BiasedType == "Biased"`, which no longer matches —
# BiasedType holds Default/Republican/Democrat now)
model <- clm(
  as.factor(AIStanceLabel_S) ~ AICorrectness + factor(NID),
  data = subset(complete_data, AIStanceLabel_S %in% c("Republican", "Democrat"))
)


# Calculate clustered standard errors
vcov_ai <- vcovCL(model_ai, cluster = complete_data$UID)

# Get coefficient tests with clustered SEs
clustered_results_ai <- coeftest(model_ai, vcov = vcov_ai)

cat("=== AI CORRECTNESS MODEL RESULTS ===\n")
print(clustered_results_ai)

# ==============================
# Marginal means for comparison
# ==============================
# Get marginal means for each BiasedCat level
emm_ai <- emmeans(model_ai, ~ BiasedCat, vcov. = vcov_ai)

cat("\n=== MARGINAL MEANS BY BIAS CATEGORY ===\n")
print(emm_ai)

# Pairwise comparisons between bias categories
pairwise_ai <- pairs(emm_ai, adjust = "fdr")

cat("\n=== PAIRWISE COMPARISONS (FDR ADJUSTED) ===\n")
print(pairwise_ai)


# =====================================
# Prepare data for bar plot
# =====================================
# Convert marginal means to data frame for plotting
emm_ai_df <- as.data.frame(emm_ai) %>%
  mutate(
    # Calculate 95% confidence intervals
    t_95 = qt(0.975, df),
    lower_95 = emmean - SE * t_95,
    upper_95 = emmean + SE * t_95,
    # Ensure proper factor ordering
    BiasedCat = factor(BiasedCat, levels = c("Default", "Moderate Bias", "Strong Bias"))
  )

cat("\n=== AI CORRECTNESS BY BIAS MAGNITUDE ===\n")
for(i in 1:nrow(emm_ai_df)) {
  cat(sprintf("%s: %.3f ± %.3f (SE), 95%% CI: [%.3f, %.3f]\n", 
              emm_ai_df$BiasedCat[i], 
              emm_ai_df$emmean[i], 
              emm_ai_df$SE[i],
              emm_ai_df$lower_95[i],
              emm_ai_df$upper_95[i]))
}

# Define colors for AI correctness
ai_colors <- c(
  "Default" = "#4E79A7",       # Blue
  "Moderate Bias" = "#A2688F", # Mauve / muted purple
  "Strong Bias" = "#E15759"    # Red
)

# Create the bar plot
p_ai_correctness <- ggplot(emm_ai_df, aes(x = BiasedCat, y = emmean, fill = BiasedCat)) +
  # Bars
  geom_col(alpha = 0.8, width = 0.6, color = "black", linewidth = 0.3) +
  
  # Error bars for 95% CI
  geom_errorbar(aes(ymin = lower_95, ymax = upper_95), 
                width = 0.15, size = 0.5, color = "black") +
  
  # Value labels above error bars
  geom_text(aes(y = upper_95 + max(upper_95) * 0.012, 
                label = round(emmean, 3)), 
            size = 3.5, family = "Arial", color = "black") +
  
  scale_fill_manual(values = ai_colors) +


  scale_y_continuous(
    name = "AI Fact-checking Correctness",
    labels = scales::number_format(accuracy = 0.01),
    expand = expansion(mult = c(0, 0.1))
  ) +
  
  labs(
    x = "AI Bias Magnitude"
  ) +
  
  theme_classic() +
  theme(legend.position = c(0.42, 0.98),
        legend.justification = c(1, 1),
        legend.background = element_rect(fill = "white", color = NA),
        legend.margin = margin(4, 4, 4, 4),
        legend.key = element_rect(fill = "white", color = NA),
        legend.key.height = unit(0.42, "cm"),
        legend.text = element_text(family = "Avenir", size = 9.5))

# Display the plot (screen device only; Rscript's default device lacks the fonts)
if (interactive()) print(p_ai_correctness)

# =====================================
# Statistical testing summary
# =====================================
cat("\n=== STATISTICAL TESTING SUMMARY ===\n")

# Overall F-test for the model
cat("Overall Model F-test:\n")
anova_result <- anova(model_ai)
print(anova_result)

# Convert pairwise results to data frame for easier reading
pairwise_df <- as.data.frame(pairwise_ai)

cat("\nPairwise Comparisons (FDR Adjusted):\n")
for(i in 1:nrow(pairwise_df)) {
  significance <- ifelse(pairwise_df$p.value[i] < 0.001, "***",
                         ifelse(pairwise_df$p.value[i] < 0.01, "**",
                                ifelse(pairwise_df$p.value[i] < 0.05, "*",
                                       ifelse(pairwise_df$p.value[i] < 0.1, ".", "ns"))))
  
  cat(sprintf("%s: Difference = %.3f ± %.3f (SE), t = %.2f, p = %.3f %s\n",
              pairwise_df$contrast[i],
              pairwise_df$estimate[i],
              pairwise_df$SE[i],
              pairwise_df$t.ratio[i],
              pairwise_df$p.value[i],
              significance))
}

cat("\nSignificance codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")

# =====================================
# Effect size interpretation
# =====================================
cat("\n=== EFFECT SIZE INTERPRETATION ===\n")

# Calculate effect sizes relative to Default
no_bias_est <- emm_ai_df$emmean[emm_ai_df$BiasedCat == "Default"]
moderate_est <- emm_ai_df$emmean[emm_ai_df$BiasedCat == "Moderate Bias"]
strong_est <- emm_ai_df$emmean[emm_ai_df$BiasedCat == "Strong Bias"]

cat("Effect sizes (relative to Default baseline):\n")
cat(sprintf("Moderate Bias effect: %.3f (%.1f%% change)\n", 
            moderate_est - no_bias_est, 
            ((moderate_est - no_bias_est) / no_bias_est) * 100))
cat(sprintf("Strong Bias effect: %.3f (%.1f%% change)\n", 
            strong_est - no_bias_est, 
            ((strong_est - no_bias_est) / no_bias_est) * 100))

# Sample sizes for power interpretation
sample_sizes <- complete_data %>%
  group_by(BiasedCat) %>%
  summarise(n = n(), .groups = 'drop') %>%
  mutate(
    BiasedCat = factor(BiasedCat, levels = c("Default", "Moderate Bias", "Strong Bias")),
    percentage = n / sum(n) * 100
  )

cat("\nSample sizes:\n")
print(sample_sizes)

# =====================================
# AI correctness distribution plot
# =====================================
cat("\n=== CREATING AI CORRECTNESS DISTRIBUTION PLOT ===\n")

# Ensure data is properly filtered and ordered
plot_data <- complete_data %>%
  dplyr::filter(!is.na(BiasedCat) & !is.na(AICorrectness)) %>%
  mutate(BiasedCat = factor(BiasedCat, levels = c("Default", "Moderate Bias", "Strong Bias")))

# Print summary statistics for each group
cat("AI Correctness summary by bias category:\n")
summary_stats <- plot_data %>%
  group_by(BiasedCat) %>%
  summarise(
    n = n(),
    mean = mean(AICorrectness, na.rm = TRUE),
    sd = sd(AICorrectness, na.rm = TRUE),
    median = median(AICorrectness, na.rm = TRUE),
    min = min(AICorrectness, na.rm = TRUE),
    max = max(AICorrectness, na.rm = TRUE),
    .groups = 'drop'
  )
print(summary_stats)

base_theme <- theme_classic() +
  theme(
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.line = element_blank(),
    text = element_text(family = "Avenir", color = "black"),
    axis.title.x = element_text(family = "Avenir", size = 13.5, margin = margin(t = 12)),
    axis.title.y = element_text(family = "Avenir", size = 13.5, margin = margin(r = 10)),
    axis.text.x = element_text(family = "Avenir", size = 11.7, color = "black",
                               margin = margin(t = 4)),
    axis.text.y = element_text(family = "Avenir", size = 11.7, color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    axis.ticks.length = unit(3.5, "pt"),
    panel.grid = element_blank(),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10))

# Create probability distribution plot showing AI Correctness by bias category
# geom_density() computes the curves itself; `divisor` rescales them to the
# plotted y-range, so the axis is a scaled density rather than one integrating to 1.
divisor <- nrow(plot_data) / 300

p_distribution <- ggplot(plot_data, aes(x = AICorrectness, 
                                        color = BiasedCat, fill = BiasedCat)) +
  # Density curves
  geom_density(data = plot_data,
               aes(AICorrectness, y = ..density../get("divisor", pos = 1)),
               alpha = 0.3, size = 0.7) +
  
  # Add vertical lines for means (more prominent)
  geom_vline(data = summary_stats, 
             aes(xintercept = mean, color = BiasedCat), 
             linetype = "solid", size = 1., alpha = 0.9) +
  
  scale_fill_manual(values = ai_colors, name = "") +
  scale_color_manual(values = ai_colors, name = "") +
  
  scale_x_continuous(
    name = "AI Fact-checking Correctness",
    expand = expansion(mult = c(0, 0.0)),
    labels = scales::number_format(accuracy = 0.01)
  ) +
  
  scale_y_continuous(
    name = "Probability Density",
    expand = expansion(mult = c(0, 0.05)),
    labels = scales::number_format(accuracy = 0.01)
  ) +
  
  # Nature journal styling
  base_theme +
  theme(
    # Legend inside plot (upper-left corner)
    legend.position = c(0.05, 0.9),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.),
    legend.margin = margin(6, 6, 6, 6),
    legend.key.size = unit(0.8, "cm"),
    legend.key = element_rect(fill = "white", color = NA),
    legend.title = element_blank(),
    legend.text = element_text(family = "Avenir", size = 9.5),
    legend.spacing.y = unit(0.5, "cm"),
    legend.key.height = unit(0.5, "cm"),  # Controls spacing between legend items
    
    # Margins
    plot.margin = margin(t = 12, r = 12, b = 10, l = 12)
  )

p_distribution
# Display and save the probability distribution plot.
# ragg is required: Avenir is unavailable on Rscript's default PDF device.
out_dist <- file.path("../figures", "second_figure_b2_ai_correctness_density.png")
ragg::agg_png(out_dist, width = 5., height = 4.3, units = "in", res = 500)
print(p_distribution); dev.off()
cat("\nSaved:", out_dist, "\n")

# =====================================
# Perceived role visualization
# =====================================
# Create bias categories from AIStanceLabel
single_ai_processed_ <- single_ai_processed_ %>%
  mutate(
    BiasCategory = case_when(
      AIStanceLabel %in% c("Default", "Politically Neutral") ~ "Default",
      AIStanceLabel %in% c("Somewhat Republican", "Somewhat Democrat") ~ "Moderate Bias",
      AIStanceLabel %in% c("Strong Republican", "Strong Democrat") ~ "Strong Bias",
      TRUE ~ NA_character_
    ),
    # Order the bias categories (left-to-right on the vertical plot)
    BiasCategory = factor(BiasCategory, levels = c("Default", "Moderate Bias", "Strong Bias"))
  )

# Check the distribution
cat("Bias Category Distribution:\n")
table(single_ai_processed_$BiasCategory, useNA = "ifany")

# Calculate counts and percentages for each combination
role_bias_summary <- single_ai_processed_ %>%
  dplyr::filter(!is.na(BiasCategory)) %>%
  group_by(BiasCategory, PerceivedAIRole) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  group_by(BiasCategory) %>%
  mutate(
    Percentage = Count / sum(Count) * 100,
    Total = sum(Count)
  ) %>%
  ungroup() %>%
  # Clean up role labels for better display
  mutate(
    PerceivedAIRole_Clean = case_when(
      PerceivedAIRole == "Mostly as a tool to assist me in making my own determinations" ~ "Tool",
      PerceivedAIRole == "A mix of both a tool and an influencing agent" ~ "Mixed",
      PerceivedAIRole == "Primarily as an agent trying to influence or persuade me in making determinations" ~ "Agent",
      PerceivedAIRole == "Neither as a tool nor as an influencing agent" ~ "Neither",
      PerceivedAIRole == "Unsure" ~ "Unsure",
      TRUE ~ PerceivedAIRole
    ),
    # Order the roles logically
    PerceivedAIRole_Clean = factor(PerceivedAIRole_Clean, 
                                   levels = c("Tool", "Mixed", "Agent", "Neither", "Unsure"))
  )

# Print summary table
cat("\nSummary Table:\n")
print(role_bias_summary)

# Define Nature-style theme
nature_theme <- theme_classic() +
  theme(
    text = element_text(family = "Avenir", size = 10),
    axis.title = element_text(family = "Avenir", size = 11, face = "plain"),
    axis.text = element_text(family = "Avenir", size = 10, color = "black"),
    axis.text.x = element_text(family = "Avenir", size = 10, color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.ticks.length = unit(0.15, "cm"),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    legend.title = element_text(family = "Avenir", size = 10, face = "bold"),
    legend.text = element_text(family = "Avenir", size = 9),
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(family = "Avenir", size = 10, face = "bold")
  )

# Define color palette (Nature-inspired, distinct colors)
role_colors <- c(
  "Tool"    = "#7A9A91",  # Muted sage/teal — darker passive tone
  "Mixed"   = "#B49A7D",  # Darker beige — midpoint with warmth
  "Agent"   = "#7B3F00",  # Deep sienna — strong, grounded agentive
  "Neither" = "#9C8C76",  # Dark taupe — off-axis but cohesive
  "Unsure"  = "#BEB3A7"   # Ashen sand — gentle, ambiguous
)

# Create vertical grouped bar chart with percentages
perceived_role_plot <- ggplot(role_bias_summary, aes(x = BiasCategory, y = Percentage, fill = PerceivedAIRole_Clean)) +
  geom_col(position = position_dodge(width = 0.9), alpha = 0.85, width = 0.8) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_dodge(width = 0.9),
            vjust = -0.5, size = 2.9, family = "Avenir") +
  scale_fill_manual(values = role_colors, name = "Perceived AI Role") +
  scale_x_discrete(expand = expansion(add = c(0.5, 0.5))) +
  scale_y_continuous(labels = label_percent(scale = 1),
                     expand = expansion(mult = c(0, 0.15)),
                     limits = c(0, max(role_bias_summary$Percentage) * 1.1)) +
  labs(
    x = "AI Bias Magnitude",
    y = "Percentage of Participants"
  ) +
  nature_theme +
  theme(
    # Legend inside the panel, upper right
    legend.position = c(0.98, 0.99),
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = scales::alpha("white", 0.7), color = NA),
    legend.key = element_rect(fill = NA, color = NA),
    legend.key.size = unit(0.35, "cm"),
    legend.margin = margin(2, 4, 2, 4),
    axis.title.y = element_text(margin = margin(r = 15)),
    axis.title.x = element_text(margin = margin(t = 10))
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE, title = ""))

if (interactive()) print(perceived_role_plot)
ggsave("../figures/second_figure_b2.png", perceived_role_plot, width = 9.2, height = 4.3, dpi = 500)
