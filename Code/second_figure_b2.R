# Check available data
cat("Unique values in BiasedCat:", paste(unique(complete_data$BiasedCat), collapse = ", "), "\n")
cat("AICorrectness summary:\n")
print(summary(complete_data$AICorrectness))

# Ensure BiasedCat is properly ordered as a factor
complete_data$BiasedCat <- factor(complete_data$BiasedCat, 
                                  levels = c("No Bias", "Moderate Bias", "Strong Bias"))

# Check the factor levels
cat("BiasedCat levels:", paste(levels(complete_data$BiasedCat), collapse = ", "), "\n")

# Fit the AI correctness model
model_ai <- lm(AICorrectness ~ BiasedCat + as.factor(NID), 
               data = complete_data)

model <- clm(
  as.factor(AIStanceLabel_S) ~ AICorrectness + factor(NID),
  data = subset(complete_data, BiasedType == "Biased")
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

cat("\n=== PAIRWISE COMPARISONS (BONFERRONI ADJUSTED) ===\n")
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
    BiasedCat = factor(BiasedCat, levels = c("No Bias", "Moderate Bias", "Strong Bias"))
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
  "No Bias" = "#4E79A7",       # Blue
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
    name = "AI Correctness",
    labels = scales::number_format(accuracy = 0.01),
    expand = expansion(mult = c(0, 0.1))
  ) +
  
  labs(
    x = "AI Bias Magnitude"
  ) +
  
  theme_classic() +
  theme(
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.line = element_blank(),
    text = element_text(family = "Arial", color = "black"),
    axis.title.x = element_text(family = "Arial", size = 13.5, margin = margin(t = 12)),
    axis.title.y = element_text(family = "Arial", size = 13.5, color = "black", margin = margin(r = 10)),
    axis.text.x = element_text(family = "Arial", size = 11.7, color = "black", margin = margin(t = 4)),
    axis.text.y = element_text(family = "Arial", size = 11.7, color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    axis.ticks.length = unit(3.5, "pt"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )

# Display the plot
print(p_ai_correctness)

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

cat("\nPairwise Comparisons (Bonferroni Adjusted):\n")
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

# Calculate effect sizes relative to No Bias
no_bias_est <- emm_ai_df$emmean[emm_ai_df$BiasedCat == "No Bias"]
moderate_est <- emm_ai_df$emmean[emm_ai_df$BiasedCat == "Moderate Bias"]
strong_est <- emm_ai_df$emmean[emm_ai_df$BiasedCat == "Strong Bias"]

cat("Effect sizes (relative to No Bias baseline):\n")
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
    BiasedCat = factor(BiasedCat, levels = c("No Bias", "Moderate Bias", "Strong Bias")),
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
  filter(!is.na(BiasedCat) & !is.na(AICorrectness)) %>%
  mutate(BiasedCat = factor(BiasedCat, levels = c("No Bias", "Moderate Bias", "Strong Bias")))

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

# Create probability distribution plot showing AI Correctness by bias category
# First, let's calculate proper probability distributions manually

# Create a grid of x values
x_range <- seq(min(plot_data$AICorrectness, na.rm = TRUE), 
               max(plot_data$AICorrectness, na.rm = TRUE), 
               length.out = 512)

# Calculate density for each group and normalize
density_data <- plot_data %>%
  split(.$BiasedCat) %>%
  map_dfr(function(group_data) {
    if(nrow(group_data) > 1) {
      # Calculate density
      dens <- density(group_data$AICorrectness, 
                      from = min(x_range), 
                      to = max(x_range),
                      n = length(x_range))
      
      # Create data frame
      data.frame(
        x = dens$x,
        y = dens$y,
        BiasedCat = unique(group_data$BiasedCat)[1]
      )
    }
  }) %>%
  # Ensure factor ordering
  mutate(BiasedCat = factor(BiasedCat, levels = c("No Bias", "Moderate Bias", "Strong Bias")))

# Verify that areas integrate to approximately 1
cat("Checking area under curves:\n")
for(bias_cat in levels(density_data$BiasedCat)) {
  subset_data <- density_data[density_data$BiasedCat == bias_cat, ]
  if(nrow(subset_data) > 1) {
    # Calculate area using trapezoidal rule
    dx <- diff(subset_data$x)[1]
    area <- sum(subset_data$y) * dx
    cat(sprintf("%s: Area ≈ %.3f\n", bias_cat, area))
  }
}

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
    name = "AI Correctness",
    expand = expansion(mult = c(0, 0.0)),
    labels = scales::number_format(accuracy = 0.01)
  ) +
  
  scale_y_continuous(
    name = "Probability Density",
    expand = expansion(mult = c(0, 0.05)),
    labels = scales::number_format(accuracy = 0.01)
  ) +
  
  # Nature journal styling
  theme_classic() +
  theme(
    # Panel and borders
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7),
    axis.line = element_blank(),
    
    # Text styling - Avenir font family
    text = element_text(family = "Avenir", color = "black"),
    plot.title = element_text(family = "Avenir", size = 12, hjust = 0.5, 
                              margin = margin(b = 8), face = "bold"),
    plot.subtitle = element_text(family = "Avenir", size = 10, hjust = 0.5, 
                                 color = "gray40", margin = margin(b = 12)),
    
    # Axis styling
    axis.title.x = element_text(family = "Avenir", size = 11.5, 
                                margin = margin(t = 8), face = "bold"),
    axis.title.y = element_text(family = "Avenir", size = 11.5, color = "black", 
                                margin = margin(r = 8), face = "bold"),
    axis.text.x = element_text(family = "Avenir", size = 10.5, color = "black", 
                               margin = margin(t = 3)),
    axis.text.y = element_text(family = "Avenir", size = 10.5, color = "black"),
    
    # Axis ticks
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    axis.ticks.length = unit(3, "pt"),
    
    # Background
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    
    # Legend inside plot (top-right corner)
    legend.position = c(0.6, 0.95),
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.),
    legend.margin = margin(6, 6, 6, 6),
    legend.key.size = unit(0.8, "cm"),
    legend.key = element_rect(fill = "white", color = NA),
    legend.title = element_blank(),
    legend.text = element_text(family = "Avenir", size = 11),
    legend.spacing.y = unit(0.5, "cm"),
    legend.key.height = unit(0.5, "cm"),  # Controls spacing between legend items
    
    # Margins
    plot.margin = margin(t = 12, r = 12, b = 10, l = 12)
  )

# Display the probability distribution plot
print(p_distribution)

# =====================================
# Perceived role visualization
# =====================================
# Create bias categories from AIStanceLabel
single_ai_processed_ <- single_ai_processed_ %>%
  mutate(
    BiasCategory = case_when(
      AIStanceLabel %in% c("Default", "Politically Neutral") ~ "No Bias",
      AIStanceLabel %in% c("Somewhat Republican", "Somewhat Democrat") ~ "Moderate Bias",
      AIStanceLabel %in% c("Strong Republican", "Strong Democrat") ~ "Strong Bias",
      TRUE ~ NA_character_
    ),
    # Order the bias categories
    BiasCategory = factor(BiasCategory, levels = c("Strong Bias", "Moderate Bias", "No Bias"))
  )

# Check the distribution
cat("Bias Category Distribution:\n")
table(single_ai_processed_$BiasCategory, useNA = "ifany")

# Calculate counts and percentages for each combination
role_bias_summary <- single_ai_processed_ %>%
  filter(!is.na(BiasCategory)) %>%
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
  geom_col(position = position_dodge(width = 0.8), alpha = 0.85, width = 0.75) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_dodge(width = 0.8), 
            hjust = -0.2, size = 3, family = "Avenir") +
  scale_fill_manual(values = role_colors, name = "Perceived AI Role") +
  scale_x_discrete(expand = expansion(add = c(0.5, 0.5))) +
  scale_y_continuous(labels = label_percent(scale = 1), 
                     expand = expansion(mult = c(0, 0.15)),
                     limits = c(0, max(role_bias_summary$Percentage) * 1.1)) +
  labs(
    x = "AI Bias Magnitude",
    y = "Percentage of Participants"
  ) +
  coord_flip() +
  nature_theme +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(family = "Avenir", size = 10, color = "black", angle = 90, hjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 15)),
    axis.title.x = element_text(margin = margin(t = 10))
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE, title = ""))

print(perceived_role_plot)
