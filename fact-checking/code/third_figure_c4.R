bias_model <- lm(PostPerformance ~ PrePerformance + BiasSide * PoliBias + 
                   as.factor(AIStanceLabel_S) + as.factor(UStanceLabel) +
                   as.factor(NID), data = repdem_single_ai)
vcov_clustered <- vcovCL(bias_model, cluster = repdem_single_ai$UID)
emm_by_group <- emmeans(bias_model, ~ BiasSide * PoliBias, vcov. = vcov_clustered)

performance_by_group <- as.data.frame(emm_by_group) %>%
  rename(MeanImprovement = emmean) %>%
  mutate(Count = NA) # We'll add counts separately

counts <- repdem_single_ai %>%
  group_by(BiasSide, PoliBias) %>%
  summarise(Count = n(), .groups = 'drop')

performance_by_group <- performance_by_group %>%
  left_join(counts, by = c("BiasSide", "PoliBias"))

emm_by_bias_type <- emmeans(bias_model, ~ PoliBias | BiasSide, vcov. = vcov_clustered)

# Calculate gaps with proper clustered SEs
rep_vs_dem_gap <- contrast(emm_by_bias_type, list("Rep-Dem" = c(-1, 0, 1)), by = "BiasSide")
rep_vs_neutral_gap <- contrast(emm_by_bias_type, list("Rep-Neutral" = c(0, -1, 1)), by = "BiasSide")
dem_vs_neutral_gap <- contrast(emm_by_bias_type, list("Dem-Neutral" = c(1, -1, 0)), by = "BiasSide")

# Convert gap results to dataframe format 
rep_vs_dem_df <- as.data.frame(rep_vs_dem_gap)
rep_vs_neutral_df <- as.data.frame(rep_vs_neutral_gap)
dem_vs_neutral_df <- as.data.frame(dem_vs_neutral_gap)

# Create gaps summary dataframe
gaps_df <- data.frame(
  BiasSide = rep_vs_dem_df$BiasSide,
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
  dplyr::select(BiasSide, PoliBias, MeanImprovement) %>%
  pivot_wider(names_from = PoliBias, values_from = MeanImprovement) %>%
  left_join(gaps_df, by = "BiasSide")

clustered_coef_test <- coeftest(bias_model, vcov = vcov_clustered)
pairwise_within_polibias <- pairs(emm_by_bias_type, by = "PoliBias")


# Calculate Average Absolute Political Bias metric
avg_abs_bias <- gaps_df %>%
  mutate(
    # Calculate average absolute political bias across all three comparisons
    Avg_Abs_Political_Bias = (abs(Rep_vs_Dem_Gap) + abs(Rep_vs_Neutral_Gap) + abs(Dem_vs_Neutral_Gap)) / 3,
    
    # For standard error, use conservative approach (max SE among the three)
    Avg_Abs_SE = pmax(Rep_vs_Dem_SE, Rep_vs_Neutral_SE, Dem_vs_Neutral_SE),
    
    # Numeric position for plotting
    bias_side_numeric = case_when(
      BiasSide == "Opposite" ~ 0,
      BiasSide == "Same" ~ 1,
      TRUE ~ NA_real_
    ),
    
    # Calculate degrees of freedom
    df = length(unique(repdem_single_ai$UID)) - 1
  )

# Calculate t-values for different confidence levels
df_clustered <- length(unique(repdem_single_ai$UID)) - 1
t_90 <- qt(0.95, df_clustered)   # 90% CI
t_95 <- qt(0.975, df_clustered)  # 95% CI  
t_99 <- qt(0.995, df_clustered)  # 99% CI

# Define deeper color palette
base_color <- "#535e3c"    # Darkest (base color)
color_90 <- "#747d63"      # 90% CI (slightly lighter)
color_95 <- "#909683"      # 95% CI (medium lighter)
color_99 <- "#d0d4c8"      # 99% CI (lightest)

# Define box widths for different confidence levels
box_width_base <- 0.08
box_width_90 <- box_width_base * 1.4   # Narrowest (90% CI)
box_width_95 <- box_width_base * 1.   # Medium (95% CI)  
box_width_99 <- box_width_base * 0.7   # Widest (99% CI)

# Add multiple confidence intervals and box coordinates
avg_bias_plot <- avg_abs_bias %>%
  mutate(
    # Calculate different CI levels for average absolute bias
    lower.CL_90 = Avg_Abs_Political_Bias - Avg_Abs_SE * t_90,
    upper.CL_90 = Avg_Abs_Political_Bias + Avg_Abs_SE * t_90,
    lower.CL_95 = Avg_Abs_Political_Bias - Avg_Abs_SE * t_95,
    upper.CL_95 = Avg_Abs_Political_Bias + Avg_Abs_SE * t_95,
    lower.CL_99 = Avg_Abs_Political_Bias - Avg_Abs_SE * t_99,
    upper.CL_99 = Avg_Abs_Political_Bias + Avg_Abs_SE * t_99,
    
    # Ensure lower bounds don't go below 0 (since we're dealing with absolute values)
    lower.CL_90 = pmax(0, lower.CL_90),
    lower.CL_95 = pmax(0, lower.CL_95),
    lower.CL_99 = pmax(0, lower.CL_99),
    
    # Box coordinates for different CI levels
    xmin_90 = bias_side_numeric - box_width_90,
    xmax_90 = bias_side_numeric + box_width_90,
    xmin_95 = bias_side_numeric - box_width_95,
    xmax_95 = bias_side_numeric + box_width_95,
    xmin_99 = bias_side_numeric - box_width_99,
    xmax_99 = bias_side_numeric + box_width_99
  )

# Create the boxen-style plot for average absolute political bias
p_avg_abs_bias <- ggplot() +
  # 99% CI boxes (widest, lightest) - BACK LAYER
  geom_rect(data = avg_bias_plot,
            aes(xmin = xmin_99, xmax = xmax_99, ymin = lower.CL_99, ymax = upper.CL_99),
            fill = color_99, alpha = 1, color = color_99, linewidth = 0) +
  
  # 95% CI boxes (medium) - MIDDLE LAYER  
  geom_rect(data = avg_bias_plot,
            aes(xmin = xmin_95, xmax = xmax_95, ymin = lower.CL_95, ymax = upper.CL_95),
            fill = color_95, alpha = 1, color = color_95, linewidth = 0) +
  
  # 90% CI boxes (narrowest, darkest) - FRONT LAYER
  geom_rect(data = avg_bias_plot,
            aes(xmin = xmin_90, xmax = xmax_90, ymin = lower.CL_90, ymax = upper.CL_90),
            fill = color_90, alpha = 1, color = color_90, linewidth = 0) +
  
  # Connect the two points with a line
  geom_line(data = avg_bias_plot,
            aes(x = bias_side_numeric, y = Avg_Abs_Political_Bias),
            color = base_color, linewidth = 1.5, alpha = 0.8) +
  
  # Points for estimated values
  geom_point(data = avg_bias_plot,
             aes(x = bias_side_numeric, y = Avg_Abs_Political_Bias),
             color = base_color, size = 5, shape = 20, stroke = 1) +
  
  # Scales and labels
  scale_y_continuous(
    name = "Average Absolute Political Bias",
    labels = scales::number_format(accuracy = 0.01),
    limits = c(0, 0.23)
  ) +
  
  scale_x_continuous(
    name = "AI Bias Direction",
    breaks = c(0, 1),
    labels = c("Opposite to User", "Same as User"),
    limits = c(-0.4, 1.4)
  ) +
  
  # Theme
  theme_classic() +
  theme(
    text = element_text(family = "Avenir"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.line = element_blank(),
    axis.title.x = element_text(family = "Avenir", size = 12, margin = margin(t = 12)),
    axis.title.y = element_text(family = "Avenir", size = 12, color = "black", margin = margin(r = 10)),
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
print(p_avg_abs_bias)

# Create summary table for average absolute political bias
summary_avg_bias <- avg_abs_bias %>%
  dplyr::select(BiasSide, Avg_Abs_Political_Bias, Avg_Abs_SE) %>%
  mutate(
    Avg_Abs_Political_Bias = round(Avg_Abs_Political_Bias, 4),
    Avg_Abs_SE = round(Avg_Abs_SE, 4),
    CI_95_lower = round(Avg_Abs_Political_Bias - Avg_Abs_SE * t_95, 4),
    CI_95_upper = round(Avg_Abs_Political_Bias + Avg_Abs_SE * t_95, 4),
    CI_95 = paste0("[", pmax(0, CI_95_lower), ", ", CI_95_upper, "]")
  ) %>%
  dplyr::select(BiasSide, Avg_Abs_Political_Bias, Avg_Abs_SE, CI_95)

cat("\n=== AVERAGE ABSOLUTE POLITICAL BIAS SUMMARY ===\n")
print(summary_avg_bias)

# Calculate the difference between Opposite and Same conditions
bias_difference <- avg_abs_bias$Avg_Abs_Political_Bias[avg_abs_bias$BiasSide == "Same"] - 
  avg_abs_bias$Avg_Abs_Political_Bias[avg_abs_bias$BiasSide == "Opposite"]

cat("\n=== BIAS SIDE COMPARISON ===\n")
cat(paste("Difference (Same - Opposite):", round(bias_difference, 4), "\n"))
if(bias_difference > 0) {
  cat("Political bias is HIGHER when AI bias is in the SAME direction as user\n")
} else {
  cat("Political bias is HIGHER when AI bias is in the OPPOSITE direction to user\n")
}

# Show the individual components that make up the average
cat("\n=== COMPONENT GAPS (for reference) ===\n")
component_table <- gaps_df %>%
  dplyr::select(BiasSide, Rep_vs_Dem_Gap, Rep_vs_Neutral_Gap, Dem_vs_Neutral_Gap) %>%
  mutate(
    Rep_vs_Dem_Gap = round(Rep_vs_Dem_Gap, 4),
    Rep_vs_Neutral_Gap = round(Rep_vs_Neutral_Gap, 4),
    Dem_vs_Neutral_Gap = round(Dem_vs_Neutral_Gap, 4),
    Abs_Rep_vs_Dem = round(abs(Rep_vs_Dem_Gap), 4),
    Abs_Rep_vs_Neutral = round(abs(Rep_vs_Neutral_Gap), 4),
    Abs_Dem_vs_Neutral = round(abs(Dem_vs_Neutral_Gap), 4)
  )

print(component_table)
