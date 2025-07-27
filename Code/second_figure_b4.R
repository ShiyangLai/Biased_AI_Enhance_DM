# Parameters -------------------------------------------------------------
G <- 1.0        # payoff if good
L <- 1.0        # loss if bad
c <- 0.25       # evaluation cost
q <- 0.9        # Pr(signal good | good)
r <- 0.1        # Pr(signal good | bad)

# Epsilon cases for perceived bias
epsilons <- list(
  "Low Perceived Bias"  = list(epsilon = 0.5,  linetype = "solid",  color = "#2E86AB"),
  "High Perceived Bias" = list(epsilon = 0.05, linetype = "longdash", color = "#F24236")
)

# Value function
valuationfunction <- function(p, epsilon) {
  pmax(pmin(1, (1 + epsilon) * p), 0)
}

# Main simulation grid
p <- seq(0, 1, length.out = 1000)

# Enhanced data preparation with cutoff identification
plot_data <- bind_rows(
  lapply(names(epsilons), function(label_base) {
    eps <- epsilons[[label_base]]$epsilon
    linetype <- epsilons[[label_base]]$linetype
    color <- epsilons[[label_base]]$color
    
    v <- valuationfunction(p, eps)
    E_eval   <- v * q * G - (1 - v) * r * L - c
    E_accept <- v * G - (1 - v) * L
    evaluate <- E_eval >= E_accept
    
    success_rate <- numeric(length(p))
    success_rate[evaluate] <- (p[evaluate] * q) / (p[evaluate] * q + (1 - p[evaluate]) * r)
    success_rate[!evaluate] <- p[!evaluate]
    
    # Find cutoff p* (where evaluate switches from TRUE to FALSE)
    eval_diff <- diff(as.integer(evaluate))
    p_star_candidates <- p[which(eval_diff < 0)]
    p_star <- if(length(p_star_candidates) > 0) p_star_candidates[1] else NA_real_
    
    label <- if(!is.na(p_star)) {
      sprintf("%s\n      (p* = %.2f)", label_base, p_star)
    } else {
      label_base
    }
    
    tibble(
      p = p,
      success_rate = success_rate,
      scenario = label,
      line_type = linetype,
      line_color = color,
      p_star = p_star,
      evaluate = evaluate,
      epsilon = eps
    )
  })
)

# Add 45-degree line (LLMs quality)
llm_ref <- tibble(
  p = p,
  success_rate = p,
  scenario = "    LLM baseline",
  line_type = "dotted",
  line_color = "#757575",
  p_star = NA_real_,
  evaluate = TRUE,
  epsilon = NA_real_
)

full_plot_data <- bind_rows(plot_data, llm_ref)

# Get actual scenario names and reorder them
actual_scenarios <- unique(full_plot_data$scenario)
llm_scenario <- actual_scenarios[grepl("LLM baseline", actual_scenarios)]
low_bias_scenario <- actual_scenarios[grepl("Low Perceived Bias", actual_scenarios)]
high_bias_scenario <- actual_scenarios[grepl("High Perceived Bias", actual_scenarios)]

# Reorder legend by converting scenario to factor with desired order
desired_order <- c(llm_scenario, low_bias_scenario, high_bias_scenario)
full_plot_data$scenario <- factor(full_plot_data$scenario, levels = desired_order)

# Extract cutoff points for annotations
cutoff_data <- plot_data %>%
  filter(!is.na(p_star)) %>%
  group_by(scenario) %>%
  summarise(
    p_star = first(p_star),
    line_color = first(line_color),
    epsilon = first(epsilon),
    .groups = "drop"
  ) %>%
  mutate(
    cutoff_y = p_star  # At cutoff, success rate equals true quality (45-degree line)
  )

# Reorder cutoff_data scenario to match the factor levels
cutoff_data$scenario <- factor(cutoff_data$scenario, levels = desired_order)

# Create evaluation regime data for shading
regime_data <- plot_data %>%
  group_by(scenario, epsilon) %>%
  summarise(
    p_min = min(p[evaluate], na.rm = TRUE),
    p_max = max(p[evaluate], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(epsilon))

# Reorder regime_data scenario to match the factor levels
regime_data$scenario <- factor(regime_data$scenario, levels = desired_order)

# Get the actual scenario names and styles from the data (respecting factor order)
scenario_names <- levels(full_plot_data$scenario)
colors <- c()
linetypes <- c()

for(scenario in scenario_names) {
  row <- full_plot_data[full_plot_data$scenario == scenario, ][1, ]
  colors[scenario] <- row$line_color
  linetypes[scenario] <- row$line_type
}

# Sophisticated Nature-style theme with Avenir font
nature_theme <- theme_classic(base_size = 11, base_family = "Avenir") +
  theme(
    # Font settings
    text = element_text(family = "Avenir"),
    
    # Remove titles
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    plot.caption = element_blank(),
    
    # Axis styling with complete border frame
    axis.title = element_text(size = 12, color = "black", family = "Avenir"),
    axis.title.x = element_text(margin = margin(t=8)),
    axis.title.y = element_text(margin = margin(r=8)),
    axis.text = element_text(size = 10, color = "black", family = "Avenir"),
    axis.line = element_blank(), # Remove default axis lines
    axis.ticks = element_line(color = "black", size = 0.4),
    axis.ticks.length = unit(0.15, "cm"),
    
    # Complete border frame
    panel.border = element_rect(color = "black", fill = NA, size = 0.8),
    
    # Legend styling - moved to bottom right
    legend.position = c(0.98, 0.005),
    legend.justification = c(1, 0),
    legend.title = element_blank(),
    legend.text = element_text(size = 10, color = "black", family = "Avenir"),
    legend.key.width = unit(1.5, "lines"),
    legend.key.height = unit(0.6, "lines"),
    legend.key = element_blank(),
    # legend.background = element_blank(),
    legend.margin = margin(t = 3, r = 3, b = 8, l = 3),
    legend.box.margin = margin(0),
    legend.spacing.y = unit(0.5, "lines"),
    
    # Panel styling without grid
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    
    # Enhanced margins
    plot.margin = margin(t = 20, r = 20, b = 15, l = 20)
  )

# Create the sophisticated plot
p1 <- ggplot(full_plot_data, aes(x = p, y = success_rate)) +
  
  # Add subtle shaded regions for evaluation regimes
  geom_rect(data = regime_data,
            aes(xmin = p_min, xmax = p_max, ymin = -Inf, ymax = Inf, fill = scenario),
            alpha = 0.08, inherit.aes = FALSE) +
  scale_fill_manual(values = colors, guide = "none") +
  
  # Add performance curves with enhanced styling
  geom_line(aes(color = scenario, linetype = scenario), 
            size = 1.1, alpha = 0.95) +
  
  # Add cutoff point indicators
  geom_vline(data = cutoff_data, 
             aes(xintercept = p_star, color = scenario),
             linetype = "solid", alpha = 0.4, size = 0.6) +
  
  # Add cutoff point markers
  geom_point(data = cutoff_data,
             aes(x = p_star, y = cutoff_y, color = scenario),
             size = 4, alpha = 0.8, stroke = 0) +
  
  # Color and linetype scales
  scale_color_manual(values = colors, 
                     name = NULL,
                     guide = guide_legend(override.aes = list(size = 1.2))) +
  scale_linetype_manual(values = linetypes, name = NULL) +
  
  # Enhanced axis styling
  labs(
    x = "True Quality (p)",
    y = "Expected Success Rate"
  ) +
  
  # Refined axis limits and breaks
  scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    expand = c(0.00, 0.0),
    labels = function(x) sprintf("%.1f", x)
  ) +
  coord_cartesian(xlim = c(0., 1)) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    expand = c(0.01, 0.01),
    labels = function(x) sprintf("%.1f", x)
  ) +
  
  # Add subtle annotations for evaluation regimes
  annotate("text", x = 0.15, y = 0.95, 
           label = "Evaluate", 
           size = 4, color = "grey20", 
           family = "Avenir", fontface = "italic", alpha = 0.9) +
  
  annotate("text", x = 0.78, y = 0.95, 
           label = "Accept directly", 
           size = 4, color = "grey20", 
           family = "Avenir", fontface = "italic", alpha = 0.9) +
  
  # Apply the sophisticated theme
  nature_theme +
  
  # Fine-tune legend appearance
  guides(
    color = guide_legend(
      override.aes = list(size = 1.2, alpha = 1),
      direction = "horizontal",
      nrow = 3
    ),
    linetype = "none"  # Don't show separate linetype legend
  )

# Display the enhanced plot
print(p1)

