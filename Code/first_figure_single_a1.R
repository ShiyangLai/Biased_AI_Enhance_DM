# Ensure proper factoring
single_ai_processed$StanceRelationship <- factor(single_ai_processed$StanceRelationship,
                                                 levels = c("Baseline", "Echo Chamber",
                                                            "Moderate Opposition", "Strong Opposition"))

single_ai_processed$PreCorrect <- ifelse(single_ai_processed$PreEva == df1$Truth, 1, 0)
single_ai_processed$PostCorrect <- ifelse(single_ai_processed$PostEva == df1$Truth, 1, 0)
single_ai_processed$IsDefault <- ifelse(single_ai_processed$AIStanceLabel == "Default", 1, 0)

# ========================================
# Main analysis
# ========================================
single_ai_processed_ <- single_ai_processed[single_ai_processed$AIStanceLabel_S != "Neutral",]
single_ai_processed_$BiasedType <- ifelse(single_ai_processed_$AIStanceLabel_S == "Default",
                                          "Non-Biased", "Biased")

single_ai_processed_$BiasedType <- factor(single_ai_processed_$BiasedType,
                                          levels = c("Non-Biased", "Biased"))

single_ai_model <- lm(PostPerformance ~ BiasedType + as.factor(NID) + PrePerformance +
                        AICorrectness,    # + as.factor(UIdeo) + AICorrectness
                      data = single_ai_processed_)

summary(single_ai_model)
vcov_clustered <- vcovCL(single_ai_model,
                        cluster = single_ai_processed_$UID)
clustered_results <- coeftest(single_ai_model, vcov = vcov_clustered)
print(clustered_results)

single_ai_model <- lmer(PostPerformance ~ PrePerformance + BiasedType + as.factor(NID) + 
                          (1|UID),  # + as.factor(UIdeo) + AICorrectness
                        data = single_ai_processed_)
summary(single_ai_model)
summary(single_ai_model)$sigma 
r2(single_ai_model)

# ========================================
# Separate analysis Rep vs Dem
# ========================================
single_ai_model <- lm(PostPerformance ~ PrePerformance + AIStanceLabel + as.factor(NID),
                        data = single_ai_processed_[single_ai_processed_$AIStanceLabel_S != "Republican", ])
summary(single_ai_model)

vcov_clustered <- vcovCL(single_ai_model,
                         cluster = single_ai_processed_[single_ai_processed_$AIStanceLabel_S != "Republican", ]$UID)
clustered_results <- coeftest(single_ai_model, vcov = vcov_clustered)
print(clustered_results)

single_ai_model <- lmer(PostPerformance ~ PrePerformance + AIStanceLabel + as.factor(NID) + (1|UID),
                      data = single_ai_processed_[single_ai_processed_$AIStanceLabel_S != "Republican", ])
summary(single_ai_model)
summary(single_ai_model)$sigma 
r2(single_ai_model)

# ========================================
# Marginal mean analysis
# ========================================
emm <- emmeans(single_ai_model, ~ BiasedType)
contrasts <- pairs(emm, infer = TRUE)
print(contrasts)

# Extract contrast estimate
contrast_estimate <- summary(contrasts)$estimate

# For mixed effects, consider total variance
total_var <- VarCorr(single_ai_model)$UID[1] + sigma(single_ai_model)^2
pooled_sd <- sqrt(total_var)

# Calculate effect size
cohens_d <- contrast_estimate / pooled_sd

# Use degrees of freedom from emmeans
df_emmeans <- summary(contrasts)$df
J <- 1 - (3 / (4 * df_emmeans - 1))
hedges_g <- cohens_d * J
print(paste("Hedges' g:", hedges_g))

# ========================================
# Casual mediation analysis
# ========================================
single_ai_processed_$NID_factor <- as.factor(single_ai_processed_$NID)

mediator_lm <- lm(AICorrectness ~ BiasedType + NID_factor, 
                  data = single_ai_processed_)

outcome_lm <- lm(PostPerformance ~ BiasedType + AICorrectness + NID_factor, 
                 data = single_ai_processed_)

total_lm <- lm(PostPerformance ~ BiasedType + NID_factor, 
               data = single_ai_processed_)

mediation_results_clustered <- mediate(mediator_lm, outcome_lm, 
                                       treat = "BiasedType", 
                                       mediator = "AICorrectness",
                                       boot = FALSE, 
                                       sims = 1000,
                                       cluster = single_ai_processed_$UID)

summary(mediation_results_clustered)

# Get emmeans for BiasedType levels
emm_single <- emmeans(single_ai_model, ~ BiasedType, vcov. = vcov_clustered)
emm_single_summary <- summary(emm_single)

print(emm_single_summary)

# Calculate difference between groups
single_ai_contrast <- pairs(emm_single)
print(single_ai_contrast)

# ========================================
# Prepare data for visualization
# ========================================
# Single AI plot data
single_ai_plot_data <- data.frame(
  BiasedType = emm_single_summary$BiasedType,
  MeanPerformanceChange = emm_single_summary$emmean,
  SE_PerformanceChange = emm_single_summary$SE,
  y_position = ifelse(emm_single_summary$BiasedType == "Biased", 2, 1)
) %>%
  mutate(
    CI_90_lower = MeanPerformanceChange - SE_PerformanceChange * qnorm(0.95),
    CI_90_upper = MeanPerformanceChange + SE_PerformanceChange * qnorm(0.95),
    CI_95_lower = MeanPerformanceChange - SE_PerformanceChange * qnorm(0.975),
    CI_95_upper = MeanPerformanceChange + SE_PerformanceChange * qnorm(0.975),
    CI_99_lower = MeanPerformanceChange - SE_PerformanceChange * qnorm(0.995),
    CI_99_upper = MeanPerformanceChange + SE_PerformanceChange * qnorm(0.995),
    BiasedType_Legend = case_when(
      BiasedType == "Biased" ~ "Republican/Democrat",
      BiasedType == "Non-Biased" ~ "Standard"
    )
  )

# ========================================
# Visualization
# ========================================
nature_theme <- theme_classic() +
  theme(
    text = element_text(family = "Avenir", size = 8),
    plot.title = element_text(family = "Avenir", size = 10, face = "bold", hjust = 0),
    axis.title = element_text(family = "Avenir", size = 9, face = "plain"),
    axis.text = element_text(family = "Avenir", size = 8, color = "black"),
    axis.text.y = element_text(family = "Avenir", size = 9, color = "black", face = "plain"),
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.ticks.length = unit(0.15, "cm"),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 10)
  )

bias_colors <- c(
  "Republican/Democrat" = "#E15759",
  "Standard" = "#4E79A7"
)

single_ai_plot <- ggplot(single_ai_plot_data, aes(y = y_position)) +
  geom_errorbarh(aes(xmin = CI_99_lower, xmax = CI_99_upper, color = BiasedType_Legend),
                 height = 0.25, size = 1.2, alpha = 0.3) +
  geom_errorbarh(aes(xmin = CI_95_lower, xmax = CI_95_upper, color = BiasedType_Legend),
                 height = 0.2, size = 0.9, alpha = 0.5) +
  geom_errorbarh(aes(xmin = CI_90_lower, xmax = CI_90_upper, color = BiasedType_Legend),
                 height = 0.15, size = 0.7, alpha = 0.8) +
  geom_point(aes(x = MeanPerformanceChange, color = BiasedType_Legend),
             size = 3, alpha = 0.9) +
  scale_color_manual(values = bias_colors) +
  scale_y_continuous(breaks = c(2, 1), labels = c("Biased", "Non-Biased"),
                     expand = expansion(add = c(0.3, 0.3))) +
  scale_x_continuous(labels = label_number(accuracy = 0.01)) +
  labs(x = "Post-Interaction Performance", 
       y = NULL) +
  xlim(0.55, 0.71) +
  nature_theme +
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        legend.text = element_text(family = "Avenir", size = 9),
        axis.text.y = element_text(angle = 90, hjust = 0.5))

# Print the plot
print(single_ai_plot)