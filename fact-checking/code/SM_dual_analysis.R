##########################
# Perceived role analysis
##########################

# Check the distribution
cat("Bias Category Distribution:\n")
table(combined_data$ExperimentType, useNA = "ifany")

# Calculate counts and percentages for each combination
role_bias_summary <- combined_data %>% 
  dplyr::filter(!is.na(ExperimentType)) %>%                              
  # ---- tidy the role labels first ----
mutate(
  PerceivedAIRole_Clean = case_when(
    PerceivedAIRole == "Mostly as a tool to assist me in making my own determinations" ~ "Tool",
    PerceivedAIRole == "A mix of both a tool and an influencing agent"                ~ "Mixed",
    PerceivedAIRole == "Primarily as an agent trying to influence or persuade me in making determinations" ~ "Agent",
    PerceivedAIRole == "Neither as a tool nor as an influencing agent"                ~ "Neither",
    PerceivedAIRole == "Unsure"                                                      ~ "Unsure",
    TRUE                                                                             ~ PerceivedAIRole
  ),
  PerceivedAIRole_Clean = factor(
    PerceivedAIRole_Clean,
    levels = c("Tool", "Mixed", "Agent", "Neither", "Unsure")
  )
) %>% 
  # ---- count every combo ----
count(ExperimentType, PerceivedAIRole_Clean, name = "Count") %>% 
  # ---- insert the zero-count cells that are missing ----
complete(
  ExperimentType,
  PerceivedAIRole_Clean,
  fill = list(Count = 0)
) %>% 
  # ---- compute % within each ExperimentType ----
group_by(ExperimentType) %>% 
  mutate(
    Percentage = Count / sum(Count) * 100,
    Total      = sum(Count)
  ) %>% 
  ungroup()


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

exp_labels <- c(
  "Single_AI_Non_Biased"      = "Single AI\nNon-Biased",
  "Single_AI_Non_Biased_Exp"   = "Single AI\nNeutralized",
  "Single_AI_Opposition" = "Single AI\nOpposition",
  "Dual_AI_Non_Biased" = "Dual AI\nNon-Biased",
  "Dual_AI_Non_Biased_Exp" = "Dual AI\nNeutralized",
  "Dual_AI_Opposition" = "Dual AI\nOpposition",
  "Dual_AI_Balanced" = "Dual AI\nBalanced"
)

perceived_role_plot <- ggplot(role_bias_summary,
                              aes(x = ExperimentType, y = Percentage, fill = PerceivedAIRole_Clean)) +
  geom_col(position = position_dodge(width = 0.8), alpha = 0.85, width = 0.75) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_dodge(width = 0.8),
            hjust = -0.2, size = 3, family = "Avenir") +
  scale_fill_manual(values = role_colors, name = "Perceived AI Role") +
  scale_x_discrete(
    limits  = names(exp_labels),   # keep desired order
    labels  = exp_labels,          # apply new tick labels
    expand  = expansion(add = c(0.5, 0.5))
  ) +
  scale_y_continuous(
    labels  = label_percent(scale = 1),
    expand  = expansion(mult = c(0, 0.15)),
    limits  = c(0, max(role_bias_summary$Percentage) * 1.1)
  ) +
  labs(x = "Treatment Type", y = "Percentage of Participants") +
  coord_flip() +
  nature_theme +
  theme(
    legend.position = "bottom",
    axis.text.y  = element_text(family = "Avenir", size = 10, color = "black"),
    axis.title.y = element_text(margin = margin(r = 15)),
    axis.title.x = element_text(margin = margin(t = 10))
  ) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE, title = ""))

print(perceived_role_plot)


##############################
# Inconsistency analysis
##############################
columns_to_keep_2 <- c("PerceivedImproveCode", "PostPerformance", "PrePerformance",
                       # AI_Combo_Numeric and BiasedType dropped: neither is produced by
                       # the R preprocessing (AI_Combo_Numeric came from the old Python
                       # pipeline), and neither is used downstream here.
                       "NID", "UStanceLabel", "UID",
                       "AI1StanceCode", "AI2StanceCode", "UStanceCode", "ConvLength",
                       "PoliBias", "AI1StanceLabel_S", "AI2StanceLabel_S",
                       "AI1Correctness", "AI2Correctness", "AIInterMean",
                       "PostCorrect", "PreCorrect", "PostConfCode", "PreConfCode", "UIdeo",
                       "EngagementBehavioral",
                       "EngagementCognitive", "EngagementEmotional",
                       "EngagementAutonomy", "EngagementSocialPresence",
                       "PerceivedAIRole", "AI1Eva", "AI2Eva")

dual_ai_data <- df2_filled %>%
  dplyr::select(all_of(columns_to_keep_2)) %>%
  mutate(
    ExperimentType = case_when(
      # Non-Biased cases
      AI1StanceLabel_S == "Default" & AI2StanceLabel_S == "Default" ~ "Dual_AI_Non_Biased",
      AI1StanceLabel_S == "Neutral" & AI2StanceLabel_S == "Neutral" ~ "Dual_AI_Non_Biased_Exp",
      # Balanced: User stance is between AI1 and AI2 stances
      AI1StanceCode != AI2StanceCode &
        UStanceCode > pmin(AI1StanceCode, AI2StanceCode) &
        UStanceCode < pmax(AI1StanceCode, AI2StanceCode) ~ "Dual_AI_Balanced",
      # AI1StanceCode * AI2StanceCode < 0 ~ "Dual_AI_Balanced",
      # Opposition: User stance is different from the two AI stances direction
      (UStanceCode != 0) & 
        (AI1StanceCode != 0) & (AI2StanceCode != 0) &
        (sign(AI1StanceCode) == -sign(UStanceCode)) & 
        (sign(AI2StanceCode) == -sign(UStanceCode)) ~ "Dual_AI_Opposition",
      TRUE ~ "Dual_AI_Other"
    )
  )
dual_ai_data <- dual_ai_data[dual_ai_data$ExperimentType != "Dual_AI_Other", ]

dual_ai_data$ExperimentType <- factor(dual_ai_data$ExperimentType, 
                                       levels = c("Dual_AI_Non_Biased",
                                                  "Dual_AI_Non_Biased_Exp",
                                                  "Dual_AI_Opposition",
                                                  "Dual_AI_Balanced"))

dual_ai_data$inconsistency <- abs(dual_ai_data$AI1Eva - dual_ai_data$AI2Eva)
dual_ai_data$AICorrectness <- (dual_ai_data$AI1Correctness + dual_ai_data$AI2Correctness)/2

model <- lm(
  inconsistency ~ ExperimentType,
  data   = dual_ai_data
)

summary(model)

exp_labels <- c(
  "Dual_AI_Non_Biased" = "Dual AI\nNon-Biased",
  "Dual_AI_Non_Biased_Exp" = "Dual AI\nNeutralized",
  "Dual_AI_Opposition" = "Dual AI\nOpposition",
  "Dual_AI_Balanced" = "Dual AI\nBalanced"
)

pretty_dist_plot <- ggplot(
  dual_ai_data,
  aes(y = ExperimentType, x = inconsistency, fill = ExperimentType)
) +
  ## full violin (width scaled, slightly transparent)
  geom_violin(
    trim  = FALSE,                # show full tails
    scale = "width",              # all violins same width
    alpha = 0.6,
    colour = NA                   # no outline
  ) +
  ## quasirandom points (spread along y so they don’t overlap)
  geom_quasirandom(
    aes(colour = ExperimentType),
    width = 0.18,                 # horizontal spread
    size  = 1.2,
    alpha = 0.9,
    show.legend = FALSE
  ) +
  ## pallet-matching scales
  scale_fill_brewer(type = "qual", palette = "Set2", guide = "none") +
  scale_colour_brewer(type = "qual", palette = "Set2", guide = "none") +
  labs(
    x = "Dual-AI Evaluation Inconsistency Rate",
    y = "Treatment Type"
  ) +
  scale_y_discrete(
    limits = names(exp_labels),
    labels = exp_labels,
    expand = expansion(add = c(0.5, 0.5))
  ) +
  coord_cartesian(clip = "off") +
  nature_theme +
  theme(
    axis.text.y  = element_text(family = "Avenir", size = 10),
    axis.title.y = element_text(margin = margin(r = 15)),
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.line.x  = element_line(linewidth = 0.4)
  )

print(pretty_dist_plot)

model <- lmer(PostPerformance ~ PrePerformance + inconsistency + C(NID) + (1|UID),
            data = dual_ai_data, na.action = na.omit)
summary(model)

# 1 ── make sure variable types are correct ───────────────────────────
dual_ai_data <- dual_ai_data %>%
  mutate(
    inconsistency   = as.numeric(inconsistency),
    NID             = factor(NID),
    UID             = factor(UID),
    PrePerformance  = as.numeric(PrePerformance),
    PostPerformance = as.numeric(PostPerformance)
  )

# 2 ── refit: treat NID & UID as random effects ───────────────────────
model <- lmer(
  PostPerformance ~ PrePerformance + inconsistency + (1 | NID) + (1 | UID),
  data = dual_ai_data,
  na.action = na.omit
)
summary(model)

# 3 ── marginal predictions for inconsistency (random effects excluded) ─
pred_df <- ggpredict(model, terms = "inconsistency [all]", type = "fixed")
# `type = "fixed"` drops random effects so you get the population-level line

# 4 ── scatter + model line + CI ribbon ───────────────────────────────
scatter_plot <- ggplot(dual_ai_data,
                       aes(x = inconsistency, y = PostPerformance)) +
  geom_point(
             size = 1.6, alpha = 0.05) +
  geom_line(data = pred_df, aes(x = x, y = predicted),
            colour = "black", linewidth = 0.9) +
  geom_ribbon(data = pred_df,
              aes(x = x, ymin = conf.low, ymax = conf.high),
              fill = "black", alpha = 0.12, inherit.aes = FALSE) +
  scale_colour_brewer(type = "qual", palette = "Set2",
                      name = "Treatment") +
  labs(
    x = "Dual-AI Inconsistency Rate",
    y = "Post-Interaction Performance"
  ) +
  nature_theme +        # your existing Nature-style theme
  theme(
    legend.position = "none",
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 12))
  )

print(scatter_plot)
