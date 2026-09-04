# ==============================================================================
# perceived_ai_role.R — perceived AI role (tool vs influencing agent) by
# AI bias magnitude, single-AI investment experiment.
#
# Investment analog of the "Perceived role visualization" in the fact-checking
# study's second_figure_b2.R (same question wording, same five answer options):
# grouped vertical bars of within-arm percentages, same role palette/theme.
# Risk-Neutral arm excluded (mirrors the Neutral-treatment exclusion there).
#
# Inputs: active_m2_treatment_data.csv, perceived_ai_role.csv (participantId,
#         ai_group, wave) + the three single-AI wave Qualtrics exports (ai_role)
# Output: perceived_ai_role.png
#   setwd("investment/code"); source("_setup.R"); source("perceived_ai_role.R")
# ==============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(ggplot2); library(scales)
})

.args <- commandArgs(FALSE); .f <- sub("^--file=", "", .args[grep("^--file=", .args)])
SCRIPT_DIR <- if (length(.f)) dirname(normalizePath(.f)) else getwd()
rd <- function(f) read.csv(file.path(DATA_DIR, f), check.names = FALSE,
                           stringsAsFactors = FALSE)

# ── ai_role from the shipped, de-identified response file ────────────────────
roles <- rd("perceived_ai_role.csv") %>%
  filter(participantId != "", ai_role != "") %>%
  distinct(participantId, .keep_all = TRUE)

df <- rd("active_m2_treatment_data.csv") %>%
  filter(ai_group %in% c("Default", "Somewhat Risk-Averse", "Extremely Risk-Averse",
                         "Somewhat Risk-Seeking", "Extremely Risk-Seeking")) %>%
  inner_join(roles, by = "participantId") %>%
  mutate(
    BiasCategory = case_when(
      ai_group == "Default"        ~ "Default",
      grepl("Somewhat", ai_group)  ~ "Moderate Bias",
      TRUE                         ~ "Strong Bias"),
    BiasCategory = factor(BiasCategory,
                          levels = c("Default", "Moderate Bias", "Strong Bias")))

cat("Bias Category Distribution:\n")
print(table(df$BiasCategory, useNA = "ifany"))

# ── counts and within-arm percentages ─────────────────────────────────────────
role_bias_summary <- df %>%
  group_by(BiasCategory, ai_role) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(BiasCategory) %>%
  mutate(Percentage = Count / sum(Count) * 100,
         Total = sum(Count)) %>%
  ungroup() %>%
  mutate(
    PerceivedAIRole_Clean = case_when(
      ai_role == "Mostly as a tool to assist me in making my own determinations" ~ "Tool",
      ai_role == "A mix of both a tool and an influencing agent" ~ "Mixed",
      ai_role == "Primarily as an agent trying to influence or persuade me in making determinations" ~ "Agent",
      ai_role == "Neither as a tool nor as an influencing agent" ~ "Neither",
      ai_role == "Unsure" ~ "Unsure",
      TRUE ~ ai_role),
    PerceivedAIRole_Clean = factor(PerceivedAIRole_Clean,
                                   levels = c("Tool", "Mixed", "Agent", "Neither", "Unsure")))

cat("\nSummary Table:\n")
print(role_bias_summary, n = 30)

# chi-square test of independence (role distribution x bias magnitude)
tab <- table(df$BiasCategory, df$ai_role)
cat("\n=== CHI-SQUARE TEST: ROLE x BIAS MAGNITUDE ===\n")
print(chisq.test(tab))
cat("(cells with expected count < 5 trigger the approximation warning;\n")
cat(" simulated-p cross-check below)\n")
set.seed(123)
print(chisq.test(tab, simulate.p.value = TRUE, B = 20000))

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

role_colors <- c(
  "Tool"    = "#7A9A91",  # Muted sage/teal — darker passive tone
  "Mixed"   = "#B49A7D",  # Darker beige — midpoint with warmth
  "Agent"   = "#7B3F00",  # Deep sienna — strong, grounded agentive
  "Neither" = "#9C8C76",  # Dark taupe — off-axis but cohesive
  "Unsure"  = "#BEB3A7"   # Ashen sand — gentle, ambiguous
)

perceived_role_plot <- ggplot(role_bias_summary,
                              aes(x = BiasCategory, y = Percentage,
                                  fill = PerceivedAIRole_Clean)) +
  geom_col(position = position_dodge2(width = 0.9, preserve = "single"),
           alpha = 0.85, width = 0.88) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_dodge2(width = 0.9, preserve = "single"),
            vjust = -0.5, size = 2.85, family = "Avenir") +
  scale_fill_manual(values = role_colors, name = "Perceived AI Role") +
  scale_x_discrete(expand = expansion(add = c(0.4, 0.5))) +
  scale_y_continuous(labels = label_percent(scale = 1),
                     expand = expansion(mult = c(0, 0.15)),
                     limits = c(0, max(role_bias_summary$Percentage) * 1.1)) +
  labs(
    x = "AI Bias Magnitude",
    y = "Percentage of Participants"
  ) +
  nature_theme +
  theme(
    legend.position = "none",
    axis.title.y = element_text(margin = margin(r = 15)),
    axis.title.x = element_text(margin = margin(t = 10))
  )

print(perceived_role_plot)

ggsave(file.path(FIG_DIR, "perceived_ai_role.png"), perceived_role_plot,
       width = 9.2, height = 4.3, dpi = 500)
cat(sprintf("\nSaved perceived_ai_role.png in %s\n", SCRIPT_DIR))
