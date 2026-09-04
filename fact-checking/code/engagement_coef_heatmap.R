# ==============================================================================
# engagement_coef_heatmap.R — RECONSTRUCTION of the engagement coefficient
# heatmap (treatment arm x engagement dimension), yellow-orange color scheme.
#
# Rows: 7-arm design vs. Single AI Non-Biased baseline (matches the original
# figure, incl. the Neutralized arms): Single Opposition / Single Neutralized /
# Dual Opposition / Dual Neutralized / Dual Non-Biased / Dual Balanced.
# Model per dimension (as in forth_figure_d4.R):
#   Engagement_i ~ ExperimentType + as.factor(NID) + PrePerformance
#                  + (1|UID) + as.factor(UStanceLabel)      [lmerTest]
# Tiles: coefficient + stars; fill = coefficient where p < .1, gray otherwise.
# Stars: *** p<.001, ** p<.01, * p<.05, † p<.1 (as in the original figure).
#
# Self-contained; saves Images/engagement_coef_heatmap.png.
# ==============================================================================
suppressMessages({library(dplyr); library(stringr); library(ggplot2); library(lmerTest); library(broom.mixed)})

base <- ".."
adj <- function(code, conf) ifelse(code == -1, (1 - conf) * 0.5,
                             ifelse(code == 1, 0.5 + conf * 0.5, 0.5))
code_of <- function(s) case_when(s == "Republican" ~ 1, s %in% c("Neutral", "Default") ~ 0,
                                 s == "Democrat" ~ -1, TRUE ~ NA_real_)
ENG <- c("EngagementBehavioral", "EngagementCognitive", "EngagementEmotional",
         "EngagementAutonomy", "EngagementSocialPresence")
ENG_NAMES <- c("Behavioral", "Cognitive", "Emotional", "Autonomy", "Social Presence")
KEEP <- c("UID", "NID", "UStanceLabel", "PrePerformance", "ExperimentType", ENG)

# ---- single-AI arms (old 7-arm construction: Echo Chamber rows dropped) -----
d1 <- read.csv(file.path(base, "data", "encrypted_ai1.csv"), stringsAsFactors = FALSE) %>%
  mutate(
    NormalizedTruthCode = (TruthCode + 1) / 2,
    PrePerformance = 1 - abs(NormalizedTruthCode - adj(PreEvaCode, PreConfCode)),
    ExperimentType = case_when(
      AIStanceLabel_S == "Default" ~ "Single_AI_Non_Biased",
      AIStanceLabel_S == "Neutral" ~ "Single_AI_Non_Biased_Exp",
      # Biased = ALL partisan single-AI rows (any Rep/Dem AI, regardless of the
      # user's stance) — the d1/d4 definition, replacing the old Opposition-only row
      AIStanceLabel_S %in% c("Republican", "Democrat") ~ "Single_AI_Biased",
      TRUE ~ "drop")) %>%
  filter(ExperimentType != "drop") %>% dplyr::select(all_of(KEEP))

# ---- dual-AI arms -------------------------------------------------------------
d2 <- read.csv(file.path(base, "data", "encrypted_ai2.csv"), stringsAsFactors = FALSE) %>%
  mutate(
    NormalizedTruthCode = (TruthCode + 1) / 2,
    PrePerformance = 1 - abs(NormalizedTruthCode - adj(PreEvaCode, PreConfCode)),
    AI1S = str_extract(AIStanceLabel_S, "(?<=\\(').*?(?=', ')"),
    AI2S = str_extract(AIStanceLabel_S, "(?<=', ').*?(?=')"),
    AI1C = code_of(AI1S), AI2C = code_of(AI2S),
    UC = case_when(UStanceLabel_S == "Republican" ~ 1, UStanceLabel_S == "Independent" ~ 0,
                   UStanceLabel_S == "Democrat" ~ -1, TRUE ~ NA_real_),
    ExperimentType = case_when(
      AI1S == "Default" & AI2S == "Default" ~ "Dual_AI_Non_Biased",
      AI1S == "Neutral" & AI2S == "Neutral" ~ "Dual_AI_Non_Biased_Exp",
      AI1C != AI2C & UC > pmin(AI1C, AI2C) & UC < pmax(AI1C, AI2C) ~ "Dual_AI_Balanced",
      UC != 0 & AI1C != 0 & AI2C != 0 &
        sign(AI1C) == -sign(UC) & sign(AI2C) == -sign(UC) ~ "Dual_AI_Opposition",
      TRUE ~ "drop")) %>%
  filter(ExperimentType != "drop") %>% dplyr::select(all_of(KEEP))

combined_data <- bind_rows(d1, d2)
combined_data$ExperimentType <- relevel(as.factor(combined_data$ExperimentType),
                                        ref = "Single_AI_Non_Biased")
cat("N by arm:\n"); print(table(combined_data$ExperimentType))

# ---- per-dimension mixed models, coefficients vs baseline ---------------------
res <- data.frame()
for (i in seq_along(ENG)) {
  m <- lmerTest::lmer(
    formula = paste(ENG[i], "~ ExperimentType + as.factor(NID) + PrePerformance + (1|UID) + as.factor(UStanceLabel)"),
    data = combined_data)
  tt <- broom.mixed::tidy(m, effects = "fixed", conf.int = TRUE)
  cc <- tt[grepl("^ExperimentType", tt$term), ]
  res <- rbind(res, data.frame(
    Dimension = ENG_NAMES[i],
    Arm = gsub("ExperimentType", "", cc$term),
    Coefficient = cc$estimate, SE = cc$std.error,
    df = cc$df, p = cc$p.value))
}
# Benjamini-Hochberg FDR within each dimension (6 arm contrasts per outcome
# family); stars and color gating below use the ADJUSTED p-values.
res$p_fdr <- ave(res$p, res$Dimension, FUN = function(x) p.adjust(x, method = "fdr"))
res$stars <- cut(res$p_fdr, c(-Inf, .001, .01, .05, .1, Inf),
                 c("***", "**", "*", "\u2020", ""), right = FALSE)  # † = dagger (locale-safe)

arm_label <- c(Single_AI_Biased        = "Single AI Biased",
               Single_AI_Non_Biased_Exp = "Single AI Neutralized",
               Dual_AI_Opposition       = "Dual AI Opposition",
               Dual_AI_Non_Biased_Exp   = "Dual AI Neutralized",
               Dual_AI_Non_Biased       = "Dual AI Default",
               Dual_AI_Balanced         = "Dual AI Balanced")
res$ArmLabel <- factor(arm_label[res$Arm],
                       levels = rev(unname(arm_label)))   # top-to-bottom as original
res$Dimension <- factor(res$Dimension,
                        levels = c("Autonomy", "Behavioral", "Cognitive", "Emotional", "Social Presence"))
res$label <- paste0(sprintf("%.3f", res$Coefficient), res$stars)
res$fill_val <- ifelse(res$p_fdr < 0.1, res$Coefficient, NA)   # gray out non-significant (FDR)

cat("\n=== coefficients vs Single AI Non-Biased (stars/fill: within-dimension FDR) ===\n")
print(res[order(res$Dimension, res$ArmLabel),
          c("Dimension", "ArmLabel", "Coefficient", "SE", "df", "p", "p_fdr", "stars")],
      row.names = FALSE, digits = 3)

# ---- heatmap (PiYG diverging: magenta = negative, yellow-green = positive) ----
lim <- max(abs(res$Coefficient)) * 1.02
res$sig_face <- ifelse(res$p < 0.1, "plain", "plain")   # weight significant cells

p <- ggplot(res, aes(x = Dimension, y = ArmLabel, fill = fill_val)) +
  geom_tile(color = "white", linewidth = 2.6) +   # wide white gutters between cells
  geom_text(aes(label = label, fontface = sig_face),
            family = "Avenir", size = 3.5, color = "black") +
  scale_fill_gradient2(low = "#D01C8B",      # magenta-pink (negative)
                       mid = "#FAF6F9",      # near-white (near zero)
                       high = "#6FA51E",     # yellow-green (positive; chartreuse, far from PRGn emerald)
                       midpoint = 0, limits = c(-lim, lim),
                       na.value = "grey96",  # non-significant (light gray)
                       guide = "none") +
  scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) +
  labs(x = "Engagement Dimension",
       y = "Treatment Type (vs. Single Default AI Baseline)") +
  theme_classic() +
  theme(
    text = element_text(family = "Avenir", color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.line = element_blank(),
    axis.text.x = element_text(family = "Avenir", size = 10, color = "black",
                               margin = margin(t = 6), angle = 45, hjust = 1),
    axis.text.y = element_text(family = "Avenir", size = 10, color = "black",
                               margin = margin(r = 4)),
    axis.title.x = element_text(family = "Avenir", size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(family = "Avenir", size = 12, margin = margin(r = 10)),
    axis.ticks = element_blank(),                 # gutters make ticks redundant
    panel.grid = element_blank(),
    plot.margin = margin(t = 12, r = 15, b = 10, l = 10)
  )

# print(p)
out <- file.path(base, "figures", "engagement_coef_heatmap.png")
ragg::agg_png(out, width = 5.9, height = 5.5, units = "in", res = 500)
print(p)
dev.off()
cat("\nSaved:", out, "\n")
