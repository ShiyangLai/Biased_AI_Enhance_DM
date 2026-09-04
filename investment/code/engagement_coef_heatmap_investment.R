# ==============================================================================
# engagement_coef_heatmap_investment.R
# Investment analog of the fact-checking study's engagement_coef_heatmap.R:
# engagement coefficient heatmap (treatment arm x engagement dimension),
# all arms vs the Single Default AI baseline.
#
# Row mapping (political -> investment):
#   Single AI Biased      -> the four single biased arms pooled (Som/Ext
#                            Risk-Averse + Som/Ext Risk-Seeking)
#   Single AI Neutralized -> Risk-Neutral arm
#   Dual AI Opposition / Default / Balanced -> dual_opposition / dual_nonbiased /
#                            dual_balanced (participant-relative, §15 export)
#   (no investment analog of "Dual AI Neutralized" -> row absent)
#
# Model mapping (reference: Engagement ~ ExperimentType + as.factor(NID)
#   + PrePerformance + (1|UID) + as.factor(UStanceLabel), lmerTest):
#   NID FE + (1|UID)  -> wave FE + HC3 (one observation per participant)
#   PrePerformance    -> pre_active_m2_ann
#   UStanceLabel      -> participant risk lean (Averse/Seeking factor)
# Per dimension: lm(dim ~ ExperimentType + wave + pre_active_m2_ann + lean),
# HC3 coefficients vs baseline. Tiles: coefficient + stars; fill = coefficient
# where p < .1, gray otherwise. *** p<.001, ** p<.01, * p<.05, dagger p<.1.
#
# Engagement scores: GPT 5-dim rubric (0-3), single = engagement_annotations.csv,
# dual = engagement_annotations_dual.csv (identical rubric; see
# engagement_annotation_dual.py). Zero-follow-up floor-scoring keeps
# behavioral/cognitive/emotional/social_presence = 0 rows; autonomy is NA there
# (silence is uninformative about deference) so the Autonomy model runs on
# engagers only -- per-dimension n printed.
#
# Inputs: engagement_annotations{,_dual}.csv,
#         active_m2_treatment_data.csv, dual_active_m2.csv,
#         participant_covariates.csv, dual_covariates.csv ()
# Output: engagement_coef_heatmap_investment.png
#   setwd("investment/code"); source("_setup.R"); source("engagement_coef_heatmap_investment.R")
# ==============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(ggplot2); library(sandwich); library(lmtest)
})

.args <- commandArgs(FALSE); .f <- sub("^--file=", "", .args[grep("^--file=", .args)])
SCRIPT_DIR <- if (length(.f)) dirname(normalizePath(.f)) else
  getwd()
NB_DIR <- DATA_DIR
rd  <- function(f) read.csv(file.path(DATA_DIR, f), check.names = FALSE, stringsAsFactors = FALSE)
rdn <- function(f) read.csv(file.path(DATA_DIR, f),    check.names = FALSE, stringsAsFactors = FALSE)

ENG       <- c("behavioral", "cognitive", "emotional", "autonomy", "social_presence")
ENG_NAMES <- c("Behavioral", "Cognitive", "Emotional", "Autonomy", "Social Presence")

# ── single-AI arms ────────────────────────────────────────────────────────────
e1 <- rdn("engagement_annotations.csv") %>%
  filter(status %in% c("ok", "no_followup")) %>%
  inner_join(rd("active_m2_treatment_data.csv") %>%
               select(participantId, pre_active_m2_ann),
             by = "participantId") %>%
  inner_join(rd("participant_covariates.csv") %>%
               select(participantId, risk_pref_score), by = "participantId") %>%
  mutate(experiment = "single")

# ── dual-AI arms ──────────────────────────────────────────────────────────────
e2 <- rdn("engagement_annotations_dual.csv") %>%
  filter(status %in% c("ok", "no_followup")) %>%
  inner_join(rd("dual_active_m2.csv") %>%
               select(participantId, pre_active_m2_ann),
             by = "participantId") %>%
  inner_join(rd("dual_covariates.csv") %>%
               select(participantId, risk_pref_score), by = "participantId") %>%
  mutate(experiment = "dual")

d <- bind_rows(e1, e2) %>% filter(!is.na(pre_active_m2_ann), !is.na(risk_pref_score))

# participant risk lean (UStanceLabel analog): median split over the combined
# analysis sample (bias_side_performance.R convention)
thr <- median(d$risk_pref_score, na.rm = TRUE)
d$lean <- factor(ifelse(d$risk_pref_score >= thr, "Seeking", "Averse"),
                 levels = c("Averse", "Seeking"))
cat(sprintf("risk_pref median split at %.3f (Averse %d / Seeking %d)\n",
            thr, sum(d$lean == "Averse"), sum(d$lean == "Seeking")))

d <- d %>% mutate(
  ai_dir = case_when(grepl("Averse",  ai_group) ~ "Averse",
                     grepl("Seeking", ai_group) ~ "Seeking",
                     TRUE ~ NA_character_),
  ExperimentType = case_when(
    experiment == "single" & ai_group == "Default"      ~ "Single_AI_Non_Biased",
    experiment == "single" & ai_group == "Risk-Neutral" ~ "Single_AI_Non_Biased_Exp",
    experiment == "single" & !is.na(ai_dir)             ~ "Single_AI_Biased",
    experiment == "dual" & dual_condition == "dual_nonbiased"  ~ "Dual_AI_Non_Biased",
    experiment == "dual" & dual_condition == "dual_balanced"   ~ "Dual_AI_Balanced",
    experiment == "dual" & dual_condition == "dual_opposition" ~ "Dual_AI_Opposition",
    TRUE ~ "drop")) %>%
  filter(ExperimentType != "drop")
d$ExperimentType <- relevel(as.factor(d$ExperimentType), ref = "Single_AI_Non_Biased")
d$wave <- factor(d$wave)
cat("\nN by arm:\n"); print(table(d$ExperimentType))

# ── per-dimension OLS (wave FE, HC3), coefficients vs baseline ────────────────
res <- data.frame()
for (i in seq_along(ENG)) {
  mm <- lm(as.formula(paste(ENG[i],
             "~ ExperimentType + wave + pre_active_m2_ann + lean")), data = d)
  cc <- coeftest(mm, vcov = vcovHC(mm, type = "HC3"))
  r  <- grep("^ExperimentType", rownames(cc))
  res <- rbind(res, data.frame(
    Dimension = ENG_NAMES[i],
    Arm = gsub("ExperimentType", "", rownames(cc)[r]),
    Coefficient = cc[r, 1], SE = cc[r, 2], p = cc[r, 4],
    n = nobs(mm)))
}
# within-dimension FDR across the 5 arm contrasts; the main figure uses the
# adjusted p, a companion "_rawp" figure uses the unadjusted p
res$p_fdr <- ave(res$p, res$Dimension, FUN = function(p) p.adjust(p, "fdr"))
stars_of <- function(p) cut(p, c(-Inf, .001, .01, .05, .1, Inf),
                            c("***", "**", "*", "†", ""), right = FALSE)
res$stars <- stars_of(res$p_fdr)

arm_label <- c(Single_AI_Biased         = "Single AI Biased",
               Single_AI_Non_Biased_Exp = "Single AI Neutralized",
               Dual_AI_Opposition       = "Dual AI Opposition",
               Dual_AI_Non_Biased       = "Dual AI Default",
               Dual_AI_Balanced         = "Dual AI Balanced")
res$ArmLabel <- factor(arm_label[res$Arm],
                       levels = rev(unname(arm_label)))   # top-to-bottom as reference
res$Dimension <- factor(res$Dimension,
                        levels = c("Autonomy", "Behavioral", "Cognitive",
                                   "Emotional", "Social Presence"))
cat("\n=== coefficients vs Single AI Default (HC3; within-dimension FDR) ===\n")
print(res[order(res$Dimension, res$ArmLabel),
          c("Dimension", "ArmLabel", "Coefficient", "SE", "p", "p_fdr", "stars", "n")],
      row.names = FALSE, digits = 3)

# ── heatmap (verbatim reference styling) ──────────────────────────────────────
lim <- max(abs(res$Coefficient)) * 1.02

mk_heat <- function(pvec) {
  res$label    <- paste0(sprintf("%.3f", res$Coefficient), stars_of(pvec))
  res$fill_val <- ifelse(pvec < 0.1, res$Coefficient, NA)  # gray out non-significant
  res$sig_face <- ifelse(pvec < 0.1, "plain", "plain")
  ggplot(res, aes(x = Dimension, y = ArmLabel, fill = fill_val)) +
  geom_tile(color = "white", linewidth = 2.6) +
  geom_text(aes(label = label, fontface = sig_face),
            family = "Avenir", size = 3.5, color = "black") +
  scale_fill_gradient2(low = "#D01C8B", mid = "#FAF6F9", high = "#6FA51E",
                       midpoint = 0, limits = c(-lim, lim),
                       na.value = "grey96", guide = "none") +
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
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(t = 12, r = 15, b = 10, l = 10)
  )
}

p <- mk_heat(res$p_fdr)           # main figure: within-dimension FDR
p_raw <- mk_heat(res$p)           # companion: unadjusted p
print(p)
ggsave(file.path(FIG_DIR, "engagement_coef_heatmap_investment.png"), p,
       width = 5.9, height = 5.5, dpi = 500)
# ggsave(file.path(FIG_DIR, "engagement_coef_heatmap_investment_rawp.png"), p_raw,
#        width = 7.4, height = 5.6, dpi = 500)
# cat(sprintf("\nSaved engagement_coef_heatmap_investment.png (FDR) and\n"))
# cat(sprintf("      engagement_coef_heatmap_investment_rawp.png (raw p) in %s\n", SCRIPT_DIR))
