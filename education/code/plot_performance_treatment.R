# ==============================================================================
# plot_performance_treatment.R — treatment-effect forest for CODING and MEMO,
# strictly in the style of notebooks/R/plot_active_m2_treatment.R: emmeans-adjusted
# arm means with nested 90/95/99% CI ladders, the reference nature_theme, and the
# reference errorbarh/point config. Two horizontally-aligned panels (Coding | Memo).
#
# Deviations from the reference (only what the study requires):
#   - arms      : Default / Novelty / Reliability (the reference's 5-arm ladder)
#   - colors    : brown-teal schema (Novelty = teal, Reliability = brown, Default = gray)
#   - model     : student FE + week FE, student-clustered SE (the bm spec) in place of
#                 the reference's ai_group + wave + baseline / HC3
#   - outcomes  : RELIABILITY-MATCHED grader sets (Option B) — each assignment is
#                 scored by the grader combination with the highest measured
#                 inter-rater reliability:
#                   coding = 2 human graders + Gemini, equal per-grader weight,
#                            Gemini mean-aligned to the human mean (alpha ~ .78;
#                            Gemini-human r ~ .5, a valid third rater)
#                   memo   = 2 human graders only (LLM memo grades correlate only
#                            r ~ .3 with human grading -> excluded as invalid)
#                 Focal stats under this choice: coding Novelty-Default D=0.541,
#                 g=0.477, p_raw .047 / p_FDR .093; memo pooled Biased-Default
#                 D=0.157, g=0.169, p .080. Full grader ladder in the supplement.
#                 NOTE: data/scores_all_graders_student_week.csv is a hybrid per
#                 Shiyang's decision (Jul 2026): coding-side grader columns are the
#                 ORIGINAL export; memo-side Gemini columns carry the corrected
#                 re-match (32 grades fixed, 32 filled).
#
# Inputs : data/bm_student_week.csv, data/coding_regrade_student_week.csv,
#          data/memo_regrade_student_week.csv, data/scores_all_graders_student_week.csv
# Outputs: figures/plot_performance_treatment.png
#   setwd("education/code"); source("_setup.R"); source("plot_performance_treatment.R")
# ==============================================================================
suppressPackageStartupMessages({
  library(ggplot2); library(emmeans); library(dplyr); library(readr); library(sandwich); library(scales)
})

# Resolve this script's own folder so data/ and figures/ resolve whether it is run
# via Rscript, sourced in RStudio, or pasted line-by-line from any working directory.
IN <- DATA_DIR; OUT <- FIG_DIR
# Route any implicit graphics (grid text measurement) to a throwaway PNG, not pdf.

# ── Data + student-FE model (bm spec) per outcome ─────────────────────────────
grades <- read_csv(file.path(IN, "coding_regrade_student_week.csv"), show_col_types = FALSE)
memo <- read_csv(file.path(IN, "memo_regrade_student_week.csv"), show_col_types = FALSE)
gem <- read_csv(file.path(IN, "scores_all_graders_student_week.csv"), show_col_types = FALSE) %>%
  dplyr::select(student_id, week, gem_c = gemini_asgn_score)
d <- read_csv(file.path(IN, "bm_student_week.csv"), show_col_types = FALSE) %>%
  left_join(grades, by = c("student_id", "week")) %>%
  left_join(memo, by = c("student_id", "week")) %>%
  left_join(gem, by = c("student_id", "week")) %>%
  mutate(arm = factor(dplyr::recode(direction, None = "Default",
                                    Novelty = "Novelty", Reliability = "Reliability"),
                      levels = c("Default", "Novelty", "Reliability")),
         week = factor(week), student_id = as.factor(student_id))

# emmeans-adjusted arm means + 90/95/99% normal CIs (reference lines 66-74)
ladder <- function(ycol, panel) {
  dat <- d[!is.na(d[[ycol]]) & d[[ycol]] > 0, ]                 # 0 = no submission
  m  <- lm(as.formula(paste(ycol, "~ arm + week + factor(student_id)")), data = dat)
  vc <- vcovCL(m, cluster = dat$student_id[as.integer(rownames(model.frame(m)))])
  es <- as.data.frame(summary(emmeans(m, ~ arm, vcov. = vc)))
  cat(sprintf("\n=== %s — adjusted arm means (student FE, student-clustered) ===\n", panel))
  print(es[, c("arm", "emmean", "SE")], row.names = FALSE, digits = 4)
  es %>% mutate(
    arm = as.character(arm), y_position = YORDER[arm], outcome = panel,
    CI_90_lower = emmean - SE * qnorm(0.95),  CI_90_upper = emmean + SE * qnorm(0.95),
    CI_95_lower = emmean - SE * qnorm(0.975), CI_95_upper = emmean + SE * qnorm(0.975),
    CI_99_lower = emmean - SE * qnorm(0.995), CI_99_upper = emmean + SE * qnorm(0.995))
}

# top-to-bottom arm order + brown/teal/gray (reference YORDER/SHORT/bias_colors)
YORDER <- c("Novelty" = 3, "Default" = 2, "Reliability" = 1)
arm_colors <- c("Novelty" = "#01665E", "Default" = "#79706E", "Reliability" = "#8C510A")  # teal / gray / brown
lab_top2bottom <- names(sort(YORDER, decreasing = TRUE))                                   # Novelty, Default, Reliability

# coding composite: 2 humans + Gemini, equal per-grader weight; Gemini mean-aligned
# on the overlapping rows; rows without a Gemini grade keep the 2-human mean
shift <- mean(d$coding_regrade[!is.na(d$gem_c)], na.rm = TRUE) -
         mean(d$gem_c[!is.na(d$coding_regrade)], na.rm = TRUE)
d$coding_hg <- ifelse(is.na(d$gem_c), d$coding_regrade,
                      (2 * d$coding_regrade + d$gem_c + shift) / 3)
d$coding_hg[is.na(d$coding_regrade)] <- NA
cat(sprintf("coding composite: Gemini mean-alignment shift %+.3f\n", shift))

pd <- bind_rows(ladder("coding_hg", "Coding Performance"),
                ladder("memo_regrade", "Memo Performance")) %>%
  mutate(outcome = factor(outcome, levels = c("Coding Performance", "Memo Performance")))

# ── Nature-ish theme (verbatim from the reference; FONT = Avenir on macOS) ─────
nature_theme <- theme_classic() +
  theme(
    text = element_text(family = FONT, size = 8),
    plot.title = element_text(family = FONT, size = 10, face = "bold", hjust = 0),
    axis.title = element_text(family = FONT, size = 9, face = "plain"),
    axis.text = element_text(family = FONT, size = 8, color = "black"),
    axis.text.y = element_text(family = FONT, size = 9, color = "black", face = "plain"),
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.ticks.length = unit(0.15, "cm"),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 10)
  )

# ── Forest ladder (reference geom_errorbarh/point config verbatim) ────────────
p <- ggplot(pd, aes(y = y_position)) +
  geom_errorbarh(aes(xmin = CI_99_lower, xmax = CI_99_upper, color = arm),
                 height = 0.25, linewidth = 1.2, alpha = 0.2) +
  geom_errorbarh(aes(xmin = CI_95_lower, xmax = CI_95_upper, color = arm),
                 height = 0.20, linewidth = 0.9, alpha = 0.4) +
  geom_errorbarh(aes(xmin = CI_90_lower, xmax = CI_90_upper, color = arm),
                 height = 0.15, linewidth = 0.7, alpha = 0.7) +
  geom_point(aes(x = emmean, color = arm), size = 3, alpha = 0.8) +
  facet_wrap(~ outcome, nrow = 1, scales = "free_x", strip.position = "bottom") +
  scale_color_manual(values = arm_colors, breaks = lab_top2bottom) +
  guides(color = guide_legend(nrow = 1)) +
  scale_y_continuous(breaks = 3:1, labels = lab_top2bottom,
                     expand = expansion(add = c(0.4, 0.4))) +
  scale_x_continuous(labels = label_number(accuracy = 0.1)) +
  labs(x = NULL, y = NULL) +
  xlim(8, 11) + 
  nature_theme +
  theme(
    legend.position = "bottom", legend.justification = "center",
    legend.title = element_blank(), legend.text = element_text(size = 9),
    strip.background = element_blank(), strip.placement = "outside",  # label under each panel's axis
    # push the bottom panel label down toward the figure bottom (raise `t` for lower)
    strip.text = element_text(family = FONT, size = 9.5, face = "bold", margin = margin(t = 0, b = 3)),
    panel.spacing = unit(1.2, "lines"),
    axis.text.y = element_text(angle = 90, hjust = 0.5)
  )

p
save_png(file.path(OUT, "plot_performance_treatment.png"), p, width = 4 * 1.0, height = 5 * 1.0, dpi = 500)
cat(sprintf("\nSaved plot_performance_treatment.png in %s\n", OUT))
cat("Done: plot_performance_treatment.R\n")
