# ==============================================================================
# engagement_coef_heatmap_education.R — Extended Data heatmap: LLM-annotated five-dimension
# engagement (reference rubric, 0-3) by treatment arm vs the Default baseline.
# In-class analog of notebooks/R/engagement_coef_heatmap_investment.R; the tile
# styling follows that script VERBATIM (white tile borders 2.6, magenta->green
# diverging fill gated at p<.1 with grey96 otherwise, coefficient + stars incl.
# the p<.1 dagger, angled x labels, black panel frame), with square cells at the
# reference's cell size (coord_fixed; width matched to the reference figure).
#
# Rows (vs Default): Novelty-Biased / Reliability-Biased / Neutralized.
#   (The QUESTIONING_ORIENTED arm exists, 7 annotated weeks, but is out of scope
#   for this figure; arm assignment joined from scores_all_graders_student_week.csv
#   because the annotation file carries no policy column.)
#
# Model mapping (reference: lm(dim ~ ExperimentType + wave FE + pre + lean), HC3,
# one obs per participant): student-week panel here, so per dimension
#     lm(dim ~ arm + week FE + student FE), student-clustered SE
# — the project's within-student spec (same reasoning as e1_engagement_heatmap.R:
# OLS without the student term loads between-student usage selection onto the arm
# coefficients). A random-intercept companion (a3's LLM convention) prints to the
# console. Within-dimension FDR across the 3 arm contrasts; fill/stars use adj. p.
#
# Deviation from the reference: annotations exist ONLY for student-weeks with a
# conversation (137 of 351), so all five dimensions are use-conditioned; there is
# no floor-scored stratum and autonomy has no NA rows. Cell ns are TINY on the
# biased side (Novelty 7, Reliability 11, Neutral 5 annotated weeks) — printed
# per arm and worth stating in the caption.
#
# Inputs : figures/engagement_annotations.csv (the upstream engagement-annotation step),
#          data/scores_all_graders_student_week.csv
# Outputs: figures/engagement_coef_heatmap_education.csv, figures/engagement_coef_heatmap_education.png
#   setwd("education/code"); source("_setup.R"); source("engagement_coef_heatmap_education.R")
# ==============================================================================
set.seed(123)
suppressPackageStartupMessages({
  library(dplyr); library(ggplot2); library(readr); library(sandwich)
  library(lmtest); library(lmerTest)
})

IN <- DATA_DIR; OUT <- FIG_DIR

ENG       <- c("autonomy", "behavioral", "cognitive", "emotional", "social_presence")
ENG_NAMES <- c("Autonomy", "Behavioral", "Cognitive", "Emotional", "Social Presence")

# ── data: annotated conversation-weeks + authoritative arm assignment ─────────
d <- read_csv(file.path(IN, "engagement_annotations.csv"), show_col_types = FALSE) %>%
  filter(status == "annotated") %>%
  mutate(across(all_of(ENG), as.numeric)) %>%
  left_join(read_csv(file.path(IN, "scores_all_graders_student_week.csv"),
                     show_col_types = FALSE) %>% dplyr::select(student_id, week, policy),
            by = c("student_id", "week")) %>%
  filter(policy != "QUESTIONING_ORIENTED") %>%          # out of scope for this figure
  mutate(arm = factor(dplyr::recode(policy, DEFAULT = "Default", NEUTRAL = "Neutralized",
                                    NOVELTY_BIASED = "Novelty-Biased",
                                    RELIABILITY_BIASED = "Reliability-Biased"),
                      levels = c("Default", "Novelty-Biased", "Reliability-Biased", "Neutralized")),
         week = factor(week), student_id = factor(student_id))
cat("annotated student-weeks by arm:\n"); print(table(d$arm))
cat(sprintf("(use-conditioned by construction: only conversation weeks were annotated)\n"))

# ── per-dimension student-FE OLS, coefficients vs Default ─────────────────────
res <- data.frame()
for (i in seq_along(ENG)) {
  mm <- lm(as.formula(paste(ENG[i], "~ arm + week + factor(student_id)")), data = d)
  cc <- coeftest(mm, vcov = vcovCL(mm, cluster = d$student_id[as.integer(rownames(model.frame(mm)))]))
  r  <- grep("^arm", rownames(cc))
  res <- rbind(res, data.frame(
    Dimension = ENG_NAMES[i],
    Arm = gsub("^arm", "", rownames(cc)[r]),
    Coefficient = cc[r, 1], SE = cc[r, 2], p = cc[r, 4],
    n = nobs(mm)))
  # random-intercept companion (a3's LLM convention), console only
  mr <- suppressMessages(lmer(as.formula(paste(ENG[i], "~ arm + week + (1|student_id)")), data = d))
  cr <- coef(summary(mr)); rr <- grep("^arm", rownames(cr))
  cat(sprintf("[RE companion] %-16s %s\n", ENG_NAMES[i],
              paste(sprintf("%s %+0.3f (p=%.3f)", gsub("^arm", "", rownames(cr)[rr]),
                            cr[rr, 1], cr[rr, 5]), collapse = " | ")))
}
# within-dimension FDR across the 3 arm contrasts (reference convention)
res$p_fdr <- ave(res$p, res$Dimension, FUN = function(p) p.adjust(p, "fdr"))
stars_of <- function(p) cut(p, c(-Inf, .001, .01, .05, .1, Inf),
                            c("***", "**", "*", "†", ""), right = FALSE)
res$stars <- stars_of(res$p_fdr)

res$ArmLabel <- factor(res$Arm,
                       levels = rev(c("Novelty-Biased", "Reliability-Biased", "Neutralized")))
res$Dimension <- factor(res$Dimension, levels = ENG_NAMES)
cat("\n=== coefficients vs Default (student FE + clustered SE; within-dimension FDR) ===\n")
print(res[order(res$Dimension, res$ArmLabel),
          c("Dimension", "Arm", "Coefficient", "SE", "p", "p_fdr", "stars", "n")],
      row.names = FALSE, digits = 3)
write_csv(res, file.path(OUT, "engagement_coef_heatmap_education.csv"))

# ── heatmap (verbatim reference styling; square cells via coord_fixed) ────────
lim <- max(abs(res$Coefficient)) * 1.02
res$label    <- paste0(sprintf("%.3f", res$Coefficient), res$stars)
res$fill_val <- ifelse(res$p_fdr < 0.1, res$Coefficient, NA)   # gray out non-significant

p <- ggplot(res, aes(x = Dimension, y = ArmLabel, fill = fill_val)) +
  geom_tile(color = "white", linewidth = 2.6) +
  geom_text(aes(label = label), family = FONT, size = 3.5, color = "black") +
  scale_fill_gradient2(low = "#D01C8B", mid = "#FAF6F9", high = "#6FA51E",
                       midpoint = 0, limits = c(-lim, lim),
                       na.value = "grey96", guide = "none") +
  scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) +
  coord_fixed() +                                   # square cells (reference-size, see save)
  labs(x = "Engagement Dimension",
       y = "Treatment Type\n(vs. Default AI Baseline)") +   # wrapped: one line clips at h=3.9
  theme_classic() +
  theme(
    text = element_text(family = FONT, color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    axis.line = element_blank(),
    axis.text.x = element_text(family = FONT, size = 10, color = "black",
                               margin = margin(t = 6), angle = 45, hjust = 1),
    axis.text.y = element_text(family = FONT, size = 10, color = "black",
                               margin = margin(r = 4)),
    axis.title.x = element_text(family = FONT, size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(family = FONT, size = 12, margin = margin(r = 10)),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(t = 12, r = 15, b = 10, l = 10)
  )

# reference is 5.9 x 5.5 in for a 5x5 grid (~0.8 in cells); same width and a height
# trimmed by two rows keeps the per-cell size, with coord_fixed enforcing squares
save_png(file.path(OUT, "engagement_coef_heatmap_education.png"), p, width = 5.9, height = 3.9, dpi = 500)
cat(sprintf("\nSaved engagement_coef_heatmap_education.csv, engagement_coef_heatmap_education.png in %s\n", OUT))
cat("Done: engagement_coef_heatmap_education.R\n")
