# ==============================================================================
# bias_side_performance.R — educational-AI replication of
# paper/reference_code/third_figure_c1.R (echo-chamber vs opposition bias).
#
# Mapping (reference -> this study):
#   UStanceLabel_S party (drop Independent) -> dominant preference lean:
#       sign(novelty_mean - reliability_mean) from the pre-survey; students with
#       direction_score == 0 have no lean and drop out (the Independent analog).
#   AIStanceLabel_S (AI side)   -> assigned biased arm (novelty / reliability)
#   BiasSide Same/Opposite      -> echo (arm matches lean) vs opposition
#   PostPerformance             -> combined_all_asgn_score (+ memo companion)
#   UID / NID / PrePerformance  -> student_id / week / prior_prog_exp + prior_ml_exp
#   Default AI reference line   -> mean over the default-arm student-weeks
#   UIdeo / AICorrectness       -> no analog; omitted.
#
# Building on the study's TWO echo operationalizations:
#   PRIMARY  (dominant-construct): BiasSide x lean interaction — the exact C1 analog.
#   SECONDARY (per-dimension):     echo flag within each biased arm from the
#       standardized matching preference (novelty arm x novelty_pref_z>0;
#       reliability arm x reliability_pref_z>0), pooled with an echo x dimension model.
#   ROBUSTNESS: reference-faithful "Independent" band — drop |direction_score| < 0.5.
#
# Inputs : data/c1_echo_student_week.csv
# Outputs: figures/c1_emmeans_cells.csv, figures/c1_contrasts_hedges.csv,
#          figures/c1_perdim_cells.csv,
#          figures/bias_side_performance.png, figures/bias_side_performance_4cell.png
#   setwd("education/code"); source("_setup.R"); source("bias_side_performance.R")
# ==============================================================================
set.seed(123)
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(ggplot2)
  library(lmerTest); library(emmeans); library(sandwich); library(lmtest); library(performance)
})

# Resolve this script's own folder so data/ and figures/ resolve whether it is run
# via Rscript, sourced in RStudio, or pasted line-by-line from any working directory.
IN <- DATA_DIR; OUT <- FIG_DIR

# Route any implicit graphics (e.g. grid text measurement) to a throwaway PNG rather than
# the default pdf device -- some R builds can't render custom fonts to pdf ("invalid font
# type"), which would abort the run. All real figures are saved as PNG (dpi 500) below.

cw <- read_csv(file.path(IN, "c1_echo_student_week.csv"), show_col_types = FALSE) %>%
  left_join(read_csv(file.path(IN, "coding_regrade_student_week.csv"), show_col_types = FALSE),
            by = c("student_id", "week")) %>%
  left_join(read_csv(file.path(IN, "memo_regrade_student_week.csv"), show_col_types = FALSE),
            by = c("student_id", "week")) %>%
  left_join(read_csv(file.path(IN, "scores_all_graders_student_week.csv"), show_col_types = FALSE) %>%
              dplyr::select(student_id, week, gem_asgn = gemini_asgn_score),
            by = c("student_id", "week"))
# Reliability-matched grader sets (same as the performance analyses): coding = the two
# human graders + Gemini (equal per-grader weight, Gemini mean-aligned to the human
# scale); memo = the two human graders only (LLM memo grades track humans at r ~ .3).
GEM_SHIFT <- mean(cw$coding_regrade[!is.na(cw$gem_asgn)], na.rm = TRUE) -
             mean(cw$gem_asgn[!is.na(cw$coding_regrade)], na.rm = TRUE)
cw$coding_hg <- ifelse(is.na(cw$gem_asgn), cw$coding_regrade,
                       (2 * cw$coding_regrade + cw$gem_asgn + GEM_SHIFT) / 3)
cw$coding_hg[is.na(cw$coding_regrade)] <- NA
cw$student_id <- as.factor(cw$student_id)
cat(sprintf("coding composite: Gemini mean-alignment shift %+.3f\n", GEM_SHIFT))

OUTCOMES <- list(coding = list(col = "coding_hg",     label = "Coding performance (2 humans + Gemini)"),
                 memo   = list(col = "memo_regrade",  label = "Memo performance (2 human graders)"))

# Biased-arm rows with a defined lean (direction_score != 0 -> lean is non-missing).
repdem <- cw %>%
  filter(is_default_reference == 0, !is.na(lean), bias_side_dominant %in% c("Same", "Opposite")) %>%
  mutate(BiasSide = factor(bias_side_dominant, levels = c("Same", "Opposite")),
         Lean = factor(ifelse(lean == "novelty", "Novel-leaning", "Reliable-leaning"),
                       levels = c("Novel-leaning", "Reliable-leaning")))
cat("BiasSide x Lean cells (biased-arm student-weeks):\n")
print(table(repdem$BiasSide, repdem$Lean))

emm_cells_out <- list(); contrasts_out <- list()

analyze_outcome <- function(d, col, label, key, variant, covars = "") {
  d <- d[!is.na(d[[col]]) & d[[col]] > 0, ]        # 0 = no submission
  cat(sprintf("\n############ %s [%s] (n=%d student-weeks, %d students) ############\n",
              toupper(label), variant, nrow(d), length(unique(d$student_id))))

  # PRIMARY spec = BiasSide x Lean + week FE, no covariates (`covars = ""`); the
  # prior-experience covariates enter only as a reported robustness variant. On this
  # small subsample they move the echo contrast substantially without adding
  # identification, so the leaner spec is the honest primary.
  f_ols <- as.formula(paste(col, "~ BiasSide * Lean", covars, "+ as.factor(week)"))
  m_ols <- lm(f_ols, data = d)
  vc <- vcovCL(m_ols, cluster = d$student_id[as.integer(rownames(model.frame(m_ols)))])
  cat("\n--- OLS + clustered SE ---\n"); print(coeftest(m_ols, vcov = vc))

  # NO student random effect: near-cross-sectional here (mostly one week/student), so
  # the RE variance is ~0 and OLS + student-clustered SE is the cleaner spec (matches
  # the reference c1 and avoids degenerate small-cell df). Figure + contrasts use m_ols.

  # Four combination cells (echo/opposition x lean), simple effects, interaction (lines 38-53)
  emm_full <- emmeans(m_ols, ~ BiasSide * Lean, vcov. = vc)
  cat("\nEstimated marginal means (echo/opposition x lean):\n"); print(emm_full)
  # Echo-vs-Opposition contrasts computed UNADJUSTED, then FDR applied BY FAMILY
  # (consistent with bm): the two simple effects (one per lean) form one focal family
  # of 2; the interaction and the pooled BiasSide main are each a single-test family.
  # (Deviation from the reference's by-lean adjust, which corrected within each lean
  # separately and so applied no correction at all -- 1 contrast per lean.)
  simple_by_lean <- contrast(emm_full, "pairwise", by = "Lean", adjust = "none")
  cat("\nEcho vs Opposition within each lean:\n"); print(simple_by_lean)
  inter <- contrast(emm_full, interaction = c("pairwise", "pairwise"), adjust = "none")
  cat("\nInteraction contrast:\n"); print(inter)

  # Hedges' g standardised on the RAW outcome SD (not sigma(m_ols)), matching the
  # convention used by the performance analyses (bm / plot_coding_memo_treatment) so
  # that g is comparable across the paper's in-class figures.
  pooled_sd <- sd(d[[col]], na.rm = TRUE); total_n <- nrow(d)
  corr <- 1 - (3 / (4 * total_n - 9))
  simp <- summary(simple_by_lean, infer = TRUE)
  int_s <- summary(inter, infer = TRUE)
  rows <- bind_rows(
    tibble(contrast = paste(simp$contrast, "|", simp$Lean), type = "simple_within_lean",
           estimate = simp$estimate, se = simp$SE, df = simp$df,
           p_raw = simp$p.value, p_fdr = p.adjust(simp$p.value, "fdr"),   # focal family: the 2 simple effects
           lo95 = simp$lower.CL, hi95 = simp$upper.CL),
    tibble(contrast = paste(int_s$BiasSide_pairwise, "x", int_s$Lean_pairwise), type = "interaction",
           estimate = int_s$estimate, se = int_s$SE, df = int_s$df,
           p_raw = int_s$p.value, p_fdr = int_s$p.value,                  # single-test family
           lo95 = int_s$lower.CL, hi95 = int_s$upper.CL))
  # BiasSide main comparison (reference c1 lines 118-133) -- its own single-test family
  emm_bias <- emmeans(m_ols, ~ BiasSide, vcov. = vc)
  cb <- summary(contrast(emm_bias, "pairwise"), infer = TRUE)
  rows <- bind_rows(rows,
    tibble(contrast = as.character(cb$contrast), type = "biasside_main",
           estimate = cb$estimate, se = cb$SE, df = cb$df,
           p_raw = cb$p.value, p_fdr = cb$p.value,
           lo95 = cb$lower.CL, hi95 = cb$upper.CL))
  rows <- rows %>%
    mutate(outcome = key, variant = variant, .before = 1) %>%
    mutate(hedges_g = estimate / pooled_sd * corr,
           hedges_g_lo = lo95 / pooled_sd * corr, hedges_g_hi = hi95 / pooled_sd * corr,
           interpretation = cut(abs(hedges_g), c(-Inf, 0.2, 0.5, 0.8, Inf),
                                labels = c("negligible", "small", "medium", "large")))
  cat("\nHedges' g (raw outcome SD", round(pooled_sd, 3), ", correction", round(corr, 4), "):\n")
  print(as.data.frame(rows[, c("contrast", "type", "estimate", "p_fdr", "hedges_g", "interpretation")]), digits = 3)

  cells <- bind_rows(
    as.data.frame(emm_full) %>%
      transmute(outcome = key, variant = variant, grouping = "BiasSide x Lean",
                BiasSide = as.character(BiasSide), Lean = as.character(Lean),
                emmean, SE, lower.CL, upper.CL),
    as.data.frame(emm_bias) %>%
      transmute(outcome = key, variant = variant, grouping = "BiasSide",
                BiasSide = as.character(BiasSide), Lean = NA_character_,
                emmean, SE, lower.CL, upper.CL))
  list(cells = cells, contrasts = rows, model = m_ols, vcov = vc)
}

primary <- list()
for (key in names(OUTCOMES)) {
  primary[[key]] <- analyze_outcome(repdem, OUTCOMES[[key]]$col, OUTCOMES[[key]]$label, key, "dominant_sign")
  emm_cells_out[[paste0(key, "_p")]] <- primary[[key]]$cells
  contrasts_out[[paste0(key, "_p")]] <- primary[[key]]$contrasts
}

# ---- robustness: the same models with the prior-experience covariates added ----
cat("\n===== ROBUSTNESS: + prior_prog_exp + prior_ml_exp =====\n")
for (key in names(OUTCOMES)) {
  rc <- analyze_outcome(repdem, OUTCOMES[[key]]$col, OUTCOMES[[key]]$label, key, "dominant_sign_cov",
                        covars = "+ prior_prog_exp + prior_ml_exp")
  emm_cells_out[[paste0(key, "_cov")]] <- rc$cells
  contrasts_out[[paste0(key, "_cov")]] <- rc$contrasts
}

# ---- dose diagnostic: AI usage is imbalanced across the echo/opposition contrast ----
cat("\n===== DOSE CHECK: AI usage by bias side (biased-arm student-weeks) =====\n")
dose <- repdem %>% filter(!is.na(coding_hg), coding_hg > 0) %>%
  group_by(BiasSide) %>%
  summarise(n = n(), zero_usage_weeks = sum(conv_round == 0),
            pct_zero = round(100 * mean(conv_round == 0)),
            median_requests = median(event_request_count), .groups = "drop")
print(as.data.frame(dose), row.names = FALSE)
dtab <- repdem %>% filter(!is.na(coding_hg), coding_hg > 0) %>%
  mutate(used = conv_round > 0)
cat(sprintf("Fisher exact p (zero-usage x BiasSide) = %.4f\n",
            fisher.test(table(dtab$BiasSide, dtab$used))$p.value))
cat(sprintf("mean coding score: zero-usage %.3f (n=%d) vs used %.3f (n=%d)\n",
            mean(dtab$coding_hg[!dtab$used]), sum(!dtab$used),
            mean(dtab$coding_hg[dtab$used]),  sum(dtab$used)))
cat("-> echo-chamber weeks are disproportionately weeks with NO assistant use, and\n",
    "   zero-usage weeks score higher; the echo/opposition contrast is dose-confounded.\n")

# Reference-faithful robustness: drop the near-neutral band (|direction| < 0.5, the
# "Independent" analog). Reported for coding only; cells get small.
band <- repdem %>% filter(abs(direction_score) >= 0.5)
cat("\n===== ROBUSTNESS: |direction_score| >= 0.5 (Independent-band analog) =====\n")
rb <- analyze_outcome(band, OUTCOMES$coding$col, OUTCOMES$coding$label, "coding", "dominant_band0.5")
emm_cells_out$coding_b <- rb$cells; contrasts_out$coding_b <- rb$contrasts

write_csv(bind_rows(emm_cells_out), file.path(OUT, "c1_emmeans_cells.csv"))
write_csv(bind_rows(contrasts_out), file.path(OUT, "c1_contrasts_hedges.csv"))

# ======================================
# Secondary: per-dimension echo (the study's Figure-16 approach), pooled model
# ======================================
perdim <- bind_rows(
  cw %>% filter(policy == "NOVELTY_BIASED", !is.na(echo_novelty)) %>%
    mutate(dimension = "novelty", echo = echo_novelty),
  cw %>% filter(policy == "RELIABILITY_BIASED", !is.na(echo_reliability)) %>%
    mutate(dimension = "reliability", echo = echo_reliability)) %>%
  mutate(Echo = factor(ifelse(echo == 1, "Echo", "Opposition"), levels = c("Opposition", "Echo")),
         dimension = factor(dimension))
cat("\n===== PER-DIMENSION ECHO (secondary) =====\ncells:\n")
print(table(perdim$Echo, perdim$dimension))
perdim_rows <- list()
for (key in names(OUTCOMES)) {
  col <- OUTCOMES[[key]]$col
  d <- perdim[!is.na(perdim[[col]]) & perdim[[col]] > 0, ]
  m <- lmer(as.formula(paste(col, "~ Echo * dimension + prior_prog_exp + prior_ml_exp + as.factor(week) + (1|student_id)")),
            data = d)
  cat(sprintf("\n--- %s: Echo x dimension mixed model ---\n", toupper(OUTCOMES[[key]]$label)))
  print(summary(m)$coefficients)
  emm <- emmeans(m, ~ Echo * dimension)
  print(emm)
  perdim_rows[[key]] <- as.data.frame(emm) %>%
    transmute(outcome = key, Echo = as.character(Echo), dimension = as.character(dimension),
              emmean, SE, lower.CL, upper.CL)
}
write_csv(bind_rows(perdim_rows), file.path(OUT, "c1_perdim_cells.csv"))

# ======================================
# Visualization (reference c1 lines 162-336) — coding, dominant-construct primary
# ======================================
m_plot <- primary$coding$model; vc_plot <- primary$coding$vcov
default_avg <- cw %>%
  filter(is_default_reference == 1, coding_hg > 0) %>%
  summarise(m = mean(coding_hg, na.rm = TRUE)) %>% pull(m)
cat(sprintf("\nDefault-arm mean coding performance (reference line): %.3f\n", default_avg))

theme_c1 <- theme_classic() +
  theme(text = element_text(family = FONT),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
        axis.line = element_blank(),
        axis.title.x = element_text(family = FONT, size = 12, margin = margin(t = 12)),
        axis.title.y = element_text(family = FONT, size = 12, color = "black", margin = margin(r = 12)),
        axis.text.x = element_text(family = FONT, size = 10, color = "black"),
        axis.text.y = element_text(family = FONT, size = 10, color = "black", angle = 90, hjust = 0.5),
        axis.ticks = element_line(color = "black", linewidth = 0.4),
        axis.ticks.length = unit(3, "pt"), panel.grid = element_blank(),
        plot.margin = margin(t = 15, r = 20, b = 15, l = 15))

# 2-bar BiasSide plot
emm_bias_df <- as.data.frame(emmeans(m_plot, ~ BiasSide, vcov. = vc_plot))
color_bias <- "#228B22"
xmax <- max(emm_bias_df$upper.CL)
p_bias_side <- ggplot(emm_bias_df, aes(y = BiasSide, x = emmean)) +
  geom_col(fill = color_bias, alpha = 0.7, width = 0.6) +
  geom_vline(xintercept = default_avg, linetype = "dashed", color = "gray60", linewidth = 0.5) +
  geom_errorbar(aes(xmin = lower.CL, xmax = upper.CL), width = 0.2, color = "black", linewidth = 0.6) +
  geom_point(shape = 15, size = 3, color = "black") +
  geom_text(aes(x = upper.CL + xmax * 0.015, label = round(emmean, 2)),
            hjust = 0, size = 3.7, family = FONT) +
  scale_x_continuous(name = "Coding performance (model-adjusted)",
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_discrete(name = "AI bias direction",
                   labels = c("Opposite" = "Opposition bias", "Same" = "Echo-chamber bias")) +
  coord_cartesian(xlim = c(7.5, 10)) +
  theme_c1
save_png(file.path(OUT, "bias_side_performance.png"), p_bias_side, width = 4.4, height = 3.6)

# nature theme for the 4-cell figure (FONT = Avenir on macOS, sans fallback elsewhere)
nature_theme <- theme_classic() +
  theme(
    text = element_text(family = FONT, size = 8),
    plot.title = element_text(family = FONT, size = 10, face = "bold", hjust = 0),
    axis.title = element_text(family = FONT, size = 9, face = "plain"),
    axis.text = element_text(family = FONT, size = 8, color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 10)
  )

# ── 4-cell plot, BOTH deliverables (styled on notebooks/R/bias_side_counterfactual.R)
# y = bias side (Opposition on top); within each group 4 dodged bars = student lean
# (fill: novel teal / reliable brown) x deliverable (solid = coding, the primary
# outcome; semi-transparent + diagonal stripes = memo, the secondary one, exactly as
# the investment figure marks its bull counterfactual). Dashed lines = the Default-arm
# mean for each deliverable. No significance annotation (added by hand downstream).
default_memo <- cw %>% filter(is_default_reference == 1, memo_regrade > 0) %>%
  summarise(m = mean(memo_regrade, na.rm = TRUE)) %>% pull(m)
cat(sprintf("Default-arm mean memo performance (reference line): %.3f\n", default_memo))

pd4 <- bind_rows(primary$coding$cells, primary$memo$cells) %>%
  filter(grouping == "BiasSide x Lean") %>%
  mutate(Deliverable = factor(ifelse(outcome == "coding", "Coding", "Memo"),
                              levels = c("Coding", "Memo")),
         BiasSide = factor(BiasSide, levels = c("Same", "Opposite")),
         Lean = factor(Lean, levels = c("Novel-leaning", "Reliable-leaning")),
         grp = factor(paste(Lean, Deliverable, sep = " | "),
                      levels = c("Reliable-leaning | Memo", "Novel-leaning | Memo",
                                 "Reliable-leaning | Coding", "Novel-leaning | Coding")))
lean_colors <- c("Novel-leaning" = "#01665E", "Reliable-leaning" = "#8C510A")  # BrBG teal / brown
dodge <- position_dodge(width = 0.82)
xpad <- 0.1 * diff(range(c(pd4$lower.CL, pd4$upper.CL)))
pd4$lab_x <- pd4$upper.CL + xpad * 0.45
# extra right-hand headroom past the value labels, for significance brackets added
# by hand downstream (raise the multiplier for more space)
XLIM <- c(min(pd4$lower.CL) - xpad * 0.5, max(pd4$upper.CL) + xpad * 10)

p_bias_lean <- ggplot(pd4, aes(y = BiasSide, x = emmean, group = grp)) +
  geom_vline(xintercept = default_avg, linetype = "dashed", color = "grey70", linewidth = 0.5) +
  geom_vline(xintercept = default_memo, linetype = "dashed", color = "grey70",
             linewidth = 0.5, alpha = 0.4) +
  ggpattern::geom_col_pattern(
    aes(fill = Lean, alpha = Deliverable, pattern = Deliverable),
    position = dodge, width = 0.72, color = NA,
    pattern_angle = 135, pattern_colour = NA, pattern_fill = "grey25",
    pattern_density = 0.12, pattern_spacing = 0.025, pattern_alpha = 0.55,
    pattern_key_scale_factor = 0.55) +
  ggpattern::scale_pattern_manual(values = c("Coding" = "none", "Memo" = "stripe"),
                                  name = NULL) +
  geom_errorbar(aes(xmin = lower.CL, xmax = upper.CL), position = dodge,
                width = 0.2, linewidth = 0.6, color = "black") +
  geom_text(aes(x = lab_x, label = sprintf("%.2f", emmean)), hjust = 0,
            position = dodge, family = FONT, size = 4, color = "black") +
  scale_fill_manual(values = lean_colors, name = "Student lean") +
  scale_alpha_manual(values = c("Coding" = 1, "Memo" = 0.35), name = NULL) +
  scale_x_continuous(name = "Assignment Performance",
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_discrete(name = "AI Bias Direction", limits = c("Same", "Opposite"),
                   labels = c("Opposite" = "Opposition Bias", "Same" = "Echo-Chamber Bias")) +
  # zoom rather than clip: geom_col draws from x = 0, so a scale `limits` would
  # drop every bar instead of cropping the axis
  coord_cartesian(xlim = XLIM) +
  guides(fill = "none", alpha = "none",
         pattern = guide_legend(override.aes = list(fill = "grey50", alpha = 1), ncol = 1)) +
  theme_classic() +
  theme(
    text = element_text(family = "Avenir"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.line = element_blank(),
    axis.title.x = element_text(family = "Avenir", size = 12, margin = margin(t = 12)),
    axis.title.y = element_text(family = "Avenir", size = 12, color = "black", margin = margin(r = 12)),
    axis.text.x = element_text(family = "Avenir", size = 10, color = "black"),
    axis.text.y = element_text(family = "Avenir", size = 10, color = "black", hjust = 0.5, angle = 90),
    axis.ticks = element_line(color = "black", linewidth = 0.4),
    axis.ticks.length = unit(3, "pt"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    panel.grid = element_blank(),
    plot.margin = margin(t = 15, r = 20, b = 15, l = 15),
    legend.position = c(0.97, 0.99),          # inside panel, upper-right corner
    legend.justification = c(1, 1),
    legend.background = element_blank(),
    legend.key.size = unit(12, "pt"),
    legend.title = element_blank(),
    legend.text = element_text(family = FONT, size = 11)
  )

save_png(file.path(OUT, "bias_side_performance_4cell.png"), p_bias_lean, width = 5, height = 5, dpi = 500)

p_bias_lean
cat("\nSaved bias_side_performance.png and bias_side_performance_4cell.png in", OUT, "\n")
cat("Done: bias_side_performance.R\n")
