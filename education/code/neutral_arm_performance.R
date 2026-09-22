# ==============================================================================
# neutral_arm_performance.R — FOUR-ARM version of
# plot_performance_treatment.R: adds the explicitly de-biased NEUTRALIZED arm to
# the Novelty / Default / Reliability ladder, for coding and memo performance.
#
# Why a separate script: the 3-arm figure is the one currently cited in the
# manuscript, and adding a fourth arm CHANGES THE FDR FAMILY (3 focal contrasts
# vs Default instead of 2), so every adj. p moves. Keeping both lets the 3-arm
# numbers stand until the 4-arm version is adopted deliberately.
#
# Data spine is data/scores_all_graders_student_week.csv (the authoritative
# 5-arm assignment, 351 student-weeks) rather than bm_student_week.csv, which was
# filtered upstream to the three original arms. QUESTIONING_ORIENTED (23 weeks)
# is excluded here -- out of scope for this figure.
#
# Outcomes (reliability-matched grader sets, as everywhere else in this repo):
#   coding = 2 human graders + Gemini (equal per-grader weight, Gemini
#            mean-aligned to the human scale)
#   memo   = the 2 human graders only (LLM memo grades track humans at r ~ .3)
# Model  : lm(y ~ arm + week FE + student FE), student-clustered SE; adjusted
#          arm means from emmeans with nested 90/95/99% CI ladders.
#
# Inputs : data/scores_all_graders_student_week.csv,
#          data/coding_regrade_student_week.csv, data/memo_regrade_student_week.csv
# Outputs: figures/neutral_arm_performance.png,
#          figures/neutral_arm_performance_contrasts.csv
#   setwd("education/code"); source("_setup.R"); source("neutral_arm_performance.R")
# ==============================================================================
suppressPackageStartupMessages({
  library(ggplot2); library(emmeans); library(dplyr); library(readr)
  library(sandwich); library(scales)
})

IN <- DATA_DIR; OUT <- FIG_DIR

ARMS <- c("Default", "Novelty", "Neutralized", "Reliability")   # Default = reference

d <- read_csv(file.path(IN, "scores_all_graders_student_week.csv"), show_col_types = FALSE) %>%
  dplyr::select(student_id, week, policy, gem = gemini_asgn_score) %>%
  filter(policy != "QUESTIONING_ORIENTED") %>%
  left_join(read_csv(file.path(IN, "coding_regrade_student_week.csv"), show_col_types = FALSE),
            by = c("student_id", "week")) %>%
  left_join(read_csv(file.path(IN, "memo_regrade_student_week.csv"), show_col_types = FALSE),
            by = c("student_id", "week")) %>%
  # engagement panel: reply turns rebuilt from the raw transcript log, which
  # reproduces bm's conv_round EXACTLY (r = 1.000, 100% of 312 rows) and is the
  # only source covering the Neutralized arm. Joined BEFORE the factor() cast so
  # the integer week keys still match. log1p() sends zero-usage weeks to exactly
  # 0, so ladder()'s `> 0` filter use-conditions this panel the same way the
  # reply-turn analyses elsewhere in this repo do.
  left_join(read_csv(file.path(IN, "telemetry_student_week.csv"), show_col_types = FALSE),
            by = c("student_id", "week")) %>%
  mutate(conv_round = ifelse(is.na(conv_round), 0, conv_round),
         turns_log = log1p(conv_round),
         arm = factor(dplyr::recode(policy, DEFAULT = "Default", NEUTRAL = "Neutralized",
                                    NOVELTY_BIASED = "Novelty",
                                    RELIABILITY_BIASED = "Reliability"), levels = ARMS),
         week = factor(week), student_id = factor(student_id))
# coding composite: 2 humans + Gemini, Gemini mean-aligned on overlapping rows
GEM_SHIFT <- mean(d$coding_regrade[!is.na(d$gem)], na.rm = TRUE) -
             mean(d$gem[!is.na(d$coding_regrade)], na.rm = TRUE)
d$coding_hg <- ifelse(is.na(d$gem), d$coding_regrade,
                      (2 * d$coding_regrade + d$gem + GEM_SHIFT) / 3)
d$coding_hg[is.na(d$coding_regrade)] <- NA
cat(sprintf("coding composite: Gemini mean-alignment shift %+.3f\n", GEM_SHIFT))
cat("student-weeks by arm:\n"); print(table(d$arm))

hedges_J <- function(n1, n2) { k <- n1 + n2 - 2; ifelse(k > 0, 1 - 3 / (4 * k - 1), 1) }
contr_out <- list(); pair_out <- list()

ladder <- function(ycol, panel) {
  dat <- d[!is.na(d[[ycol]]) & d[[ycol]] > 0, ]
  m  <- lm(as.formula(paste(ycol, "~ arm + week + student_id")), data = dat)
  vc <- vcovCL(m, cluster = dat$student_id[as.integer(rownames(model.frame(m)))])
  em <- emmeans(m, ~ arm, vcov. = vc)
  es <- as.data.frame(summary(em))
  cat(sprintf("\n=== %s — adjusted arm means (student FE, student-clustered) ===\n", panel))
  print(cbind(es[, c("arm", "emmean", "SE")], n = as.integer(table(dat$arm))),
        row.names = FALSE, digits = 4)
  # focal family = the THREE arm-vs-Default contrasts; FDR within it
  pw <- as.data.frame(summary(pairs(em, adjust = "none"), infer = TRUE))
  foc <- pw[grepl("Default", pw$contrast), ]
  foc$p_fdr <- p.adjust(foc$p.value, "fdr")
  nb <- table(dat$arm); ysd <- sd(dat[[ycol]], na.rm = TRUE)
  foc$g <- mapply(function(e, ct) {
    gg <- strsplit(gsub("[()]", "", as.character(ct)), " - ")[[1]]
    (e / ysd) * hedges_J(nb[[gg[1]]], nb[[gg[2]]]) }, foc$estimate, foc$contrast)
  # harmonise sign to "arm - Default"
  flip <- grepl("^\\s*Default", foc$contrast)
  cat("--- vs Default (positive = arm higher; FDR over the 3 focal contrasts) ---\n")
  print(data.frame(arm = trimws(gsub("Default|-", "", foc$contrast)),
                   estimate = ifelse(flip, -foc$estimate, foc$estimate),
                   lo = ifelse(flip, -foc$upper.CL, foc$lower.CL),
                   hi = ifelse(flip, -foc$lower.CL, foc$upper.CL),
                   g = ifelse(flip, -foc$g, foc$g),
                   p_raw = foc$p.value, p_fdr = foc$p_fdr),
        row.names = FALSE, digits = 3)
  contr_out[[panel]] <<- data.frame(outcome = panel,
    arm = trimws(gsub("Default|-", "", foc$contrast)),
    estimate = ifelse(flip, -foc$estimate, foc$estimate),
    lo = ifelse(flip, -foc$upper.CL, foc$lower.CL),
    hi = ifelse(flip, -foc$lower.CL, foc$upper.CL),
    g = ifelse(flip, -foc$g, foc$g), p_raw = foc$p.value, p_fdr = foc$p_fdr)
  # ---- ALL six pairwise contrasts, FDR by family --------------------------
  # focal family  = the 3 arm-vs-Default contrasts (above)
  # secondary     = the 3 arm-vs-arm contrasts among the non-Default arms
  pw$fam <- ifelse(grepl("Default", pw$contrast), "vs Default", "arm vs arm")
  pw$p_fdr <- ave(pw$p.value, pw$fam, FUN = function(p) p.adjust(p, "fdr"))
  pw$g <- mapply(function(e, ct) {
    gg <- strsplit(gsub("[()]", "", as.character(ct)), " - ")[[1]]
    (e / ysd) * hedges_J(nb[[gg[1]]], nb[[gg[2]]]) }, pw$estimate, pw$contrast)
  cat("--- all pairwise (as printed: first arm - second arm; FDR within family) ---\n")
  print(pw[, c("contrast", "fam", "estimate", "lower.CL", "upper.CL", "g", "p.value", "p_fdr")],
        row.names = FALSE, digits = 3)
  pair_out[[panel]] <<- data.frame(outcome = panel, contrast = as.character(pw$contrast),
    family = pw$fam, estimate = pw$estimate, lo = pw$lower.CL, hi = pw$upper.CL,
    g = pw$g, p_raw = pw$p.value, p_fdr = pw$p_fdr)

  es %>% mutate(
    arm = as.character(arm), y_position = YORDER[arm], outcome = panel,
    CI_90_lower = emmean - SE * qnorm(0.95),  CI_90_upper = emmean + SE * qnorm(0.95),
    CI_95_lower = emmean - SE * qnorm(0.975), CI_95_upper = emmean + SE * qnorm(0.975),
    CI_99_lower = emmean - SE * qnorm(0.995), CI_99_upper = emmean + SE * qnorm(0.995))
}

# top-to-bottom order follows the conceptual bias axis, Default kept as the
# interior reference: Novelty / Default / Neutralized / Reliability
YORDER <- c("Novelty" = 4, "Default" = 3, "Neutralized" = 2, "Reliability" = 1)
# gray for BOTH non-directional arms (Default, Neutralized); teal/brown carry the
# two bias directions. Rows are identified by the y-axis labels, so no legend.
arm_colors <- c("Novelty" = "#01665E", "Default" = "#BAB0AC",
                "Neutralized" = "#79706E", "Reliability" = "#8C510A")
lab_top2bottom <- names(sort(YORDER, decreasing = TRUE))

# reply turns live in their own standalone figure (neutral_arm_engagement.R);
# this figure stays the two performance outcomes
PANELS <- c("Coding Performance", "Memo Performance")
pd <- bind_rows(ladder("coding_hg", PANELS[1]),
                ladder("memo_regrade", PANELS[2])) %>%
  mutate(outcome = factor(outcome, levels = PANELS))
write_csv(bind_rows(contr_out), file.path(OUT, "neutral_arm_performance_contrasts.csv"))
write_csv(bind_rows(pair_out), file.path(OUT, "neutral_arm_performance_pairwise.csv"))

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

p <- ggplot(pd, aes(y = y_position)) +
  geom_errorbarh(aes(xmin = CI_99_lower, xmax = CI_99_upper, color = arm),
                 height = 0.25, linewidth = 1.2, alpha = 0.4) +
  geom_errorbarh(aes(xmin = CI_95_lower, xmax = CI_95_upper, color = arm),
                 height = 0.20, linewidth = 0.9, alpha = 0.6) +
  geom_errorbarh(aes(xmin = CI_90_lower, xmax = CI_90_upper, color = arm),
                 height = 0.15, linewidth = 0.7, alpha = 0.85) +
  geom_point(aes(x = emmean, color = arm), size = 3, alpha = 0.8) +
  facet_wrap(~ outcome, nrow = 1, scales = "free_x", strip.position = "bottom") +
  scale_color_manual(values = arm_colors, breaks = lab_top2bottom, guide = "none") +
  scale_y_continuous(breaks = 4:1, labels = lab_top2bottom,
                     expand = expansion(add = c(0.4, 0.4))) +
  scale_x_continuous(labels = label_number(accuracy = 0.1),
                     expand = expansion(mult = c(0.2, 1))) +
  labs(x = NULL, y = NULL) +
  # widened from the 3-arm figure's xlim(8, 11): the Neutralized coding cell
  # (n = 11) has a 99% CI reaching ~6.9, which xlim(8, .) silently clipped
  # no fixed xlim: the three panels are on different scales (grade points vs a log
  # count), so each free_x facet sets its own range -- a shared limit clipped the
  # Neutralized coding interval, whose 99% bound reaches ~6.9
  nature_theme +
  theme(
    legend.position = "none",
    strip.background = element_blank(), strip.placement = "outside",
    strip.text = element_text(family = FONT, size = 9.5, face = "bold",
                              margin = margin(t = 0, b = 3)),
    panel.spacing = unit(1.2, "lines")
  )
p
save_png(file.path(OUT, "neutral_arm_performance.png"), p,
         width = 3.8, height = 2.8, dpi = 500)
cat(sprintf("\nSaved neutral_arm_performance.png, neutral_arm_performance_contrasts.csv in %s\n", OUT))
cat("Done: neutral_arm_performance.R\n")
