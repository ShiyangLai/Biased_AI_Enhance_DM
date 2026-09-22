# ==============================================================================
# neutral_arm_helpfulness.R — perceived-helpfulness ladder across the four arms,
# in the same style as neutral_arm_engagement.R's reply-turns panel.
#
#   figures/neutral_arm_helpfulness.png      use-conditioned (matches bm panel 3 and the
#                                    reply-turn panels)
#   figures/neutral_arm_helpfulness_all_weeks.png  all surveyed weeks (companion)
#   figures/neutral_arm_helpfulness_pairwise.csv
#
# *** COVERAGE WARNING ***  The post-assignment survey barely reached the
# Neutralized arm: 2 responses in total, and only 1 of those in a week with any
# assistant use. Its plotted value is therefore a single student-week (or two)
# and is not an arm mean in any useful sense -- the interval is drawn wide for
# exactly that reason. Consider reporting this panel with three arms.
#
# Outcome : post-survey "usefulness" item (1-7)
# Model   : lm(usefulness ~ arm + week FE + student FE), student-clustered SE;
#           adjusted means with nested 90/95/99% CIs.
# Inputs  : data/post_survey_student_week.csv, data/telemetry_student_week.csv
#   setwd("education/code"); source("_setup.R"); source("neutral_arm_helpfulness.R")
# ==============================================================================
suppressPackageStartupMessages({
  library(ggplot2); library(emmeans); library(dplyr); library(readr)
  library(sandwich); library(scales)
})

IN <- DATA_DIR; OUT <- FIG_DIR

ARMS <- c("Default", "Novelty", "Neutralized", "Reliability")
tel <- read_csv(file.path(IN, "telemetry_student_week.csv"), show_col_types = FALSE)
d <- read_csv(file.path(IN, "post_survey_student_week.csv"), show_col_types = FALSE) %>%
  # qualified: MASS::select masks dplyr's if MASS is attached in the session
  dplyr::select(student_id, week, policy, usefulness) %>%
  filter(!is.na(usefulness), policy != "QUESTIONING_ORIENTED") %>%
  left_join(tel, by = c("student_id", "week")) %>%
  mutate(conv_round = ifelse(is.na(conv_round), 0, conv_round),
         arm = factor(dplyr::recode(policy, DEFAULT = "Default", NEUTRAL = "Neutralized",
                                    NOVELTY_BIASED = "Novelty",
                                    RELIABILITY_BIASED = "Reliability"), levels = ARMS),
         week = factor(week), student_id = factor(student_id))
cat("=== survey responses by arm ===\n")
print(d %>% group_by(arm) %>%
        summarise(responses = n(), use_positive = sum(conv_round > 0),
                  students = n_distinct(student_id), .groups = "drop"))

YORDER <- c("Novelty" = 4, "Default" = 3, "Neutralized" = 2, "Reliability" = 1)
arm_colors <- c("Novelty" = "#01665E", "Default" = "#BAB0AC",
                "Neutralized" = "#79706E", "Reliability" = "#8C510A")
lab_top2bottom <- names(sort(YORDER, decreasing = TRUE))
pair_out <- list()

fit_arm <- function(dat, lab, key) {
  m  <- lm(usefulness ~ arm + week + student_id, data = dat)
  vc <- vcovCL(m, cluster = dat$student_id[as.integer(rownames(model.frame(m)))])
  em <- suppressMessages(emmeans(m, ~ arm, vcov. = vc))
  es <- as.data.frame(summary(em))
  cat(sprintf("\n=== %s (n=%d) — adjusted means (student FE, clustered) ===\n", lab, nrow(dat)))
  print(cbind(es[, c("arm", "emmean", "SE")], n = as.integer(table(droplevels(dat$arm)))),
        row.names = FALSE, digits = 4)
  pw <- as.data.frame(summary(pairs(em, adjust = "none"), infer = TRUE))
  pw$family <- ifelse(grepl("Default", pw$contrast), "vs Default", "arm vs arm")
  pw$p_fdr  <- ave(pw$p.value, pw$family, FUN = function(p) p.adjust(p, "fdr"))
  print(pw[, c("contrast", "family", "estimate", "lower.CL", "upper.CL", "p.value", "p_fdr")],
        row.names = FALSE, digits = 3)
  pair_out[[key]] <<- data.frame(sample = key, contrast = as.character(pw$contrast),
    family = pw$family, estimate = pw$estimate, lo = pw$lower.CL, hi = pw$upper.CL,
    p_raw = pw$p.value, p_fdr = pw$p_fdr)
  es %>% mutate(arm = as.character(arm), y_position = YORDER[arm],
    CI_90_lower = emmean - SE * qnorm(.95),  CI_90_upper = emmean + SE * qnorm(.95),
    CI_95_lower = emmean - SE * qnorm(.975), CI_95_upper = emmean + SE * qnorm(.975),
    CI_99_lower = emmean - SE * qnorm(.995), CI_99_upper = emmean + SE * qnorm(.995))
}

nature_theme <- theme_classic() +
  theme(
    text = element_text(family = FONT, size = 8),
    axis.title = element_text(family = FONT, size = 9, face = "plain"),
    axis.text = element_text(family = FONT, size = 8, color = "black"),
    axis.text.y = element_text(family = FONT, size = 9, color = "black"),
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.ticks.length = unit(0.15, "cm"),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 10),
    legend.position = "none",
    # the outcome name is carried by a bold facet strip at the bottom rather than
    # an axis title, so this panel drops straight in beside neutral_arm_engagement_reply_turns.png
    strip.background = element_blank(), strip.placement = "outside",
    strip.text = element_text(family = FONT, size = 9.5, face = "bold",
                              margin = margin(t = 0, b = 3))
  )

ladder <- function(pd) {
  pd$outcome <- "Perceived Helpfulness"
  ggplot(pd, aes(y = y_position)) +
    geom_errorbarh(aes(xmin = CI_99_lower, xmax = CI_99_upper, color = arm),
                   height = .25, linewidth = 1.2, alpha = .2) +
    geom_errorbarh(aes(xmin = CI_95_lower, xmax = CI_95_upper, color = arm),
                   height = .20, linewidth = .9, alpha = .4) +
    geom_errorbarh(aes(xmin = CI_90_lower, xmax = CI_90_upper, color = arm),
                   height = .15, linewidth = .7, alpha = .7) +
    geom_point(aes(x = emmean, color = arm), size = 3, alpha = .8) +
    facet_wrap(~ outcome, nrow = 1, strip.position = "bottom") +
    scale_color_manual(values = arm_colors, breaks = lab_top2bottom, guide = "none") +
    scale_y_continuous(breaks = 4:1, labels = lab_top2bottom,
                       expand = expansion(add = c(.4, .4))) +
    scale_x_continuous(labels = label_number(accuracy = 1),
                       expand = expansion(mult = c(.3, .3))) +
    labs(x = NULL, y = NULL) + nature_theme
}

pd_use <- fit_arm(d[d$conv_round > 0, ], "USE-CONDITIONED", "use-conditioned")
pd_all <- fit_arm(d,                      "ALL SURVEYED WEEKS", "all weeks")
write_csv(bind_rows(pair_out), file.path(OUT, "neutral_arm_helpfulness_pairwise.csv"))
save_png(file.path(OUT, "neutral_arm_helpfulness.png"),     ladder(pd_use), width = 2.5, height = 2.8)
save_png(file.path(OUT, "neutral_arm_helpfulness_all_weeks.png"), ladder(pd_all), width = 2.5, height = 2.8)
cat(sprintf("\nSaved neutral_arm_helpfulness.png, neutral_arm_helpfulness_all_weeks.png, neutral_arm_helpfulness_pairwise.csv in %s\n", OUT))
cat("NOTE: the Neutralized arm has 2 survey responses (1 use-conditioned) -- see header.\n")
cat("Done: neutral_arm_helpfulness.R\n")
