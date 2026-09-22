# ==============================================================================
# neutral_arm_engagement.R — conversation-volume engagement across FOUR arms
# (Novelty / Default / Neutralized / Reliability), using ONLY the two telemetry
# measures that are exactly reconstructable for every arm.
#
# Why this script exists: the derived analysis files (a3/b1/bm/c1_student_week.csv)
# were filtered upstream to the three original arms, so e1_engagement_llm_heatmap.R
# had to reconstruct Neutral telemetry, and two of its five dimensions
# (event_request_count, session minutes) are only calibrated proxies (r ~ .97,
# ~60% exact). Rebuilding conv_round and conv_length straight from
# telemetry_student_week.csv reproduces the authoritative bm columns EXACTLY
# (r = 1.000, 100% of 312 rows), so those two measures carry no proxy caveat and
# are available for all arms:
#     conv_round  <- count of user messages per student-week
#     conv_length <- sum of user_words per student-week
#
# Model: lm(log1p(y) ~ arm + week FE + student FE), student-clustered SE — the
# project's within-student spec. Outcomes are logged (both are heavily
# right-skewed) and use-conditioned in the primary analysis (a turn count of zero
# is an absence of conversation, not a short one); an all-weeks companion that
# keeps the zeros is reported alongside.
# FDR by family: the 3 arm-vs-Default contrasts are focal, the 3 arm-vs-arm
# contrasts form a second family.
#
# Inputs : data/telemetry_student_week.csv,
#          data/scores_all_graders_student_week.csv (authoritative 5-arm assignment)
# Outputs: figures/neutral_arm_engagement_pairwise.csv,
#          figures/neutral_arm_engagement.png
#   setwd("education/code"); source("_setup.R"); source("neutral_arm_engagement.R")
# ==============================================================================
suppressPackageStartupMessages({
  library(ggplot2); library(emmeans); library(dplyr); library(readr)
  library(sandwich); library(scales)
})

IN <- DATA_DIR; OUT <- FIG_DIR

ARMS <- c("Default", "Novelty", "Neutralized", "Reliability")

# ── exact telemetry from the raw transcript log ──────────────────────────────
tel <- read_csv(file.path(IN, "telemetry_student_week.csv"), show_col_types = FALSE)

d <- read_csv(file.path(IN, "scores_all_graders_student_week.csv"), show_col_types = FALSE) %>%
  dplyr::select(student_id, week, policy) %>%
  filter(policy != "QUESTIONING_ORIENTED") %>%
  left_join(tel, by = c("student_id", "week")) %>%
  mutate(across(c(conv_round, conv_length), ~ifelse(is.na(.x), 0, .x)),
         arm = factor(dplyr::recode(policy, DEFAULT = "Default", NEUTRAL = "Neutralized",
                                    NOVELTY_BIASED = "Novelty",
                                    RELIABILITY_BIASED = "Reliability"), levels = ARMS),
         week = factor(week), student_id = factor(student_id))
cat("=== student-weeks and usage by arm ===\n")
print(d %>% group_by(arm) %>%
        summarise(weeks = n(), with_usage = sum(conv_round > 0),
                  median_turns_users = median(conv_round[conv_round > 0]),
                  median_words_users = median(conv_length[conv_length > 0]), .groups = "drop"))

hedges_J <- function(n1, n2) { k <- n1 + n2 - 2; ifelse(k > 0, 1 - 3 / (4 * k - 1), 1) }
pair_out <- list(); viz <- list()

analyse <- function(ycol, label, users_only = TRUE) {
  dat <- d; if (users_only) dat <- dat[dat$conv_round > 0, ]
  dat$.y <- log1p(dat[[ycol]])
  m  <- lm(.y ~ arm + week + student_id, data = dat)
  vc <- vcovCL(m, cluster = dat$student_id[as.integer(rownames(model.frame(m)))])
  em <- emmeans(m, ~ arm, vcov. = vc)
  es <- as.data.frame(summary(em))
  cat(sprintf("\n=== %s%s — adjusted means on log(1+x) (student FE, clustered) ===\n",
              label, if (users_only) " [use-conditioned]" else " [all weeks, zeros kept]"))
  print(cbind(es[, c("arm", "emmean", "SE")], n = as.integer(table(droplevels(dat$arm)))),
        row.names = FALSE, digits = 4)
  pw <- as.data.frame(summary(pairs(em, adjust = "none"), infer = TRUE))
  pw$family <- ifelse(grepl("Default", pw$contrast), "vs Default", "arm vs arm")
  pw$p_fdr  <- ave(pw$p.value, pw$family, FUN = function(p) p.adjust(p, "fdr"))
  nb <- table(droplevels(dat$arm)); ysd <- sd(dat$.y)
  pw$g <- mapply(function(e, ct) {
    gg <- strsplit(gsub("[()]", "", as.character(ct)), " - ")[[1]]
    (e / ysd) * hedges_J(nb[[gg[1]]], nb[[gg[2]]]) }, pw$estimate, pw$contrast)
  cat("--- all pairwise (first arm - second arm; FDR within family) ---\n")
  print(pw[, c("contrast", "family", "estimate", "lower.CL", "upper.CL", "g", "p.value", "p_fdr")],
        row.names = FALSE, digits = 3)
  key <- paste(label, if (users_only) "users" else "all")
  pair_out[[key]] <<- data.frame(outcome = label,
    sample = if (users_only) "use-conditioned" else "all weeks",
    contrast = as.character(pw$contrast), family = pw$family,
    estimate = pw$estimate, lo = pw$lower.CL, hi = pw$upper.CL,
    g = pw$g, p_raw = pw$p.value, p_fdr = pw$p_fdr)
  if (users_only)
    viz[[label]] <<- es %>% mutate(arm = as.character(arm), y_position = YORDER[arm],
      outcome = label,
      CI_90_lower = emmean - SE * qnorm(.95),  CI_90_upper = emmean + SE * qnorm(.95),
      CI_95_lower = emmean - SE * qnorm(.975), CI_95_upper = emmean + SE * qnorm(.975),
      CI_99_lower = emmean - SE * qnorm(.995), CI_99_upper = emmean + SE * qnorm(.995))
  invisible(NULL)
}

YORDER <- c("Novelty" = 4, "Default" = 3, "Neutralized" = 2, "Reliability" = 1)
arm_colors <- c("Novelty" = "#01665E", "Default" = "#BAB0AC",
                "Neutralized" = "#79706E", "Reliability" = "#8C510A")
lab_top2bottom <- names(sort(YORDER, decreasing = TRUE))

analyse("conv_round",  "log(1+Number of Reply Turns)")
analyse("conv_length", "log(1+Words Written)")
cat("\n########## companions keeping zero-usage weeks ##########\n")
analyse("conv_round",  "log(1+Number of Reply Turns)",   users_only = FALSE)
analyse("conv_length", "log(1+Words Written)", users_only = FALSE)
write_csv(bind_rows(pair_out), file.path(OUT, "neutral_arm_engagement_pairwise.csv"))

pd <- bind_rows(viz) %>%
  mutate(outcome = factor(outcome, levels = c("log(1+Number of Reply Turns)", "log(1+Words Written)")))

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
    plot.margin = margin(t = 20, r = 20, b = 20, l = 10)
  )

p <- ggplot(pd, aes(y = y_position)) +
  geom_errorbarh(aes(xmin = CI_99_lower, xmax = CI_99_upper, color = arm),
                 height = 0.25, linewidth = 1.2, alpha = 0.2) +
  geom_errorbarh(aes(xmin = CI_95_lower, xmax = CI_95_upper, color = arm),
                 height = 0.20, linewidth = 0.9, alpha = 0.4) +
  geom_errorbarh(aes(xmin = CI_90_lower, xmax = CI_90_upper, color = arm),
                 height = 0.15, linewidth = 0.7, alpha = 0.7) +
  geom_point(aes(x = emmean, color = arm), size = 3, alpha = 0.8) +
  facet_wrap(~ outcome, nrow = 1, scales = "free_x", strip.position = "bottom") +
  scale_color_manual(values = arm_colors, breaks = lab_top2bottom, guide = "none") +
  scale_y_continuous(breaks = 4:1, labels = lab_top2bottom,
                     expand = expansion(add = c(0.4, 0.4))) +
  scale_x_continuous(labels = label_number(accuracy = 0.1),
                     expand = expansion(mult = c(0.25, 1))) +
  labs(x = NULL, y = NULL) +
  nature_theme +
  theme(legend.position = "none",
        strip.background = element_blank(), strip.placement = "outside",
        strip.text = element_text(family = FONT, size = 9.5, face = "bold",
                                  margin = margin(t = 0, b = 3)),
        panel.spacing = unit(1.2, "lines"),
        axis.text.y = element_text(angle = 90, hjust = 0.5))

p
save_png(file.path(OUT, "neutral_arm_engagement.png"), p,
         width = 3.3 * 1.11, height = 5 * 1.11, dpi = 500)

# ── standalone reply-turns ladder (single panel) ─────────────────────────────
# arm labels run horizontal here: at one-panel width the 90-degree labels used in
# the multi-panel figures overlap each other.
p1 <- p %+% (pd %>% filter(outcome == "log(1+Number of Reply Turns)")) +
  facet_wrap(~ outcome, nrow = 1, scales = "free_x", strip.position = "bottom") +
  theme(axis.text.y = element_text(angle = 0, hjust = 1))
p1
save_png(file.path(OUT, "neutral_arm_engagement_reply_turns.png"), p1,
         width = 3, height = 2.8, dpi = 500)
cat(sprintf("\nSaved neutral_arm_engagement_pairwise.csv, neutral_arm_engagement.png in %s\n", OUT))
cat("Done: neutral_arm_engagement.R\n")
