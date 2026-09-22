# ==============================================================================
# final_project_outcomes.R — two standalone figures for the final-project (SI)
# dose analysis: outcome vs the randomized number of biased weeks assigned.
#
#   figures/final_essay_vs_dose.png   human essay grade   (0-10)
#   figures/final_innov_vs_dose.png   LLM innovativeness  (mean of GPT-5.4 and
#                                    Gemini 3.1 Pro essay ratings)
#
# Unit = PROJECT (team projects are graded once, so the student row duplicates a
# shared grade; the project is where the outcome was actually assigned). The
# x variable is the team-mean of n_biased_assigned = got_nov + got_rel, i.e. the
# count of directionally-biased weeks (neutral and questioning are not biased).
# Fit is OLS with a 95% CI band; point area is team size. House style: Avenir,
# theme_classic + black panel frame, no legend, dpi 500.
#
# Inputs : data/final_project_student.csv, data/final_project_docs.csv
#   setwd("education/code"); source("_setup.R"); source("final_project_outcomes.R")
# ==============================================================================
suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(readr); library(sandwich); library(lmtest)
})

IN <- DATA_DIR; OUT <- FIG_DIR

p <- read_csv(file.path(IN, "final_project_student.csv"), show_col_types = FALSE) %>%
  filter(!is.na(doc_code)) %>%
  group_by(doc_code) %>%
  summarise(essay = first(essay), dose = mean(n_biased_assigned), team = n(), .groups = "drop") %>%
  left_join(read_csv(file.path(IN, "final_project_docs.csv"), show_col_types = FALSE) %>%
              dplyr::select(doc_code, innov_mean), by = "doc_code")

nature_theme <- theme_classic() +
  theme(
    text = element_text(family = FONT, size = 8, color = "black"),
    axis.title = element_text(family = FONT, size = 9),
    axis.text = element_text(family = FONT, size = 8, color = "black"),
    axis.line = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.4),
    axis.ticks.length = unit(2.5, "pt"),
    panel.grid = element_blank(),
    plot.margin = margin(t = 12, r = 14, b = 8, l = 8),
    legend.position = "none"
  )

# One panel: the HUBER robust fit with a 95% band from its robust vcov (no raw
# points -- the predictor is clumped at dose = 1 and the scatter obscures the fit).
# Inference reported alongside is Huber + exact randomization inference on the
# randomized dose; HC3 is printed only for reference.
dose_panel <- function(ycol, ylab, col, acc = 0.5) {
  d <- p[!is.na(p[[ycol]]), ]; d$.y <- d[[ycol]]
  h  <- MASS::rlm(.y ~ dose, data = d); ch <- coeftest(h)
  o  <- lm(.y ~ dose, data = d)
  co <- coeftest(o, vcov = vcovHC(o, type = "HC3"))
  set.seed(1)
  pm <- replicate(20000, { x <- d; x$dose <- sample(x$dose)
                           coef(lm(.y ~ dose, data = x))["dose"] })
  p_rand <- mean(abs(pm) >= abs(coef(o)["dose"]))
  dfres <- nrow(d) - length(coef(h))
  cat(sprintf("%-11s n=%2d | Huber b=%+.3f (SE %.3f, p=%.3f) | randomization p=%.3f | [HC3 p=%.3f]\n",
              ycol, nrow(d), ch["dose", 1], ch["dose", 2],
              2 * pt(abs(ch["dose", 3]), dfres, lower.tail = FALSE), p_rand, co["dose", 4]))
  # 95% band around the Huber fit from its robust variance-covariance
  V <- vcov(h); nd <- data.frame(dose = seq(0, 2, length.out = 100))
  X <- cbind(1, nd$dose)
  nd$fit <- as.vector(X %*% coef(h))
  nd$se  <- sqrt(rowSums((X %*% V) * X))
  tcrit  <- qt(0.975, dfres)
  nd$lwr <- nd$fit - tcrit * nd$se; nd$upr <- nd$fit + tcrit * nd$se
  ggplot(nd, aes(x = dose)) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), fill = col, alpha = 0.18) +
    geom_line(aes(y = fit), color = col, linewidth = 1.0) +
    scale_x_continuous(name = "Number of Biased Weeks Assigned",
                       breaks = c(0, 0.5, 1, 1.5, 2), limits = c(-0.05, 2.05),
                       expand = c(0, 0)) +
    scale_y_continuous(name = ylab, labels = scales::number_format(accuracy = acc)) +
    nature_theme
}

cat("=== project-level dose regressions (Huber robust + randomization inference) ===\n")
# BrBG house palette: brown for the graded-performance outcome, teal for the
# novelty-flavoured one, matching the arm colours used throughout the in-class figures
p1 <- dose_panel("essay",      "Final Project Grade",  "#8C510A", acc = 0.5)
p2 <- dose_panel("innov_mean", "LLM-annotated Final Project Innovativeness", "#01665E", acc = 0.5)

p2
save_png(file.path(OUT, "final_essay_vs_dose.png"), p1, width = 5, height = 4)
save_png(file.path(OUT, "final_innov_vs_dose.png"), p2, width = 5, height = 4)
cat(sprintf("\nSaved final_essay_vs_dose.png and final_innov_vs_dose.png in %s\n", OUT))
cat("Done: final_project_outcomes.R\n")

# ==============================================================================
# Arm-effect ladder: innovativeness by ASSIGNED ARM, as CONTRASTS not levels.
#
# Why contrasts: each student received exactly two of {novelty, reliability,
# neutralized, questioning}, and Default was the within-student baseline (the
# untreated weeks), not an assigned condition. With questioning dropped from the
# analysis, NO student has zero studied arms, so there is no observable baseline
# level to anchor a ladder of means. The model is additive in arm exposure and the
# plotted quantity is each arm's effect relative to that common reference, with a
# line at zero. Nested 90/95/99% intervals from the Huber robust vcov.
# ==============================================================================
pa <- read_csv(file.path(IN, "final_project_student.csv"), show_col_types = FALSE) %>%
  filter(!is.na(doc_code)) %>% group_by(doc_code) %>%
  summarise(nov = mean(got_nov), rel = mean(got_rel), neu = mean(got_neu), .groups = "drop") %>%
  left_join(read_csv(file.path(IN, "final_project_docs.csv"), show_col_types = FALSE) %>%
              dplyr::select(doc_code, innov_mean), by = "doc_code") %>%
  filter(!is.na(innov_mean))

ma <- MASS::rlm(innov_mean ~ nov + rel + neu, data = pa)
V <- vcov(ma); bb <- coef(ma); dfr <- nrow(ma$model) - length(bb)
ARM_ORDER <- c("Novelty", "Reliability", "Neutralized")   # top -> bottom
LAD <- data.frame(term = c("nov", "rel", "neu"),
                  arm  = c("Novelty", "Reliability", "Neutralized")) %>%
  rowwise() %>%
  mutate(est = bb[term], SE = sqrt(V[term, term])) %>% ungroup() %>%
  # ARM_ORDER is top -> bottom on the plot; y is reversed so the FIRST level sits
  # highest. Labels are taken from LAD itself below, so the two can never desync.
  mutate(arm = factor(arm, levels = ARM_ORDER),
         y = length(ARM_ORDER) - as.integer(arm) + 1,
         lo90 = est - qt(.95, dfr) * SE,  hi90 = est + qt(.95, dfr) * SE,
         lo95 = est - qt(.975, dfr) * SE, hi95 = est + qt(.975, dfr) * SE,
         lo99 = est - qt(.995, dfr) * SE, hi99 = est + qt(.995, dfr) * SE,
         p = 2 * pt(abs(est / SE), dfr, lower.tail = FALSE))
ARMCOL <- c(Novelty = "#01665E", Reliability = "#8C510A", Neutralized = "#79706E")
cat("\n=== arm effects on innovativeness (Huber; common reference) ===\n")
print(LAD %>% mutate(across(where(is.numeric), ~round(., 3))) %>%
        dplyr::select(arm, est, SE, lo95, hi95, p), row.names = FALSE)

pl <- ggplot(LAD, aes(y = y)) +
  geom_vline(xintercept = 0, color = "grey55", linetype = "dashed", linewidth = 0.45) +
  geom_errorbarh(aes(xmin = lo99, xmax = hi99, color = arm), height = .25, linewidth = 1.2, alpha = .2) +
  geom_errorbarh(aes(xmin = lo95, xmax = hi95, color = arm), height = .20, linewidth = .9, alpha = .4) +
  geom_errorbarh(aes(xmin = lo90, xmax = hi90, color = arm), height = .15, linewidth = .7, alpha = .7) +
  geom_point(aes(x = est, color = arm), size = 3, alpha = .85) +
  scale_color_manual(values = ARMCOL, guide = "none") +
  scale_y_continuous(breaks = LAD$y, labels = as.character(LAD$arm),
                     expand = expansion(add = c(.5, .5))) +
  scale_x_continuous(name = "Effect on Final Project Innovativeness",
                     labels = scales::number_format(accuracy = 0.5),
                     expand = expansion(add = c(1., 2))) +
  labs(y = NULL) + nature_theme
pl

save_png(file.path(OUT, "final_innov_by_arm.png"), pl, width = 3.2, height = 2.4)
cat(sprintf("Saved final_innov_by_arm.png in %s\n", OUT))
