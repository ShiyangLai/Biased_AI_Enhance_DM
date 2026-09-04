# ==============================================================================
# reply_turns_single_vs_dual.R
# Investment analog of forth_figure_d4.R (political): number of participant
# REPLY TURNS as the outcome, across the 5 experimental conditions.
#   Single AI Default / Single AI Biased / Dual AI Default / Dual AI Opposition
#   / Dual AI Balanced   (conditions assembled exactly as active_m2_single_vs_dual.R)
#
# Outcome PLOTTED: follow-up participation rate = P(>=1 reply turn). The turn
# COUNT (pre-registered) is retained and printed to the console, because the
# condition differences load on the extensive margin (who engages at all) rather
# than on volume -- same reason panel 2 of bias_magnitude_outcomes.R plots
# participation. Turn counts come from n_reply_turns (canned opener excluded; §17).
# Model (pre-registered structure; the political script's NID FE / UID clustering
# have no analog — one observation per participant): outcome ~ ExperimentType
# + wave, HC3 (linear probability model for the rate, so the CI construction
# matches the count model; logit cross-check printed). emmeans adjusted means ->
# horizontal bar (value-ramp, d4 style) + FDR pairwise contrasts with Hedges' g.
#
# CAVEAT: single-AI vs dual-AI contrasts are cross-experiment (separate samples);
# 9 participants appear in both, so the reply-turns join is keyed on
# (participantId, experiment).
# Inputs: reply_turns.csv (§17), active_m2_treatment_data.csv ,
#         dual_active_m2.csv 
#   setwd("investment/code"); source("_setup.R"); source("reply_turns_single_vs_dual.R")
# ==============================================================================
suppressPackageStartupMessages({
  library(ggplot2); library(emmeans); library(dplyr); library(sandwich)
  library(lmtest); library(scales)
})

.args <- commandArgs(FALSE); .f <- sub("^--file=", "", .args[grep("^--file=", .args)])
SCRIPT_DIR <- if (length(.f)) dirname(normalizePath(.f)) else getwd()
rd <- function(f) { p <- file.path(DATA_DIR, f); if (!file.exists(p)) p <- file.path(DATA_DIR, f)
                    read.csv(p, check.names = FALSE, stringsAsFactors = FALSE) }

single <- rd("active_m2_treatment_data.csv")
dual   <- rd("dual_active_m2.csv")
rt     <- rd("reply_turns.csv")

s1 <- single %>%
  filter(ai_group %in% c("Default", "Extremely Risk-Averse", "Somewhat Risk-Averse",
                         "Extremely Risk-Seeking", "Somewhat Risk-Seeking")) %>%
  transmute(participantId, wave, experiment = "single",
            ExperimentType = ifelse(ai_group == "Default",
                                    "Single AI Default", "Single AI Biased"))
d1 <- dual %>%
  filter(dual_condition %in% c("dual_nonbiased", "dual_balanced", "dual_opposition")) %>%
  transmute(participantId, wave, experiment = "dual",
            ExperimentType = recode(dual_condition,
                                    "dual_nonbiased"  = "Dual AI Default",
                                    "dual_opposition" = "Dual AI Opposition",
                                    "dual_balanced"   = "Dual AI Balanced"))
LEVELS <- c("Single AI Default", "Single AI Biased",
            "Dual AI Default", "Dual AI Opposition", "Dual AI Balanced")
df <- bind_rows(s1, d1) %>%
  left_join(rt, by = c("participantId", "experiment")) %>%
  filter(!is.na(n_reply_turns)) %>%
  mutate(ExperimentType = factor(ExperimentType, levels = LEVELS), wave = factor(wave),
         anyfu = as.integer(n_reply_turns > 0))
cat("N by condition:\n"); print(table(df$ExperimentType))
cat("\nreply turns and follow-up participation by condition:\n")
print(df %>% group_by(ExperimentType) %>%
        summarise(n = n(), mean_turns = mean(n_reply_turns), sd = sd(n_reply_turns),
                  participation = mean(anyfu), .groups = "drop") %>%
        as.data.frame(), digits = 3, row.names = FALSE)

# ── pre-registered TURN-COUNT model (console only) ────────────────────────────
mc <- lm(n_reply_turns ~ ExperimentType + wave, data = df)
cat("\n=== [count, pre-registered] OLS (HC3): n_reply_turns ~ ExperimentType + wave ===\n")
print(coeftest(mc, vcov = vcovHC(mc, type = "HC3")))
cat("\n--- adjusted mean reply turns ---\n")
print(as.data.frame(summary(emmeans(mc, ~ ExperimentType,
      vcov. = vcovHC(mc, type = "HC3")))), digits = 4, row.names = FALSE)

# ── PLOTTED model: follow-up participation rate (LPM + HC3) ───────────────────
m <- lm(anyfu ~ ExperimentType + wave, data = df)
Vrob <- vcovHC(m, type = "HC3")
cat("\n=== OLS/LPM (HC3): follow-up participation ~ ExperimentType + wave ===\n")
print(coeftest(m, vcov = Vrob))
gl <- glm(anyfu ~ ExperimentType + wave, family = binomial(), data = df)
cat("\n--- logit cross-check (OR vs Single AI Default, HC3) ---\n")
ccg <- coeftest(gl, vcov = vcovHC(gl, type = "HC3"))
rg <- grep("^ExperimentType", rownames(ccg))
print(data.frame(term = sub("^ExperimentType", "", rownames(ccg)[rg]),
                 OR = exp(ccg[rg, 1]), lo = exp(ccg[rg, 1] - 1.96 * ccg[rg, 2]),
                 hi = exp(ccg[rg, 1] + 1.96 * ccg[rg, 2]), p = ccg[rg, 4]),
      row.names = FALSE, digits = 3)
emm <- emmeans(m, ~ ExperimentType, vcov. = Vrob)
cat("\n=== adjusted follow-up participation rate per condition ===\n")
plot_data <- as.data.frame(summary(emm)); print(plot_data, digits = 4)

# ── pairwise, FDR, Hedges' g (mirrors d4) ─────────────────────────────────────
pw <- as.data.frame(summary(pairs(emm, adjust = "fdr"), infer = TRUE))
n_by <- table(df$ExperimentType); pooled_sd <- sigma(m)
hedges_J <- function(n1, n2) { d0 <- n1 + n2 - 2; ifelse(d0 > 0, 1 - 3 / (4 * d0 - 1), 1) }
parts <- strsplit(as.character(pw$contrast), " - ")
pw$hedges_g <- mapply(function(est, pr) {
  g1 <- gsub("[()]", "", pr[1]); g2 <- gsub("[()]", "", pr[2])
  (est / pooled_sd) * hedges_J(n_by[[g1]], n_by[[g2]])
}, pw$estimate, parts)
pw$sig <- cut(pw$p.value, c(-Inf, .001, .01, .05, .1, Inf),
              labels = c("***", "**", "*", "†", "ns"))
cat("\n=== pairwise contrasts (FDR) with Hedges' g ===\n")
print(pw[, c("contrast", "estimate", "SE", "p.value", "hedges_g", "sig")],
      row.names = FALSE, digits = 3)
cat("NOTE: single-AI vs dual-AI contrasts are cross-experiment (separate samples).\n")

# ── figure: horizontal bar, brown value-ramp (d4 style) ───────────────────────
plot_data <- plot_data %>%
  rename(estimate = emmean, lower_ci = lower.CL, upper_ci = upper.CL) %>%
  mutate(formal_label = factor(gsub(" AI ", " AI\n", as.character(ExperimentType)),
                               levels = rev(gsub(" AI ", " AI\n", LEVELS))))
# value ramp: goldenrod family, matching the follow-up-participation panel of
# bias_magnitude_outcomes.R (swap back to
# c("#D6C0E5","#B88BD8","#9370DB","#4B0082") for the original purple ramp)
bramp <- colorRampPalette(c("#EBDCA8", "#D4B23F", "#B8860B", "#6B4E06"))(100)
nv <- with(plot_data, (estimate - min(estimate)) / diff(range(estimate)))
plot_data$fill_color <- bramp[pmax(1, round(nv * 99) + 1)]
x_max <- 1                       # participation is a proportion
padx  <- 0.02

p <- ggplot(plot_data, aes(y = formal_label, x = estimate)) +
  geom_col(aes(fill = fill_color), color = "black", width = 0.7, linewidth = 0) +
  geom_errorbar(aes(xmin = lower_ci, xmax = upper_ci),
                width = 0.3, linewidth = 0.5, color = "black") +
  geom_text(aes(x = upper_ci + padx, label = sprintf("%.2f", estimate)),
            hjust = 0, family = "Avenir", size = 3, color = "black") +
  geom_point(color = "black", size = 2.4, shape = 20) +
  scale_fill_identity() +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 1, 0.25),
                     labels = number_format(accuracy = 0.01)) +
  coord_cartesian(xlim = c(0, x_max)) +
  labs(y = "Investment Experimental Condition", x = "Follow-up Participation Rate") +
  theme_classic() +
  theme(
    text = element_text(family = "Avenir", color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.line = element_blank(),
    axis.text.x = element_text(family = "Avenir", size = 9, color = "black"),
    axis.text.y = element_text(family = "Avenir", size = 9, color = "black", angle = 90, hjust = 0.5),
    axis.title.x = element_text(family = "Avenir", size = 12, color = "black",
                                margin = margin(t = 12)),
    axis.title.y = element_text(family = "Avenir", size = 12, color = "black",
                                margin = margin(r = 12)),
    axis.ticks = element_line(color = "black", linewidth = 0.4),
    axis.ticks.length = unit(3, "pt"),
    panel.grid = element_blank(),
    plot.margin = margin(t = 15, r = 40, b = 15, l = 15),
    legend.position = "none"
  )
print(p)
ggsave(file.path(FIG_DIR, "followup_participation_single_vs_dual.png"), p,
       width = 4, height = 4.5, dpi = 500)
cat(sprintf("\nSaved followup_participation_single_vs_dual.png in %s\n", SCRIPT_DIR))
