# ==============================================================================
# plot_active_m2_priming.R
# Priming-effect figure for Active M² — emmeans forest, same style/model as
# plot_active_m2_treatment.R.
#
# Arms (3):
#   Default        — ORIGINAL BASELINE: main-experiment Default arm (told default,
#                    AI = default)  [from active_m2_treatment_data.csv]
#   Primed Averse  — priming experiment, told "risk-averse", AI = default
#   Primed Seeking — priming experiment, told "risk-seeking", AI = default
# The AI CONTENT is identical (default) in all three arms -> any arm difference
# is a pure LABEL/priming effect on realized performance.
#
# Model (pre-registered structure): post_active_m2_ann ~ arm + wave + pre_active_m2_ann
# Inference: HC3 (set SE_METHOD <- "cluster" for session-clustered CR2; few clusters!).
# CAVEAT: baseline comes from the MAIN experiment sample; priming arms from a
# separate sample. Randomization holds within each experiment, not across them —
# interpret Default-vs-primed contrasts as cross-experiment comparisons.
#
# Inputs:  priming_active_m2.csv        (notebook §14 export)
#          active_m2_treatment_data.csv (notebook §13 export)
#   setwd("investment/code"); source("_setup.R"); source("plot_active_m2_priming.R")
# ==============================================================================
suppressPackageStartupMessages({
  library(ggplot2); library(emmeans); library(dplyr); library(sandwich); library(scales); library(clubSandwich)
})

.args <- commandArgs(FALSE); .f <- sub("^--file=", "", .args[grep("^--file=", .args)])
SCRIPT_DIR <- if (length(.f)) dirname(normalizePath(.f)) else getwd()
rd <- function(f) { p <- file.path(DATA_DIR, f); if (!file.exists(p)) p <- file.path(DATA_DIR, f)
                    read.csv(p, check.names = FALSE, stringsAsFactors = FALSE) }

pr <- rd("priming_active_m2.csv")
bl <- rd("active_m2_treatment_data.csv")

# ── assemble the 3-arm dataset ────────────────────────────────────────────────
bl <- bl %>% filter(ai_group == "Default",
                    !is.na(pre_active_m2_ann), !is.na(post_active_m2_ann)) %>%
  transmute(participantId, wave, session_id,
            pre_active_m2_ann, post_active_m2_ann, arm = "Default")
pr <- pr %>% filter(!is.na(pre_active_m2_ann), !is.na(post_active_m2_ann)) %>%
  transmute(participantId, wave, session_id,
            pre_active_m2_ann, post_active_m2_ann,
            arm = ifelse(priming_condition == "risk-averse", "Primed Averse", "Primed Seeking"))
df <- bind_rows(bl, pr)
ARM_LEVELS <- c("Primed Averse", "Default", "Primed Seeking")   # risk gradient, averse -> seeking
df$arm  <- relevel(factor(df$arm, levels = ARM_LEVELS), ref = "Default")
df$wave <- factor(df$wave)
cat(sprintf("N = %d: %s\n", nrow(df),
            paste(names(table(df$arm)), table(df$arm), sep = "=", collapse = ", ")))

# ── model + robust SEs (mirrors plot_active_m2_treatment.R) ───────────────────
m <- lm(post_active_m2_ann ~ arm + wave + pre_active_m2_ann, data = df)

SE_METHOD <- "HC3"          # "cluster" or "HC3"
CLUSTER   <- "session_id"   # used only if SE_METHOD == "cluster"
if (SE_METHOD == "cluster") {
  stopifnot(CLUSTER %in% names(df), !all(is.na(df[[CLUSTER]])))
  n_clusters <- nlevels(factor(df[[CLUSTER]]))
  Vrob <- as.matrix(clubSandwich::vcovCR(m, cluster = df[[CLUSTER]], type = "CR2"))
  if (n_clusters < 15)
    message(sprintf("NOTE: %d clusters on '%s' -> CR2 is fragile with so few clusters.",
                    n_clusters, CLUSTER))
  se_desc <- sprintf("CR2 clustered by %s (%d clusters)", CLUSTER, n_clusters)
} else {
  Vrob <- sandwich::vcovHC(m, type = "HC3")
  se_desc <- "HC3 heteroskedasticity-robust"
}
emm <- emmeans(m, ~ arm, vcov. = Vrob)

cat(sprintf("\n=== emmeans (adjusted mean post Active M2 per arm; %s) ===\n", se_desc))
emm_sum <- summary(emm); print(emm_sum)
cat("\n=== pairwise arm contrasts ===\n"); print(pairs(emm))
cat("\nCAVEAT: Default baseline is from the MAIN experiment; priming arms are a\n")
cat("separate sample -> cross-experiment contrasts, interpret cautiously.\n")

# ── plot data: 90 / 95 / 99% CIs from emmean ± SE * z ─────────────────────────
YORDER <- c("Primed Averse" = 3, "Default" = 2, "Primed Seeking" = 1)

pd <- as.data.frame(emm_sum) %>%
  mutate(
    label       = as.character(arm),
    y_position  = YORDER[label],
    CI_90_lower = emmean - SE * qnorm(0.95),  CI_90_upper = emmean + SE * qnorm(0.95),
    CI_95_lower = emmean - SE * qnorm(0.975), CI_95_upper = emmean + SE * qnorm(0.975),
    CI_99_lower = emmean - SE * qnorm(0.995), CI_99_upper = emmean + SE * qnorm(0.995)
  )
lab_top2bottom <- names(sort(YORDER, decreasing = TRUE))

# primed arms in lighter tints (label-only) of the group colors; Default gray
bias_colors <- c(
  "Primed Averse"  = "#8CD17D",
  "Default"        = "#999999",
  "Primed Seeking" = "#D4A6C8"
)

nature_theme <- theme_classic() +
  theme(
    text = element_text(family = "Avenir", size = 8),
    plot.title = element_text(family = "Avenir", size = 10, face = "bold", hjust = 0),
    axis.title = element_text(family = "Avenir", size = 9, face = "plain"),
    axis.text = element_text(family = "Avenir", size = 8, color = "black"),
    axis.text.y = element_text(family = "Avenir", size = 9, color = "black", face = "plain"),
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
  geom_errorbarh(aes(xmin = CI_99_lower, xmax = CI_99_upper, color = label),
                 height = 0.25, linewidth = 1.2, alpha = 0.3) +
  geom_errorbarh(aes(xmin = CI_95_lower, xmax = CI_95_upper, color = label),
                 height = 0.20, linewidth = 0.9, alpha = 0.5) +
  geom_errorbarh(aes(xmin = CI_90_lower, xmax = CI_90_upper, color = label),
                 height = 0.15, linewidth = 0.7, alpha = 0.8) +
  geom_point(aes(x = emmean, color = label), size = 3, alpha = 0.9) +
  scale_color_manual(values = bias_colors, breaks = lab_top2bottom) +
  guides(color = guide_legend(nrow = 1)) +
  scale_y_continuous(breaks = 3:1, labels = lab_top2bottom,
                     expand = expansion(add = c(0.4, 0.4))) +
  scale_x_continuous(labels = label_number(accuracy = 0.001)) +
  labs(x = expression("Post-interaction Active M"^2), y = NULL) +
  xlim(-0.04, -0.02) +
  nature_theme +
  theme(
    legend.position = "bottom", legend.justification = "center",
    legend.title = element_blank(), legend.text = element_text(size = 9),
    axis.text.y = element_text(angle = 90, hjust = 0.5)
  )
print(p)

# ggsave(file.path(FIG_DIR, "active_m2_priming_effect.pdf"), p, width = 6.5, height = 4.5)
ggsave(file.path(FIG_DIR, "active_m2_priming_effect.png"), p, width = 3.2, height = 4, dpi = 500)
cat(sprintf("\nSaved active_m2_priming_effect.{pdf,png} in %s\n", SCRIPT_DIR))
