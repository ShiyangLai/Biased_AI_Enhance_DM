# ==============================================================================
# plot_active_m2_treatment.R
# Pre-registered treatment-effect figure for Active M² (Single AI), emmeans forest.
#
# Pre-registered model:  post_active_m2_ann ~ ai_group + wave + pre_active_m2_ann
#   - ai_group : treatment arm (Risk-Neutral EXCLUDED -> 5 arms; Default = reference)
#   - wave     : wave fixed effects
#   - pre_active_m2_ann : baseline (initial-portfolio Active M²)
# Inference: session-clustered CR2 (clubSandwich) by default; set SE_METHOD <- "HC3" to switch back.
# Plotted: emmeans-adjusted mean post Active M² per arm, with 90/95/99% CIs. No title, no zero line.
#
# Run from this folder (so it finds the CSV + preprocess script):
#   Rscript plot_active_m2_treatment.R
# ==============================================================================
suppressPackageStartupMessages({
  library(ggplot2); library(emmeans); library(dplyr); library(sandwich); library(scales); library(clubSandwich)
})
# Resolve paths relative to THIS script so it runs from any working directory.
.args <- commandArgs(FALSE)
.f <- sub("^--file=", "", .args[grep("^--file=", .args)])
SCRIPT_DIR <- if (length(.f)) dirname(normalizePath(.f)) else getwd()
PRE <- "preprocess_active_m2.R"                              # as set (project-root relative)
if (!file.exists(PRE)) PRE <- file.path(SCRIPT_DIR, "preprocess_active_m2.R")  # fallback: script dir
source(PRE)

DATA <- "active_m2_treatment_data.csv"                       # as set (project-root relative)
if (!file.exists(DATA)) DATA <- file.path(DATA_DIR, "active_m2_treatment_data.csv")  # fallback: script dir
df <- load_active_m2(DATA)

# ── Pre-registered model + cluster-robust (CR2) emmeans over arms ────────────
m   <- lm(post_active_m2_ann ~ ai_group + wave + pre_active_m2_ann, data = df)

# ── Robust SEs ───────────────────────────────────────────────────────────────
# Default: clubSandwich CR2 clustered by session_id.
# Set SE_METHOD <- "HC3" to switch back to obs-level heteroskedasticity-robust SEs.
# CAVEAT: only ~5 sessions (nested in wave) -> very few clusters; CR2 SEs here are FRAGILE
# (the warning below fires). Use CLUSTER <- "eval_start" if you want more clusters.
SE_METHOD <- "HC3"      # "cluster" or "HC3"
CLUSTER   <- "session_id"   # used only if SE_METHOD == "cluster"

if (SE_METHOD == "cluster") {
  stopifnot(CLUSTER %in% names(df), !all(is.na(df[[CLUSTER]])))
  n_clusters <- nlevels(factor(df[[CLUSTER]]))
  Vrob <- as.matrix(clubSandwich::vcovCR(m, cluster = df[[CLUSTER]], type = "CR2"))
  if (n_clusters < 15)
    message(sprintf("NOTE: %d clusters on '%s' -> CR2 is fragile/unreliable with so few clusters.",
                    n_clusters, CLUSTER))
  se_desc <- sprintf("CR2 clustered by %s (%d clusters)", CLUSTER, n_clusters)
} else {
  Vrob <- sandwich::vcovHC(m, type = "HC3")
  se_desc <- "HC3 heteroskedasticity-robust"
}
emm <- emmeans(m, ~ ai_group, vcov. = Vrob)

cat(sprintf("\n=== emmeans (adjusted mean post Active M2 per arm; %s) ===\n", se_desc))
emm_sum <- summary(emm); print(emm_sum)
cat("\n=== pairwise arm contrasts ===\n"); print(pairs(emm))

# ── Plot data: 90 / 95 / 99% CIs from emmean ± SE * z ────────────────────────
YORDER <- c("Extremely Risk-Averse" = 5, "Somewhat Risk-Averse" = 4,
            "Default" = 3, "Somewhat Risk-Seeking" = 2, "Extremely Risk-Seeking" = 1)
SHORT  <- c("Extremely Risk-Averse" = "E-Averse", "Somewhat Risk-Averse" = "S-Averse",
            "Default" = "Default",
            "Somewhat Risk-Seeking" = "S-Seeking", "Extremely Risk-Seeking" = "E-Seeking")

pd <- as.data.frame(emm_sum) %>%
  mutate(
    arm         = as.character(ai_group),
    y_position  = YORDER[arm],
    label       = SHORT[arm],
    CI_90_lower = emmean - SE * qnorm(0.95),  CI_90_upper = emmean + SE * qnorm(0.95),
    CI_95_lower = emmean - SE * qnorm(0.975), CI_95_upper = emmean + SE * qnorm(0.975),
    CI_99_lower = emmean - SE * qnorm(0.995), CI_99_upper = emmean + SE * qnorm(0.995)
  )

# top-to-bottom label order (y = 6..1) for axis + legend
lab_top2bottom <- unname(SHORT[names(sort(YORDER, decreasing = TRUE))])

# Diverging red (averse) -> gray (Default) -> blue (seeking)
bias_colors <- c("E-Averse"="#1B7837","S-Averse"="#7FBF7B","Neutral"="#BABABA",
                 "Default"="#999999","S-Seeking"="#AF8DC3","E-Seeking"="#762A83")

# Nature-ish theme (drop family="Avenir" unless installed; add it back if you have it)
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
  guides(color = guide_legend(nrow = 2)) +
  scale_y_continuous(breaks = 5:1, labels = lab_top2bottom,
                     expand = expansion(add = c(0.4, 0.4))) +
  scale_x_continuous(labels = label_number(accuracy = 0.001)) +   # auto range (NOT hardcoded)
  labs(
    x = expression("Post-interaction Active M"^2), y = NULL
  ) +
  nature_theme +
  xlim(-0.04, -0.0) +
  theme(
    legend.position = "bottom", legend.justification = "center",
    legend.title = element_blank(), legend.text = element_text(size = 9),
    axis.text.y = element_text(angle = 90, hjust = 0.5)   # horizontal labels (avoid overlap with 6 arms)
  )
print(p)

# ggsave(file.path(FIG_DIR, "active_m2_treatment_effect.pdf"), p, width = 6.5, height = 4.5)
ggsave(file.path(FIG_DIR, "active_m2_treatment_effect.png"), p, width = 5, height = 4.95, dpi = 500)
cat(sprintf("\nSaved active_m2_treatment_effect.{pdf,png} in %s\n", SCRIPT_DIR))


# ==============================================================================
# PER-WAVE version — same model & style, estimated separately WITHIN each wave,
# shown as three horizontally-aligned panels.
# ==============================================================================
# Within a single wave the wave FE is constant, so the model drops it:
#     post_active_m2_ann ~ ai_group + pre_active_m2_ann
# SEs: HC3. (Session clustering is infeasible within a wave — waves 2 & 3 have a
# single session each -> 1 cluster; HC3 is the robust SE that works in every wave.)

emm_by_arm_hc3 <- function(data) {
  data <- droplevels(data)
  mw <- lm(post_active_m2_ann ~ ai_group + pre_active_m2_ann, data = data)
  Vw <- sandwich::vcovHC(mw, type = "HC3")
  as.data.frame(summary(emmeans(mw, ~ ai_group, vcov. = Vw)))
}

waves <- sort(unique(as.character(df$wave)))
pd_wave <- do.call(rbind, lapply(waves, function(w) {
  es <- emm_by_arm_hc3(df[df$wave == w, ]); es$wave <- w; es
}))
pd_wave <- pd_wave %>%
  mutate(
    arm         = as.character(ai_group),
    y_position  = YORDER[arm],
    label       = SHORT[arm],
    CI_90_lower = emmean - SE * qnorm(0.95),  CI_90_upper = emmean + SE * qnorm(0.95),
    CI_95_lower = emmean - SE * qnorm(0.975), CI_95_upper = emmean + SE * qnorm(0.975),
    CI_99_lower = emmean - SE * qnorm(0.995), CI_99_upper = emmean + SE * qnorm(0.995)
  )

cat(sprintf("\n=== per-wave emmeans (HC3): %s ===\n", paste(waves, collapse = ", ")))
print(pd_wave[, c("wave", "arm", "emmean", "SE")], row.names = FALSE)

# ── per-wave arm-vs-Default contrasts (HC3), with multiplicity handling ───────
# p_unadj    : per-contrast, no adjustment (exploratory read)
# p_dunnett  : many-to-one adjustment within each wave's 4-contrast family
# p_fdr_all  : BH-FDR across all waves x contrasts (here 12 tests)
cat("\n=== per-wave contrasts vs Default (HC3) ===\n")
hedges_J <- function(n1, n2) { d0 <- n1 + n2 - 2; ifelse(d0 > 0, 1 - 3 / (4 * d0 - 1), 1) }
ct_wave <- do.call(rbind, lapply(waves, function(w) {
  dw <- droplevels(df[df$wave == w, ])
  mw <- lm(post_active_m2_ann ~ ai_group + pre_active_m2_ann, data = dw)
  ew <- emmeans(mw, ~ ai_group, vcov. = sandwich::vcovHC(mw, type = "HC3"))
  cn <- contrast(ew, method = "trt.vs.ctrl", ref = "Default")
  out <- as.data.frame(summary(cn, adjust = "none", infer = c(TRUE, TRUE)))
  out$p_dunnett <- as.data.frame(summary(cn, adjust = "dunnettx"))$p.value
  nb <- table(dw$ai_group)
  out$hedges_g <- sapply(seq_len(nrow(out)), function(i) {
    gg <- strsplit(gsub("[()]", "", as.character(out$contrast[i])), " - ")[[1]]
    (out$estimate[i] / sigma(mw)) * hedges_J(nb[[gg[1]]], nb[[gg[2]]]) })
  out$wave <- w
  out
}))
names(ct_wave)[names(ct_wave) == "p.value"] <- "p_unadj"
ct_wave$p_fdr_all <- p.adjust(ct_wave$p_unadj, "fdr")
print(ct_wave[, c("wave", "contrast", "estimate", "lower.CL", "upper.CL", "hedges_g",
                  "p_unadj", "p_dunnett", "p_fdr_all")],
      row.names = FALSE, digits = 3)
cat("(lower/upper.CL = unadjusted 95% CI; hedges_g = estimate / residual SD x J)\n")

p_wave <- ggplot(pd_wave, aes(y = y_position)) +
  geom_errorbarh(aes(xmin = CI_99_lower, xmax = CI_99_upper, color = label),
                 height = 0.25, linewidth = 1.2, alpha = 0.3) +
  geom_errorbarh(aes(xmin = CI_95_lower, xmax = CI_95_upper, color = label),
                 height = 0.20, linewidth = 0.9, alpha = 0.5) +
  geom_errorbarh(aes(xmin = CI_90_lower, xmax = CI_90_upper, color = label),
                 height = 0.15, linewidth = 0.7, alpha = 0.8) +
  geom_point(aes(x = emmean, color = label), size = 3, alpha = 0.9) +
  facet_wrap(~ wave, nrow = 1,
             labeller = as_labeller(function(x) gsub("wave", "Wave ", x))) +
  scale_color_manual(values = bias_colors, breaks = lab_top2bottom) +
  guides(color = guide_legend(nrow = 1)) +
  scale_y_continuous(breaks = 5:1, labels = lab_top2bottom,
                     expand = expansion(add = c(0.4, 0.4))) +
  scale_x_continuous(labels = label_number(accuracy = 0.001)) +
  labs(x = expression("Post-interaction Active M"^2), y = NULL) +
  nature_theme +
  xlim(-0.06, 0.06) +
  theme(
    legend.position  = "bottom", legend.justification = "center",
    legend.title     = element_blank(), legend.text = element_text(size = 9),
    # axis.text.y      = element_text(angle = 0, hjust = 1),
    strip.background = element_blank(),
    strip.text = element_blank(),
    panel.spacing    = unit(1.2, "lines"),
    axis.text.y = element_text(angle = 90, hjust = 0.5)
  )
print(p_wave)


# ggsave(file.path(FIG_DIR, "active_m2_treatment_effect_by_wave.pdf"), p_wave, width = 12, height = 4.5)
ggsave(file.path(FIG_DIR, "active_m2_treatment_effect_by_wave.png"), p_wave, width = 5, height = 4.95, dpi = 500)
cat(sprintf("\nSaved active_m2_treatment_effect_by_wave.{pdf,png} in %s\n", SCRIPT_DIR))
