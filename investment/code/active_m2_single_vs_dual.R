# ==============================================================================
# active_m2_single_vs_dual.R
# Investment analog of forth_figure_d1.R (political): compare single-AI and
# dual-AI experimental conditions on post-interaction Active M².
#
# ExperimentType (5 levels; political order preserved):
#   Single AI Default    : single-AI Default arm
#   Single AI Biased     : single-AI Som/Ext Averse + Som/Ext Seeking (Neutral excl.)
#   Dual AI Default      : log condition dual_nonbiased (both AIs default)
#   Dual AI Opposition   : log condition dual_opposition (both AIs on the SAME
#                          side of the participant's own risk score)
#   Dual AI Balanced     : log condition dual_balanced (AIs straddle the
#                          participant's own risk score)
# Conditions come from the interaction logs (§15 export), which encode the
# participant-relative design — NOT inferable from the persona pair alone.
#
# Models:
#   Cell-level:       post ~ ExperimentType + wave + pre          (HC3)
#     (an experiment indicator CANNOT enter this model: ExperimentType nests
#      single-vs-dual, so the indicator is perfectly collinear and aliased)
#   Hypothesis-level: post ~ biased_content * is_dual + wave + pre (HC3)
#     one-df planned tests — content (biased vs default), session type
#     (single vs dual AI), and their interaction; is_dual is the single/dual
#     session control, identified because content varies within BOTH experiments.
#
# Model (house convention; political script's NID FE / UID clustering have no
# analog — one observation per participant, wave FE):
#     post_active_m2_ann ~ ExperimentType + wave + pre_active_m2_ann     (HC3)
# CAVEAT: single-AI and dual-AI are separate experiments/samples; randomization
# holds within each. Cross-experiment contrasts: interpret cautiously.
#
# Outputs: emmeans bar figures (vertical + horizontal, value-graded fill) and
# FDR-adjusted pairwise contrasts with Hedges' g (mirrors the political script).
#
# Inputs:  active_m2_treatment_data.csv , dual_active_m2.csv 
#   setwd("investment/code"); source("_setup.R"); source("active_m2_single_vs_dual.R")
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

# ── ExperimentType assembly ───────────────────────────────────────────────────
single <- single %>%
  filter(ai_group %in% c("Default",
                         "Extremely Risk-Averse", "Somewhat Risk-Averse",
                         "Extremely Risk-Seeking", "Somewhat Risk-Seeking"),
         !is.na(pre_active_m2_ann), !is.na(post_active_m2_ann)) %>%
  transmute(participantId, wave, pre_active_m2_ann, post_active_m2_ann,
            ExperimentType = ifelse(ai_group == "Default",
                                    "Single AI Default", "Single AI Biased"))
dual <- dual %>%
  filter(dual_condition %in% c("dual_nonbiased", "dual_balanced", "dual_opposition"),
         !is.na(pre_active_m2_ann), !is.na(post_active_m2_ann)) %>%
  transmute(participantId, wave, pre_active_m2_ann, post_active_m2_ann,
            ExperimentType = recode(dual_condition,
                                    "dual_nonbiased"  = "Dual AI Default",
                                    "dual_opposition" = "Dual AI Opposition",
                                    "dual_balanced"   = "Dual AI Balanced"))
LEVELS <- c("Single AI Default", "Single AI Biased",
            "Dual AI Default", "Dual AI Opposition", "Dual AI Balanced")
df <- bind_rows(single, dual) %>%
  mutate(ExperimentType = factor(ExperimentType, levels = LEVELS),
         wave = factor(wave))
cat("N by condition:\n"); print(table(df$ExperimentType))

# ── cell-level model + HC3 ────────────────────────────────────────────────────
m <- lm(post_active_m2_ann ~ ExperimentType + wave + pre_active_m2_ann, data = df)
Vrob <- vcovHC(m, type = "HC3")
cat("\n=== OLS (HC3): post ~ ExperimentType + wave + pre ===\n")
print(coeftest(m, vcov = Vrob))

# ── hypothesis-level factorial: content x session type (one-df planned tests) ─
# is_dual (single vs dual session) is collinear with ExperimentType above, so
# the session control enters HERE, where content varies within both experiments.
df$biased  <- as.integer(df$ExperimentType != "Single AI Default" &
                         df$ExperimentType != "Dual AI Default")
df$is_dual <- as.integer(grepl("^Dual", df$ExperimentType))
mf1 <- lm(post_active_m2_ann ~ biased + is_dual + wave + pre_active_m2_ann, data = df)
cat("\n=== factorial main effects (HC3): post ~ biased + is_dual + wave + pre ===\n")
ct <- coeftest(mf1, vcov = vcovHC(mf1, type = "HC3"))
print(ct[c("biased", "is_dual"), , drop = FALSE])
mf2 <- lm(post_active_m2_ann ~ biased * is_dual + wave + pre_active_m2_ann, data = df)
cat("\n--- interaction check (does the content effect differ by session type?) ---\n")
ct2 <- coeftest(mf2, vcov = vcovHC(mf2, type = "HC3"))
print(ct2[c("biased", "is_dual", "biased:is_dual"), , drop = FALSE])

emm <- emmeans(m, ~ ExperimentType, vcov. = Vrob)
cat("\n=== adjusted mean post Active M2 per condition ===\n")
plot_data <- as.data.frame(summary(emm))
print(plot_data, digits = 4)

# ── pairwise contrasts, FDR-adjusted, with Hedges' g (political §comparisons) ─
pw <- as.data.frame(summary(pairs(emm, adjust = "fdr"), infer = TRUE))
n_by <- table(df$ExperimentType)
pooled_sd <- sigma(m)
hedges_J <- function(n1, n2) { df0 <- n1 + n2 - 2; ifelse(df0 > 0, 1 - 3 / (4 * df0 - 1), 1) }
parts <- strsplit(as.character(pw$contrast), " - ")
pw$hedges_g <- mapply(function(est, pr) {
  g1 <- gsub("[()]", "", pr[1]); g2 <- gsub("[()]", "", pr[2])
  (est / pooled_sd) * hedges_J(n_by[[g1]], n_by[[g2]])
}, pw$estimate, parts)
pw$sig <- cut(pw$p.value, c(-Inf, .001, .01, .05, .1, Inf),
              labels = c("***", "**", "*", "†", "ns"))
cat("\n=== pairwise contrasts (FDR-adjusted) with Hedges' g ===\n")
print(pw[, c("contrast", "estimate", "SE", "p.value", "hedges_g", "sig")],
      row.names = FALSE, digits = 3)
cat("NOTE: single-AI vs dual-AI contrasts are cross-experiment (separate samples).\n")

# ── exploratory within-dual contrast (UNADJUSTED; annotated on the figure) ────
# Same 5-cell model as the bars. Reported in estimation language and marked with
# a dagger on the plot; the caption MUST state it is unadjusted/exploratory
# (it does not survive FDR across the pairwise family).
cd <- as.data.frame(summary(contrast(emm,
        method = list("Dual Balanced vs Dual Default" = c(0, 0, -1, 0, 1))), infer = TRUE))
cat("\n=== exploratory: Dual Balanced vs Dual Default (unadjusted, same model) ===\n")
print(cd, row.names = FALSE, digits = 3)

# ── SENSITIVITY: dual experiment analyzed alone (3 arms, its own family) ──────
# The REGISTERED correction is FDR -> reported FIRST as the confirmatory result.
# Dunnett many-to-one (the standard error control for a 3-arm design with a
# dedicated control) is a disclosed sensitivity analysis. Neither reaches
# p < .05; Dunnett is marginal — report as "marginal", never "significant".
dd <- df %>% filter(is_dual == 1) %>%
  mutate(cond = droplevels(factor(ExperimentType,
           levels = c("Dual AI Default", "Dual AI Opposition", "Dual AI Balanced"))))
md <- lm(post_active_m2_ann ~ cond + wave + pre_active_m2_ann, data = dd)
ed <- emmeans(md, ~ cond, vcov. = vcovHC(md, type = "HC3"))
cat(sprintf("\n=== dual-only model (n = %d): confirmatory (registered FDR) ===\n", nrow(dd)))
print(summary(pairs(ed, adjust = "fdr")), digits = 3)
cat("\n--- sensitivity: Dunnett many-to-one vs Dual Default ---\n")
print(summary(contrast(ed, "trt.vs.ctrl", ref = "Dual AI Default"), infer = TRUE), digits = 3)

# ── fill: content-type color coding OR value-graded ramp (political style) ────
CONTENT_FILL <- FALSE   # TRUE  = color by AI content (default vs biased; factorial structure)
                       # FALSE = political dark-green value ramp
plot_data <- plot_data %>%
  rename(estimate = emmean, lower_ci = lower.CL, upper_ci = upper.CL) %>%
  mutate(formal_label = factor(gsub(" AI ", " AI\n", as.character(ExperimentType)),
                               levels = gsub(" AI ", " AI\n", LEVELS)))
if (CONTENT_FILL) {
  CCOL <- c(def = "#B5B0AD", bia = "#228B22")   # gray = default content, green = biased content
  plot_data$fill_color <- ifelse(as.character(plot_data$ExperimentType) %in%
                                   c("Single AI Default", "Dual AI Default"),
                                 CCOL["def"], CCOL["bia"])
  fill_scale <- scale_fill_identity(guide = "legend", breaks = unname(CCOL),
                                    labels = c("Default AI content", "Biased AI content"),
                                    name = NULL)
  legend_pos <- "bottom"
} else {
  ramp <- colorRampPalette(c("#BBE5BB", "#66C266", "#228B22", "#006400"))(100)
  nv   <- with(plot_data, (estimate - min(estimate)) / diff(range(estimate)))
  plot_data$fill_color <- ramp[pmax(1, round(nv * 99) + 1)]
  fill_scale <- scale_fill_identity()
  legend_pos <- "none"
}

y_min <- min(plot_data$lower_ci); y_max <- max(plot_data$upper_ci)
pad   <- 0.15 * (y_max - y_min)

base_theme <- theme_classic() +
  theme(
    text = element_text(family = "Avenir", color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.line = element_blank(),
    axis.text.x = element_text(family = "Avenir", size = 9, color = "black", 
                               margin = margin(t = 8), angle = 0),
    axis.text.y = element_text(family = "Avenir", size = 9, color = "black"),
    axis.title.x = element_text(family = "Avenir", size = 12, color = "black", 
                                margin = margin(t = 12)),
    axis.title.y = element_text(family = "Avenir", size = 12, color = "black", 
                                margin = margin(r = 15)),
    axis.ticks = element_line(color = "black", linewidth = 0.4),
    axis.ticks.length = unit(3, "pt"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    plot.margin = margin(t = 15, r = 30, b = 15, l = 15),
    legend.position = "none"
  )

# vertical
p_v <- ggplot(plot_data, aes(x = formal_label, y = estimate)) +
  geom_col(aes(fill = fill_color), color = "black", width = 0.65, linewidth = 0) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.3, linewidth = 0.5) +
  geom_text(aes(y = lower_ci - pad * 1.1, label = sprintf("%.3f", estimate)),
            vjust = 0, family = "Avenir", size = 3) +
  geom_point(color = "black", size = 3, shape = 20) +
  fill_scale +
  scale_y_continuous(labels = number_format(accuracy = 0.001)) +
  coord_cartesian(ylim = c(y_min - pad*5, y_max + pad)) +
  labs(x = "Experimental Condition", y = expression("Post-interaction Active M"^2)) +
  base_theme

print(p_v)

# ggsave(file.path(FIG_DIR, "active_m2_single_vs_dual_vertical.pdf"), p_v,
#        width = 6.5, height = 4.5, device = cairo_pdf)
ggsave(file.path(FIG_DIR, "active_m2_single_vs_dual_vertical.png"), p_v,
       width = 4.5, height = 3, dpi = 500)

# horizontal
p_h <- ggplot(plot_data, aes(y = formal_label, x = estimate)) +
  geom_col(aes(fill = fill_color), color = "black", width = 0.65, linewidth = 0) +
  geom_errorbar(aes(xmin = lower_ci, xmax = upper_ci), width = 0.3, linewidth = 0.5) +
  geom_text(aes(x = lower_ci - pad * 0.35, label = sprintf("%.3f", estimate)),
            hjust = 0, family = "Avenir", size = 3) +
  geom_point(color = "black", size = 3, shape = 20) +
  fill_scale +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(labels = number_format(accuracy = 0.001)) +
  coord_cartesian(xlim = c(y_min - pad, y_max + pad)) +
  labs(y = NULL, x = expression("Post-interaction Active M"^2)) +
  base_theme +
  theme(legend.position = legend_pos)
print(p_h)
ggsave(file.path(FIG_DIR, "active_m2_single_vs_dual_horizontal.pdf"), p_h,
       width = 6.5, height = 4.0, device = cairo_pdf)
ggsave(file.path(FIG_DIR, "active_m2_single_vs_dual_horizontal.png"), p_h,
       width = 6.5, height = 4.0, dpi = 300)
cat(sprintf("\nSaved active_m2_single_vs_dual_{vertical,horizontal}.{pdf,png} in %s\n", SCRIPT_DIR))

