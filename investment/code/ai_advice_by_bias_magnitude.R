# ==============================================================================
# ai_advice_by_bias_magnitude.R
# Investment analog of the AI-correctness panel in the fact-checking study's
# second_figure_b2.R: how good is the ASSISTANT'S OWN recommended portfolio,
# by magnitude of risk bias?
#
#   fact-checking            ->  investment
#   AICorrectness            ->  ai_m2, the Active M2 of the assistant's own
#                                recommended portfolio, scored on the SAME
#                                14-day window as the participant's outcome
#   BiasedCat (No/Mod/Strong)->  No Bias (Default) / Moderate (Somewhat Averse +
#                                Somewhat Seeking) / Strong (Extremely Averse +
#                                Extremely Seeking); Risk-Neutral excluded
#   as.factor(NID) FE        ->  wave FE
#   UID-clustered SEs        ->  HC3 (one observation per participant)
#
# Recommended portfolios were extracted from the interaction transcripts with
# GPT-4o (ai_portfolio_extraction.py) and exported as ai_only_by_wave.csv.
#
# CAVEAT: magnitude pools the two BIAS DIRECTIONS, whose advice quality moves in
# opposite directions in these (defensive) windows, so the pooled bars can
# cancel. The direction-split version is printed to console and saved as
# ai_advice_by_bias_magnitude_split.png.
#
# Inputs:  ai_only_by_wave.csv, daily_returns.csv, active_m2_treatment_data.csv
# Outputs: ai_advice_by_bias_magnitude.png (+ _split.png)
#   setwd("investment/code"); source("_setup.R"); source("ai_advice_by_bias_magnitude.R")
# ==============================================================================
suppressPackageStartupMessages({
  library(ggplot2); library(emmeans); library(dplyr); library(sandwich)
  library(lmtest); library(scales)
})

.args <- commandArgs(FALSE); .f <- sub("^--file=", "", .args[grep("^--file=", .args)])
SCRIPT_DIR <- if (length(.f)) dirname(normalizePath(.f)) else getwd()
rd <- function(f) { p <- file.path(DATA_DIR, f); if (!file.exists(p)) p <- file.path(DATA_DIR, f)
                    read.csv(p, check.names = FALSE, stringsAsFactors = FALSE) }

ASSETS <- c("SHY","IEF","LQD","GLD","VNQ","SPY","XLF","XLE","XLI","XLP","XLU",
            "AAPL","MSFT","AMZN","GOOGL","NVDA","TSLA","SHOP","SNOW","PLTR","DKNG",
            "RIVN","CRSP","BTC","ETH")
AWC <- paste0("ai_w_", ASSETS)
hedges_J <- function(n1, n2) { d0 <- n1 + n2 - 2; ifelse(d0 > 0, 1 - 3 / (4 * d0 - 1), 1) }

# ── assistant's own recommended portfolio, scored on the participant's window ──
ret <- rd("daily_returns.csv"); ret$Date <- as.Date(ret$Date)
ai  <- rd("ai_only_by_wave.csv"); ai$eval_start <- as.Date(ai$eval_start)
active_m2 <- function(w, R) {
  s <- sum(w, na.rm = TRUE); if (!is.finite(s) || s <= 0) return(NA_real_)
  w <- ifelse(is.na(w), 0, w) / s
  rp <- as.numeric(R %*% w); rb <- R[, "SPY"]; a <- rp - rb
  sa <- sd(a); sm <- sd(rb)
  if (is.na(sa) || sa == 0 || sm == 0) return(NA_real_)
  (mean(a) / sa) * sm * sqrt(252) }
ai$ai_m2 <- NA_real_
for (i in seq_len(nrow(ai))) {
  Ri <- as.matrix(ret[ret$Date >= ai$eval_start[i] &
                      ret$Date <= (ai$eval_start[i] + 14), ASSETS, drop = FALSE])
  if (nrow(Ri) >= 3) ai$ai_m2[i] <- active_m2(as.numeric(ai[i, AWC]), Ri) }

ARMS <- c("Default", "Somewhat Risk-Averse", "Extremely Risk-Averse",
          "Somewhat Risk-Seeking", "Extremely Risk-Seeking")
d <- rd("active_m2_treatment_data.csv") %>%
  select(participantId, ai_group, wave, pre_active_m2_ann) %>%
  inner_join(ai %>% select(participantId, ai_m2), by = "participantId") %>%
  filter(ai_group %in% ARMS, !is.na(ai_m2)) %>%
  mutate(
    BiasedCat = factor(case_when(
      ai_group == "Default"       ~ "No Bias",
      grepl("Somewhat", ai_group) ~ "Moderate Bias",
      TRUE                        ~ "Strong Bias"),
      levels = c("No Bias", "Moderate Bias", "Strong Bias")),
    dir = factor(ifelse(ai_group == "Default", "Default",
                 ifelse(grepl("Averse", ai_group), "Averse", "Seeking")),
                 levels = c("Averse", "Default", "Seeking")),
    wave = factor(wave))
cat("N by bias magnitude:\n"); print(table(d$BiasedCat))

# ── model: ai_m2 ~ BiasedCat + wave (HC3), mirroring the reference spec ───────
m <- lm(ai_m2 ~ BiasedCat + wave, data = d)
V <- vcovHC(m, type = "HC3")
cat("\n=== ai_m2 ~ BiasedCat + wave (HC3) ===\n"); print(coeftest(m, vcov = V))
emm <- emmeans(m, ~ BiasedCat, vcov. = V)
cat("\n=== marginal means (AI advice Active M2) ===\n")
es <- as.data.frame(summary(emm)); es$emmean_pct <- es$emmean * 100
print(es[, c("BiasedCat", "emmean", "SE", "emmean_pct")], row.names = FALSE, digits = 4)
pw <- as.data.frame(summary(pairs(emm, adjust = "fdr"), infer = TRUE))
nb <- table(d$BiasedCat)
pw$hedges_g <- mapply(function(est, ct) {
  g <- strsplit(as.character(ct), " - ")[[1]]
  (est / sigma(m)) * hedges_J(nb[[g[1]]], nb[[g[2]]]) }, pw$estimate, pw$contrast)
pw[, c("estimate", "lower.CL", "upper.CL")] <- pw[, c("estimate", "lower.CL", "upper.CL")] * 100
cat("--- pairwise (FDR); estimates in percentage units ---\n")
print(pw[, c("contrast", "estimate", "lower.CL", "upper.CL", "hedges_g", "p.value")],
      row.names = FALSE, digits = 3)
cat("\n[robustness] + pre-interaction Active M2 as covariate:\n")
m2r <- lm(ai_m2 ~ BiasedCat + wave + pre_active_m2_ann, data = d)
print(as.data.frame(summary(pairs(emmeans(m2r, ~ BiasedCat,
        vcov. = vcovHC(m2r, "HC3")), adjust = "fdr")))[, c("contrast","estimate","p.value")],
      row.names = FALSE, digits = 3)

# ── figure (second_figure_b2.R styling) ──────────────────────────────────────
# Reference palette; swap to the investment green/purple family if preferred.
ai_colors <- c("No Bias" = "#4E79A7", "Moderate Bias" = "#A2688F",
               "Strong Bias" = "#E15759")
pd <- as.data.frame(summary(emm)) %>%
  mutate(lower_95 = emmean - SE * qt(0.975, df),
         upper_95 = emmean + SE * qt(0.975, df),
         lab_y = ifelse(emmean >= 0, upper_95, lower_95) +
                 sign(emmean + 1e-12) * 0.05 * diff(range(c(emmean, 0))))

base_theme <- theme_classic() +
  theme(
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.line = element_blank(),
    text = element_text(family = "Avenir", color = "black"),
    axis.title.x = element_text(family = "Avenir", size = 13.5, margin = margin(t = 12)),
    axis.title.y = element_text(family = "Avenir", size = 13.5, margin = margin(r = 10)),
    axis.text.x = element_text(family = "Avenir", size = 11.7, color = "black",
                               margin = margin(t = 4)),
    axis.text.y = element_text(family = "Avenir", size = 11.7, color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    axis.ticks.length = unit(3.5, "pt"),
    panel.grid = element_blank(),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10))

p <- ggplot(pd, aes(x = BiasedCat, y = emmean, fill = BiasedCat)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  geom_col(alpha = 0.8, width = 0.6, color = "black", linewidth = 0.3) +
  geom_errorbar(aes(ymin = lower_95, ymax = upper_95),
                width = 0.15, linewidth = 0.5, color = "black") +
  geom_text(aes(y = lab_y, label = sprintf("%.3f", emmean)),
            size = 3.5, family = "Avenir", color = "black") +
  scale_fill_manual(values = ai_colors) +
  scale_y_continuous(name = expression("AI-recommended Portfolio Active M"^2),
                     labels = number_format(accuracy = 0.01)) +
  labs(x = "AI Bias Magnitude") + base_theme
print(p)
ggsave(file.path(FIG_DIR, "ai_advice_by_bias_magnitude.png"), p,
       width = 4.6, height = 4.3, dpi = 500)

# ── direction-split companion (magnitude pools opposite-signed directions) ────
d2 <- d %>% mutate(cell = factor(ifelse(ai_group == "Default", "Default",
             paste0(ifelse(grepl("Somewhat", ai_group), "Moderate ", "Extreme "),
                    ifelse(grepl("Averse", ai_group), "Averse", "Seeking"))),
             levels = c("Extreme Averse","Moderate Averse","Default",
                        "Moderate Seeking","Extreme Seeking")))
ms <- lm(ai_m2 ~ cell + wave, data = d2)
ems <- emmeans(ms, ~ cell, vcov. = vcovHC(ms, "HC3"))
cat("\n=== direction-split: AI advice Active M2 by magnitude x direction ===\n")
print(as.data.frame(summary(ems)), row.names = FALSE, digits = 4)
cat("--- pairwise (FDR across 10) ---\n")
print(as.data.frame(summary(pairs(ems, adjust = "fdr")))[, c("contrast","estimate","p.value")],
      row.names = FALSE, digits = 3)
pds <- as.data.frame(summary(ems)) %>%
  mutate(lower_95 = emmean - SE * qt(0.975, df), upper_95 = emmean + SE * qt(0.975, df),
         lab_y = ifelse(emmean >= 0, upper_95, lower_95) +
                 sign(emmean + 1e-12) * 0.05 * diff(range(c(emmean, 0))),
         side = ifelse(grepl("Averse", cell), "Averse",
                ifelse(grepl("Seeking", cell), "Seeking", "Default")))
scol <- c("Averse" = "#1B7837", "Default" = "#999999", "Seeking" = "#762A83")
ps <- ggplot(pds, aes(x = cell, y = emmean, fill = side)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  geom_col(alpha = 0.85, width = 0.6, color = "black", linewidth = 0.3) +
  geom_errorbar(aes(ymin = lower_95, ymax = upper_95),
                width = 0.15, linewidth = 0.5, color = "black") +
  geom_text(aes(y = lab_y, label = sprintf("%.3f", emmean)),
            size = 3.2, family = "Avenir", color = "black") +
  scale_fill_manual(values = scol) +
  scale_y_continuous(name = expression("AI-recommended Portfolio Active M"^2),
                     labels = number_format(accuracy = 0.01)) +
  labs(x = "AI Bias Magnitude and Direction") + base_theme +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 10))
print(ps)
ggsave(file.path(FIG_DIR, "ai_advice_by_bias_magnitude_split.png"), ps,
       width = 5.6, height = 4.3, dpi = 500)

# ── distribution figure (reference's AI-correctness density panel), 5 arms ────
# Magnitude alone pools opposite-signed directions, so the densities are drawn
# per arm: the two averse arms sit far right of Default, the two seeking arms
# on top of it.
arm_colors <- c("Extreme Averse"    = "#1B7837", "Moderate Averse" = "#7FBF7B",
                "Default"          = "#999999",
                "Moderate Seeking" = "#AF8DC3", "Extreme Seeking"  = "#762A83")
pdist <- d2 %>% filter(!is.na(ai_m2))
sstats <- pdist %>% group_by(cell) %>%
  summarise(n = n(), mean = mean(ai_m2), sd = sd(ai_m2), median = median(ai_m2),
            .groups = "drop")
cat("\n=== AI advice Active M2: raw distribution by arm ===\n")
print(as.data.frame(sstats), row.names = FALSE, digits = 3)

cat("--- Kolmogorov-Smirnov vs Default (FDR across 4) ---\n")
ks <- do.call(rbind, lapply(setdiff(levels(pdist$cell), "Default"), function(g)
  data.frame(arm = g,
             D = suppressWarnings(ks.test(pdist$ai_m2[pdist$cell == g],
                                          pdist$ai_m2[pdist$cell == "Default"]))$statistic,
             p = suppressWarnings(ks.test(pdist$ai_m2[pdist$cell == g],
                                          pdist$ai_m2[pdist$cell == "Default"]))$p.value)))
ks$p_fdr <- p.adjust(ks$p, "fdr"); print(ks, row.names = FALSE, digits = 3)

p_dist <- ggplot(pdist, aes(x = ai_m2, color = cell, fill = cell)) +
  geom_density(alpha = 0.25, linewidth = 0.7) +
  geom_vline(data = sstats, aes(xintercept = mean, color = cell),
             linetype = "solid", linewidth = 0.9, alpha = 0.9) +
  scale_fill_manual(values = arm_colors, name = "") +
  scale_color_manual(values = arm_colors, name = "") +
  scale_x_continuous(name = expression("AI-recommended Portfolio Active M"^2),
                     expand = expansion(mult = c(0, 0)),
                     labels = number_format(accuracy = 0.01)) +
  scale_y_continuous(name = "Probability Density",
                     expand = expansion(mult = c(0, 0.05)),
                     labels = number_format(accuracy = 1)) +
  base_theme +
  theme(legend.position = c(0.42, 0.98),
        legend.justification = c(1, 1),
        legend.background = element_rect(fill = "white", color = NA),
        legend.margin = margin(4, 4, 4, 4),
        legend.key = element_rect(fill = "white", color = NA),
        legend.key.height = unit(0.42, "cm"),
        legend.text = element_text(family = "Avenir", size = 9.5))
print(p_dist)
ggsave(file.path(FIG_DIR, "ai_advice_distribution_by_arm.png"), p_dist,
       width = 5.0, height = 4.3, dpi = 500)
cat(sprintf("\nSaved ai_advice_by_bias_magnitude{,_split}.png and ai_advice_distribution_by_arm.png in %s\n",
            SCRIPT_DIR))
