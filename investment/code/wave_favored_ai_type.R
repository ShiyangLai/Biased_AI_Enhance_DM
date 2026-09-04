# ==============================================================================
# wave_favored_ai_type.R
# Which AI-recommendation TYPE does each wave's market regime favor?
#
# Method (per-individual, then aggregate):
#   1. For EACH participant, evaluate their AI-ONLY recommended portfolio over
#      THEIR OWN realized 14-day window (eval_start -> +14 days) -> Active M².
#   2. To decide whether a wave prefers seeking vs averse, AGGREGATE those
#      per-individual Active M² values by wave x arm (mean, se), rank the
#      favored type, and test the arm x wave interaction.
#
# Inputs (written by the notebook §13 export):
#   ai_only_by_wave.csv   participantId, wave, ai_group, eval_start, ai_w_<TICKER> x25
#   daily_returns.csv     Date + 25 asset daily-return columns (canonical tickers)
# Run:  Rscript wave_favored_ai_type.R   (from project root or this folder)
# ==============================================================================
suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(tidyr); library(scales)
})

.args <- commandArgs(FALSE); .f <- sub("^--file=", "", .args[grep("^--file=", .args)])
SCRIPT_DIR <- if (length(.f)) dirname(normalizePath(.f)) else getwd()
rd <- function(f) {
  p <- file.path(DATA_DIR, f); if (!file.exists(p)) p <- file.path(DATA_DIR, f)
  read.csv(p, check.names = FALSE, stringsAsFactors = FALSE)
}

ASSETS <- c("SHY","IEF","LQD","GLD","VNQ","SPY","XLF","XLE","XLI","XLP","XLU",
            "AAPL","MSFT","AMZN","GOOGL","NVDA","TSLA","SHOP","SNOW","PLTR","DKNG",
            "RIVN","CRSP","BTC","ETH")
AWC <- paste0("ai_w_", ASSETS)

ai  <- rd("ai_only_by_wave.csv"); ai$eval_start <- as.Date(ai$eval_start)
ret <- rd("daily_returns.csv");   ret$Date <- as.Date(ret$Date)
stopifnot(all(ASSETS %in% names(ret)), all(AWC %in% names(ai)))

# Active M² (annualized) = IR * sd(SPY) * sqrt(252); w in ASSETS order, R = (T x N) matrix
active_m2 <- function(w, R) {
  s <- sum(w, na.rm = TRUE); if (!is.finite(s) || s <= 0) return(NA_real_)
  w  <- ifelse(is.na(w), 0, w) / s
  rp <- as.numeric(R %*% w); rb <- R[, "SPY"]; act <- rp - rb
  sa <- sd(act); sm <- sd(rb)
  if (is.na(sa) || is.na(sm) || sa == 0 || sm == 0) return(NA_real_)
  (mean(act) / sa) * sm * sqrt(252)
}

# ── PER-INDIVIDUAL: AI-only Active M² over each participant's OWN 14-day window ─
own_window <- function(s0) as.matrix(ret[ret$Date >= s0 & ret$Date <= (s0 + 14), ASSETS, drop = FALSE])
ai$ai_m2 <- NA_real_
ai$r_crypto <- NA_real_; ai$r_tech <- NA_real_; ai$r_def <- NA_real_; ai$r_spy <- NA_real_
for (i in seq_len(nrow(ai))) {
  Ri <- own_window(ai$eval_start[i]); if (nrow(Ri) < 3) next
  ai$ai_m2[i] <- active_m2(as.numeric(ai[i, AWC]), Ri)
  cr <- function(cols) prod(1 + rowMeans(Ri[, cols, drop = FALSE])) - 1
  ai$r_crypto[i] <- cr(c("BTC","ETH")); ai$r_tech[i] <- cr(c("NVDA","TSLA","SHOP","SNOW","PLTR"))
  ai$r_def[i] <- cr(c("SHY","IEF","GLD")); ai$r_spy[i] <- prod(1 + Ri[, "SPY"]) - 1
}
cat(sprintf("Computed AI-only Active M² for %d/%d participants (own 14-day windows).\n",
            sum(!is.na(ai$ai_m2)), nrow(ai)))

ARMS <- c("Extremely Risk-Averse","Somewhat Risk-Averse","Risk-Neutral",
          "Default","Somewhat Risk-Seeking","Extremely Risk-Seeking")
ai$ai_group <- factor(ai$ai_group, levels = ARMS)

# ── AGGREGATE by wave x arm (this is where the wave-preference is decided) ────
summ <- ai %>% filter(!is.na(ai_m2)) %>%
  group_by(wave, ai_group) %>%
  summarise(n = n(), mean = mean(ai_m2), se = sd(ai_m2) / sqrt(n()), .groups = "drop")
cat("\n=== AI-only Active M² by wave x arm (per-individual windows, aggregated) ===\n")
print(as.data.frame(summ), digits = 3)

fav <- summ %>% group_by(wave) %>% slice_max(mean, n = 1) %>% ungroup()
cat("\n=== Favored AI type per wave (max mean AI Active M²) ===\n")
print(as.data.frame(fav[, c("wave", "ai_group", "mean")]), digits = 3)

# Seeking - Averse gap (risk axis) by wave, with a paired-ish t-test on individuals
side <- ai %>% mutate(side = case_when(grepl("Averse", ai_group) ~ "Averse",
                                       grepl("Seeking", ai_group) ~ "Seeking", TRUE ~ "Other")) %>%
  filter(side %in% c("Averse", "Seeking"), !is.na(ai_m2))
gap <- side %>% group_by(wave, side) %>% summarise(m = mean(ai_m2), .groups = "drop") %>%
  pivot_wider(names_from = side, values_from = m) %>% mutate(Seeking_minus_Averse = Seeking - Averse)
gap$p_value <- sapply(gap$wave, function(w) {
  d <- side[side$wave == w, ]; tryCatch(t.test(ai_m2 ~ side, data = d)$p.value, error = function(e) NA)
})
cat("\n=== Seeking - Averse AI Active M² gap by wave (Welch t across individuals) ===\n")
print(as.data.frame(gap), digits = 3)

# ── test: does the arm ranking differ by wave? (arm x wave interaction) ──────
fit <- lm(ai_m2 ~ ai_group * factor(wave), data = ai)
cat("\n=== Type x wave interaction (ANOVA) ===\n"); print(anova(fit))

# ── regime label: mean cumulative asset-class return over participants' windows ─
regime <- ai %>% filter(!is.na(ai_m2)) %>% group_by(wave) %>%
  summarise(n = n(), SPY = mean(r_spy), Crypto = mean(r_crypto),
            Tech = mean(r_tech), Defensive = mean(r_def), .groups = "drop")
cat("\n=== Regime (mean cumulative return over participants' own windows) ===\n")
print(as.data.frame(regime), row.names = FALSE, digits = 3)

# ── figure: three wave panels, AI-only Active M² by arm (house style) ────────
YORDER <- c("Extremely Risk-Averse"=6,"Somewhat Risk-Averse"=5,"Risk-Neutral"=4,
            "Default"=3,"Somewhat Risk-Seeking"=2,"Extremely Risk-Seeking"=1)
SHORT  <- c("Extremely Risk-Averse"="E-Averse","Somewhat Risk-Averse"="S-Averse","Risk-Neutral"="Neutral",
            "Default"="Default","Somewhat Risk-Seeking"="S-Seeking","Extremely Risk-Seeking"="E-Seeking")
bias_colors <- c("E-Averse"="#1B7837","S-Averse"="#7FBF7B","Neutral"="#BABABA",
                 "Default"="#999999","S-Seeking"="#AF8DC3","E-Seeking"="#762A83")
labs_t2b <- unname(SHORT[names(sort(YORDER, decreasing = TRUE))])

pd <- summ %>% mutate(
  arm = as.character(ai_group), y = YORDER[arm], label = SHORT[arm],
  lo90 = mean - se*qnorm(.95),  hi90 = mean + se*qnorm(.95),
  lo95 = mean - se*qnorm(.975), hi95 = mean + se*qnorm(.975),
  lo99 = mean - se*qnorm(.995), hi99 = mean + se*qnorm(.995)
)

theme_n <- theme_classic() + theme(
  text = element_text(size = 8), axis.text = element_text(color = "black", size = 8),
  axis.line = element_line(color = "black", linewidth = .5),
  axis.ticks = element_line(color = "black", linewidth = .5),
  panel.grid = element_blank(),
  panel.border = element_rect(color = "black", fill = NA, linewidth = .5),
  strip.background = element_rect(fill = "grey92", color = "black", linewidth = .5),
  strip.text = element_text(size = 9, face = "bold"), panel.spacing = unit(.6, "lines"),
  plot.margin = margin(15, 15, 10, 10))

p <- ggplot(pd, aes(y = y)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60", linewidth = .4) +
  geom_errorbarh(aes(xmin = lo99, xmax = hi99, color = label), height = .25, linewidth = 1.2, alpha = .3) +
  geom_errorbarh(aes(xmin = lo95, xmax = hi95, color = label), height = .20, linewidth = .9, alpha = .5) +
  geom_errorbarh(aes(xmin = lo90, xmax = hi90, color = label), height = .15, linewidth = .7, alpha = .8) +
  geom_point(aes(x = mean, color = label), size = 3, alpha = .9) +
  facet_wrap(~ wave, nrow = 1, labeller = as_labeller(function(x) gsub("wave", "Wave ", x))) +
  scale_color_manual(values = bias_colors, breaks = labs_t2b) +
  scale_y_continuous(breaks = 6:1, labels = labs_t2b, expand = expansion(add = c(.4, .4))) +
  scale_x_continuous(labels = label_number(accuracy = 0.01)) +
  labs(x = expression("AI-only Active M"^2 * "  (each over the participant's own window)"),
       y = NULL, color = NULL) +
  guides(color = guide_legend(nrow = 1)) +
  theme_n + theme(legend.position = "bottom", axis.text.y = element_text(angle = 0, hjust = 1))

print(p)
ggsave(file.path(FIG_DIR, "wave_favored_ai_type.pdf"), p, width = 12, height = 4.5)
ggsave(file.path(FIG_DIR, "wave_favored_ai_type.png"), p, width = 12, height = 4.5, dpi = 300)
cat(sprintf("\nSaved wave_favored_ai_type.{pdf,png} in %s\n", SCRIPT_DIR))
