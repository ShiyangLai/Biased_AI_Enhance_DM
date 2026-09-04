# ==============================================================================
# persuasion_backfire_dual.R
# DUAL-AI investment analog of the political dual_ai_decomposition_v2.R,
# with investment conventions taken from persuasion_backfire_performance.R.
#
# Pair configuration is classified FIRST (political v2 taxonomy):
#   d1 = ai1_m2 - pre,  d2 = ai2_m2 - pre   (each AI's rec vs initial portfolio,
#                                            both scored on the participant's
#                                            OWN 14-day window)
#   split        : one AI ahead (>= tau_a), the other behind (<= -tau_a)
#   cons_ahead   : >= 1 ahead, none behind   ("one tied" folded into consensus)
#   cons_behind  : >= 1 behind, none ahead
#   tied         : both within +-tau_a
# then delta = post - pre gated at tau_d:
#   |delta| < tau_d                 -> No shift            (excluded)
#   cons_ahead  & delta >=  tau_d   -> PP  Positive persuasion
#   cons_behind & delta <= -tau_d   -> NP  Negative persuasion
#   cons_behind & delta >=  tau_d   -> PB  Positive backfire
#   cons_ahead  & delta <= -tau_d   -> NB  Negative backfire
#   split       & delta >=  tau_d   -> SB  Selection: followed the BETTER advisor
#   split       & delta <= -tau_d   -> SW  Selection: followed the WORSE advisor
#   tied pair with a shift          -> tied_shift          (excluded, footnoted)
# Within each arm the six cell %s sum to 100 (No shift / tied excluded).
#
# tau: as in the single script, tau = TAU_FRAC * SD (Active M^2 is unbounded);
# tau_a uses the SD of the stacked (d1, d2), tau_d the SD of delta.
# Sensitivity sweep printed. CAVEAT: outcome-inferred persuasion (political
# proxy for cross-study comparability), NOT the pre-registered §12 analysis.
#
# Arms (participant-relative design; from the §15 export, i.e. persona logs):
#   dual_nonbiased -> Dual Default, dual_opposition -> Dual Opposition,
#   dual_balanced  -> Dual Balanced
#
# Inputs: dual_active_m2.csv, daily_returns.csv (),
#         ai_portfolios_dual_extracted.csv (; GPT-4o extraction of
#         BOTH AIs' final recommendations, ai_portfolio_extraction_dual.py)
# Run:  Rscript persuasion_backfire_dual.R     (console output only)
# ==============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(sandwich); library(lmtest); library(emmeans)
})

.args <- commandArgs(FALSE); .f <- sub("^--file=", "", .args[grep("^--file=", .args)])
SCRIPT_DIR <- if (length(.f)) dirname(normalizePath(.f)) else getwd()
rd <- function(f) { p <- file.path(DATA_DIR, f); if (!file.exists(p)) p <- file.path(DATA_DIR, f)
                    read.csv(p, check.names = FALSE, stringsAsFactors = FALSE) }

TAU_FRAC <- 0.1   # tau = TAU_FRAC * SD(quantity); mirrors the single-AI script

# ── data ──────────────────────────────────────────────────────────────────────
m2  <- rd("dual_active_m2.csv");  m2$eval_start <- as.Date(m2$eval_start)
ret <- rd("daily_returns.csv");   ret$Date <- as.Date(ret$Date)
ex  <- read.csv(file.path(DATA_DIR, "ai_portfolios_dual_extracted.csv"),
                check.names = FALSE, stringsAsFactors = FALSE)

ASSETS <- c("SHY","IEF","LQD","GLD","VNQ","SPY","XLF","XLE","XLI","XLP","XLU",
            "AAPL","MSFT","AMZN","GOOGL","NVDA","TSLA","SHOP","SNOW","PLTR","DKNG",
            "RIVN","CRSP","BTC","ETH")
W1 <- paste0("ai1_w_", ASSETS); W2 <- paste0("ai2_w_", ASSETS)
stopifnot(all(ASSETS %in% names(ret)), all(c(W1, W2) %in% names(ex)))

ok <- function(v) v == "True" | v == TRUE
cat(sprintf("extraction file: %d rows, both-AI extractable %d\n",
            nrow(ex), sum(ok(ex$ai1_extractable) & ok(ex$ai2_extractable))))

# each AI's recommended portfolio scored on the participant's OWN window
active_m2 <- function(w, R) {
  s <- sum(w, na.rm = TRUE); if (!is.finite(s) || s <= 0) return(NA_real_)
  w  <- ifelse(is.na(w), 0, w) / s
  rp <- as.numeric(R %*% w); rb <- R[, "SPY"]; act <- rp - rb
  sa <- sd(act); sm <- sd(rb)
  if (is.na(sa) || is.na(sm) || sa == 0 || sm == 0) return(NA_real_)
  (mean(act) / sa) * sm * sqrt(252)
}
own_window <- function(s0) as.matrix(ret[ret$Date >= s0 & ret$Date <= (s0 + 14), ASSETS, drop = FALSE])

d <- merge(m2, ex, by.x = "participantId", by.y = "pid")
# condition cross-check: §15 (persona logs) is authoritative
mism <- sum(d$dual_condition != d$condition, na.rm = TRUE)
if (mism > 0) cat(sprintf("WARNING: %d condition mismatches vs extraction file (using dual_active_m2)\n", mism))

d$ai1_m2 <- NA_real_; d$ai2_m2 <- NA_real_
for (i in seq_len(nrow(d))) {
  Ri <- own_window(d$eval_start[i]); if (nrow(Ri) < 3) next
  d$ai1_m2[i] <- active_m2(as.numeric(d[i, W1]), Ri)
  d$ai2_m2[i] <- active_m2(as.numeric(d[i, W2]), Ri)
}

n0 <- nrow(d)
d <- d[!is.na(d$ai1_m2) & !is.na(d$ai2_m2) &
       !is.na(d$pre_active_m2_ann) & !is.na(d$post_active_m2_ann), ]
d$Arm <- factor(recode(d$dual_condition,
                       dual_nonbiased  = "Dual Default",
                       dual_balanced   = "Dual Balanced",
                       dual_opposition = "Dual Opposition"),
                levels = c("Dual Default", "Dual Opposition", "Dual Balanced"))
d$wave <- factor(d$wave)
cat(sprintf("N = %d with pre/post/AI1/AI2 Active M^2 (dropped %d w/o both AI recs): %s\n",
            nrow(d), n0 - nrow(d),
            paste(names(table(d$Arm)), table(d$Arm), sep = "=", collapse = ", ")))

# ── six-cell classification (political v2 regimes; tau-gated) ─────────────────
d$d1    <- d$ai1_m2 - d$pre_active_m2_ann
d$d2    <- d$ai2_m2 - d$pre_active_m2_ann
d$delta <- d$post_active_m2_ann - d$pre_active_m2_ann

classify <- function(tau_a, tau_d) with(d, {
  regime <- case_when(
    (d1 >= tau_a & d2 <= -tau_a) | (d1 <= -tau_a & d2 >= tau_a) ~ "split",
    pmax(d1, d2) >=  tau_a & pmin(d1, d2) > -tau_a ~ "cons_ahead",
    pmin(d1, d2) <= -tau_a & pmax(d1, d2) <  tau_a ~ "cons_behind",
    TRUE ~ "tied")
  case_when(
    abs(delta) < tau_d ~ "noshift",
    regime == "cons_ahead"  & delta >=  tau_d ~ "PP",
    regime == "cons_behind" & delta <= -tau_d ~ "NP",
    regime == "cons_behind" & delta >=  tau_d ~ "PB",
    regime == "cons_ahead"  & delta <= -tau_d ~ "NB",
    regime == "split" & delta >=  tau_d ~ "SB",
    regime == "split" & delta <= -tau_d ~ "SW",
    TRUE ~ "tied_shift")
})

sd_a <- sd(c(d$d1, d$d2)); sd_d <- sd(d$delta)
cat("\n=== tau sensitivity (category counts; tau = frac * SD) ===\n")
cat(sprintf("    SD(stacked d1,d2) = %.4f, SD(delta) = %.4f\n", sd_a, sd_d))
for (fr in c(0.02, 0.05, 0.1, 0.25)) {
  tb <- table(classify(fr * sd_a, fr * sd_d))
  cat(sprintf("  frac=%.2f : %s\n", fr, paste(names(tb), tb, sep = "=", collapse = ", ")))
}

tau_a <- TAU_FRAC * sd_a; tau_d <- TAU_FRAC * sd_d
d$cat6 <- classify(tau_a, tau_d)
cat(sprintf("\nUsing TAU_FRAC = %.2f -> tau_ai = %.4f, tau_delta = %.4f\n",
            TAU_FRAC, tau_a, tau_d))

CELLS <- c("PP", "NP", "PB", "NB", "SB", "SW")
s <- d %>% filter(cat6 %in% CELLS)

# ── figure numbers ────────────────────────────────────────────────────────────
pers_pct <- 100 * mean(s$cat6 %in% c("PP", "NP"))
back_pct <- 100 * mean(s$cat6 %in% c("PB", "NB"))
sel_pct  <- 100 * mean(s$cat6 %in% c("SB", "SW"))
pos_pct  <- 100 * mean(s$cat6 %in% c("PP", "PB", "SB"))
cell_tab <- 100 * prop.table(table(s$Arm, factor(s$cat6, levels = CELLS)), margin = 1)

cat("\n================ FIGURE NUMBERS (dual mockup) ================\n")
cat(sprintf("Column headers : Persuasion %.1f%% | Backfire %.1f%% | Selection %.1f%%\n",
            pers_pct, back_pct, sel_pct))
cat(sprintf("Row bands      : Positive %.1f%% | Negative %.1f%%\n", pos_pct, 100 - pos_pct))
lab6 <- c(PP = "Positive x Persuasion", PB = "Positive x Backfire",
          SB = "Positive x Selection (followed better)",
          NP = "Negative x Persuasion", NB = "Negative x Backfire",
          SW = "Negative x Selection (followed worse)")
for (k in c("PP", "PB", "SB", "NP", "NB", "SW")) {
  v <- cell_tab[, k]
  cat(sprintf("%-40s: %s\n", lab6[k],
              paste(sprintf("%s %.1f%%", rownames(cell_tab), v), collapse = ", ")))
}
cat("(within each arm the six cells sum to 100)\n")

cat("\nExcluded-share diagnostics by arm:\n")
print(round(100 * prop.table(table(d$Arm, d$cat6 == "noshift"), margin = 1)[, "TRUE", drop = FALSE], 1))
cat(sprintf("tied_shift (tied pair, moved) n = %d; substantive n = %d (SB=%d, SW=%d)\n",
            sum(d$cat6 == "tied_shift"), nrow(s), sum(s$cat6 == "SB"), sum(s$cat6 == "SW")))
cat("\nSubstantive-shift counts by arm x cell:\n")
print(table(s$Arm, factor(s$cat6, levels = CELLS)))

# ── statistical tests ─────────────────────────────────────────────────────────
cat("\n================ STATISTICAL TESTS ================\n")
cat("\n=== global: arm x six-cell distribution (substantive shifts) ===\n")
tb6 <- table(s$Arm, factor(s$cat6, levels = CELLS))
print(chisq.test(tb6))
set.seed(123); print(chisq.test(tb6, simulate.p.value = TRUE, B = 20000))

# flags on the FULL sample (No shift = FALSE), mirroring the single-AI script
d <- d %>% mutate(
  pos_persuasion = cat6 == "PP", neg_persuasion = cat6 == "NP",
  pos_backfire   = cat6 == "PB", neg_backfire   = cat6 == "NB",
  sel_better     = cat6 == "SB", sel_worse      = cat6 == "SW",
  persuasion = pos_persuasion | neg_persuasion,
  backfire   = pos_backfire   | neg_backfire,
  selection  = sel_better     | sel_worse,
  pos = pos_persuasion | pos_backfire | sel_better,
  neg = neg_persuasion | neg_backfire | sel_worse
)

run_logit <- function(y, data, label) {
  f  <- glm(as.formula(paste(y, "~ Arm + pre_active_m2_ann + wave")),
            family = binomial(), data = data)
  cc <- tryCatch(coeftest(f, vcov = vcovHC(f, type = "HC3")), error = function(e) NULL)
  if (is.null(cc)) return(NULL)
  r <- grep("^Arm", rownames(cc))
  out <- data.frame(outcome = label, term = sub("^Arm", "vs Default: ", rownames(cc)[r]),
                    estimate = cc[r, 1], OR = exp(cc[r, 1]), se = cc[r, 2], p = cc[r, 4])
  # Opposition vs Balanced contrast via emmeans (same model, FDR across the 3 pairs)
  em <- tryCatch(as.data.frame(pairs(emmeans(f, ~ Arm), adjust = "none")),
                 error = function(e) NULL)
  if (!is.null(em)) {
    ob <- em[grepl("Opposition", em$contrast) & grepl("Balanced", em$contrast), ]
    if (nrow(ob) == 1)
      out <- rbind(out, data.frame(outcome = label, term = ob$contrast,
                                   estimate = ob$estimate, OR = exp(ob$estimate),
                                   se = ob$SE, p = ob$p.value))
  }
  out
}

cat("\n=== each flag ~ Arm + pre + wave (logit HC3; full sample incl. No shift) ===\n")
FLAGS <- c("pos_persuasion", "neg_persuasion", "pos_backfire", "neg_backfire",
           "sel_better", "sel_worse", "persuasion", "backfire", "selection",
           "pos", "neg")
allf <- do.call(rbind, lapply(FLAGS, function(y) run_logit(y, d, y)))
print(allf, digits = 3, row.names = FALSE)

# conditional tests: "which arm is better under which configuration"
# (rebuild the subsets AFTER the flag mutate so they carry the flag columns)
s <- d %>% filter(cat6 %in% CELLS)
cat("\n=== conditional: P(positive | substantive shift) ~ Arm + pre + wave ===\n")
print(run_logit("pos", s, "pos | substantive"), digits = 3, row.names = FALSE)

cons <- s %>% filter(cat6 %in% c("PP", "NP", "PB", "NB"))
cat("\n=== conditional: P(backfire | consensus-pair shift) ~ Arm + pre + wave ===\n")
print(run_logit("backfire", cons, "backfire | consensus"), digits = 3, row.names = FALSE)

spl <- s %>% filter(cat6 %in% c("SB", "SW"))
cat("\n=== conditional: P(followed better | split-pair shift) ~ Arm + pre + wave ===\n")
cat(sprintf("(split-pair substantive n = %d: %s)\n", nrow(spl),
            paste(names(table(spl$Arm)), table(spl$Arm), sep = "=", collapse = ", ")))
if (nrow(spl) >= 30 && all(table(spl$Arm) >= 5)) {
  print(run_logit("sel_better", spl, "sel_better | split"), digits = 3, row.names = FALSE)
} else {
  cat("too few split-pair shifts for the covariate logit; Fisher test instead:\n")
  print(fisher.test(table(spl$Arm, spl$cat6 == "SB")))
  print(100 * prop.table(table(spl$Arm, spl$cat6), margin = 1))
}

cat("\nCAVEAT: outcome-inferred persuasion/backfire (political-proxy taxonomy),\n")
cat("tau-dependent; ex-post Active M^2 verdicts are regime-embedded.\n")
