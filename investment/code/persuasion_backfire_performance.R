# ==============================================================================
# persuasion_backfire_performance.R
# Investment-experiment analog of first_single_figure_a4_separated.R (political):
# tau-gated Positive/Negative Persuasion/Backfire classification from PERFORMANCE,
# with the biased arm SEPARATED into Averse and Seeking (Default = reference).
#
# Mapping from the political script:
#   PrePerformance  -> pre_active_m2_ann   (initial portfolio, own 14-day window)
#   PostPerformance -> post_active_m2_ann  (final portfolio,   own 14-day window)
#   AICorrectness   -> ai_m2               (AI-only recommended portfolio's Active
#                                           M^2 over the SAME window; computed here)
#   Democrat/Republican -> Averse/Seeking  (Som+Ext pooled; Risk-Neutral excluded)
#   "| NID" FE + cluster ~UID -> wave FE + HC3 (one observation per participant)
#
# Classification (identical structure to the political script):
#   ai_ahead = ai_m2 - pre    (>0 AI's rec beat the participant's initial portfolio)
#   delta    = post - pre     (>0 participant's final portfolio improved)
#   ai_ahead >=  tau_a & delta >=  tau_d -> Positive persuasion
#   ai_ahead <= -tau_a & delta <= -tau_d -> Negative persuasion
#   ai_ahead <= -tau_a & delta >=  tau_d -> Positive backfire
#   ai_ahead >=  tau_a & delta <= -tau_d -> Negative backfire
#   otherwise                            -> No shift
#
# tau: the political tau=0.1 lives on a bounded correctness scale; Active M^2 is
# unbounded, so tau here = TAU_FRAC * SD of each quantity (default TAU_FRAC=0.1),
# with a sensitivity sweep printed. Override TAU_FRAC below if desired.
#
# CAVEAT (vs pre-registered §12): this infers persuasion/backfire from OUTCOMES
# (AI-ahead x improved), not from movement toward the AI (Influence). It mirrors
# the political proxy for cross-study comparability; it is NOT the §12 analysis.
#
# Inputs (notebook §13 export): active_m2_treatment_data.csv,
#   ai_only_by_wave.csv, daily_returns.csv   (all in )
#   setwd("investment/code"); source("_setup.R"); source("persuasion_backfire_performance.R")
# ==============================================================================
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(sandwich); library(lmtest)
})

.args <- commandArgs(FALSE); .f <- sub("^--file=", "", .args[grep("^--file=", .args)])
SCRIPT_DIR <- if (length(.f)) dirname(normalizePath(.f)) else getwd()
rd <- function(f) { p <- file.path(DATA_DIR, f); if (!file.exists(p)) p <- file.path(DATA_DIR, f)
                    read.csv(p, check.names = FALSE, stringsAsFactors = FALSE) }

TAU_FRAC <- 0.1   # tau = TAU_FRAC * SD(quantity); see header

# ── data ──────────────────────────────────────────────────────────────────────
m2  <- rd("active_m2_treatment_data.csv")
ai  <- rd("ai_only_by_wave.csv");  ai$eval_start <- as.Date(ai$eval_start)
ret <- rd("daily_returns.csv");    ret$Date <- as.Date(ret$Date)

ASSETS <- c("SHY","IEF","LQD","GLD","VNQ","SPY","XLF","XLE","XLI","XLP","XLU",
            "AAPL","MSFT","AMZN","GOOGL","NVDA","TSLA","SHOP","SNOW","PLTR","DKNG",
            "RIVN","CRSP","BTC","ETH")
AWC <- paste0("ai_w_", ASSETS)
stopifnot(all(ASSETS %in% names(ret)), all(AWC %in% names(ai)))

# AI-only Active M^2 over the participant's OWN window (same as wave_favored_ai_type.R)
active_m2 <- function(w, R) {
  s <- sum(w, na.rm = TRUE); if (!is.finite(s) || s <= 0) return(NA_real_)
  w  <- ifelse(is.na(w), 0, w) / s
  rp <- as.numeric(R %*% w); rb <- R[, "SPY"]; act <- rp - rb
  sa <- sd(act); sm <- sd(rb)
  if (is.na(sa) || is.na(sm) || sa == 0 || sm == 0) return(NA_real_)
  (mean(act) / sa) * sm * sqrt(252)
}
own_window <- function(s0) as.matrix(ret[ret$Date >= s0 & ret$Date <= (s0 + 14), ASSETS, drop = FALSE])
ai$ai_m2 <- NA_real_
for (i in seq_len(nrow(ai))) {
  Ri <- own_window(ai$eval_start[i]); if (nrow(Ri) < 3) next
  ai$ai_m2[i] <- active_m2(as.numeric(ai[i, AWC]), Ri)
}

d <- merge(m2, ai[, c("participantId", "ai_m2")], by = "participantId")

# 3 arms, Default = reference (mirrors Default/Democrat/Republican)
d$grp <- with(d, ifelse(ai_group %in% c("Extremely Risk-Averse","Somewhat Risk-Averse"), "Averse",
              ifelse(ai_group %in% c("Extremely Risk-Seeking","Somewhat Risk-Seeking"), "Seeking",
              ifelse(ai_group == "Default", "Default", NA_character_))))
d <- d[!is.na(d$grp) & !is.na(d$ai_m2) &
       !is.na(d$pre_active_m2_ann) & !is.na(d$post_active_m2_ann), ]
d$grp  <- factor(d$grp, levels = c("Default", "Averse", "Seeking"))
d$wave <- factor(d$wave)
cat(sprintf("N = %d with pre/post/AI Active M^2 (Neutral excluded): %s\n",
            nrow(d), paste(names(table(d$grp)), table(d$grp), sep = "=", collapse = ", ")))

# ── classification ────────────────────────────────────────────────────────────
d$ai_ahead <- d$ai_m2 - d$pre_active_m2_ann              # >0 -> AI's rec was better
d$delta    <- d$post_active_m2_ann - d$pre_active_m2_ann # >0 -> participant improved

classify <- function(tau_a, tau_d) with(d, case_when(
  ai_ahead >=  tau_a & delta >=  tau_d ~ "Positive persuasion",
  ai_ahead <= -tau_a & delta <= -tau_d ~ "Negative persuasion",
  ai_ahead <= -tau_a & delta >=  tau_d ~ "Positive backfire",
  ai_ahead >=  tau_a & delta <= -tau_d ~ "Negative backfire",
  TRUE                                 ~ "No shift"
))

# tau sensitivity sweep (counts) — the classification is tau-dependent; be open about it
cat("\n=== tau sensitivity (category counts; tau = frac * SD) ===\n")
cat(sprintf("    SD(ai_ahead) = %.4f, SD(delta) = %.4f\n", sd(d$ai_ahead), sd(d$delta)))
for (fr in c(0, 0.05, 0.1, 0.25)) {
  tb <- table(classify(fr * sd(d$ai_ahead), fr * sd(d$delta)))
  cat(sprintf("  frac=%.2f : %s\n", fr,
              paste(names(tb), tb, sep = "=", collapse = ", ")))
}

tau_a <- TAU_FRAC * sd(d$ai_ahead)
tau_d <- TAU_FRAC * sd(d$delta)
d$category <- classify(tau_a, tau_d)
cat(sprintf("\nUsing TAU_FRAC = %.2f -> tau_ai = %.4f, tau_delta = %.4f\n",
            TAU_FRAC, tau_a, tau_d))

# ── proportions among the four substantive outcomes (mirrors `props`) ─────────
props <- d %>%
  filter(category != "No shift") %>%
  count(grp, category) %>%
  group_by(grp) %>%
  mutate(prop = n / sum(n)) %>%
  select(-n) %>%
  pivot_wider(names_from = category, values_from = prop, values_fill = 0)
cat("\n=== proportions of substantive outcomes by arm (No shift excluded) ===\n")
print(as.data.frame(props), digits = 3, row.names = FALSE)
cat("\nNo-shift share by arm:\n")
print(round(tapply(d$category == "No shift", d$grp, mean), 3))

# ── outcome flags (mirrors `temp`) ────────────────────────────────────────────
d <- d %>% mutate(
  pos_backfire   = category == "Positive backfire",
  neg_backfire   = category == "Negative backfire",
  pos_persuasion = category == "Positive persuasion",
  neg_persuasion = category == "Negative persuasion",
  pos        = pos_backfire | pos_persuasion,
  neg        = neg_backfire | neg_persuasion,
  backfire   = pos_backfire | neg_backfire,
  persuasion = pos_persuasion | neg_persuasion
)

# ── marginal shares among substantive shifts (No-shift cases EXCLUDED) ────────
# persuasion + backfire = 100 and positive + negative = 100 within each row.
marg_pct <- function(dd) dd %>%
  summarise(n_substantive  = n(),
            persuasion_pct = 100 * mean(persuasion),
            backfire_pct   = 100 * mean(backfire),
            positive_pct   = 100 * mean(pos),
            negative_pct   = 100 * mean(neg))
subst <- d %>% filter(category != "No shift")
marg <- bind_rows(
  subst %>% group_by(grp) %>% marg_pct() %>% ungroup() %>% mutate(grp = as.character(grp)),
  subst %>% marg_pct() %>% mutate(grp = "Overall")
)
cat("\n=== marginal shares among substantive shifts (%; No-shift excluded) ===\n")
print(as.data.frame(marg), digits = 3, row.names = FALSE)

# ── headline model (mirrors fe_fit): neg_persuasion, wave FE, HC3 ─────────────
# glm + wave dummies replaces feglm "| NID" (3 waves only); HC3 replaces UID
# clustering (one observation per participant).
fit <- glm(neg_persuasion ~ grp + pre_active_m2_ann + wave,
           family = binomial(), data = d)
ct  <- coeftest(fit, vcov = vcovHC(fit, type = "HC3"))
cat("\n=== neg_persuasion ~ grp + pre_active_m2_ann + wave (logit, HC3) ===\n")
print(ct)

# OR table for the arm coefficients (mirrors or_table)
rows <- grep("^grp", rownames(ct))
or_table <- data.frame(
  term     = rownames(ct)[rows],
  estimate = ct[rows, 1], std.error = ct[rows, 2],
  OR       = exp(ct[rows, 1]),
  CI_lower = exp(ct[rows, 1] - 1.96 * ct[rows, 2]),
  CI_upper = exp(ct[rows, 1] + 1.96 * ct[rows, 2]),
  p.value  = ct[rows, 4]
)
cat("\n=== odds ratios vs Default (HC3 CIs) ===\n")
print(or_table, digits = 3, row.names = FALSE)

# ── same logit for every flag (exploratory; grp rows only) ────────────────────
cat("\n=== all outcome flags ~ grp + pre + wave (logit HC3; grp rows) ===\n")
FLAGS <- c("pos_persuasion", "neg_persuasion", "pos_backfire", "neg_backfire",
           "pos", "neg", "persuasion", "backfire")
allf <- do.call(rbind, lapply(FLAGS, function(y) {
  f <- glm(as.formula(paste(y, "~ grp + pre_active_m2_ann + wave")),
           family = binomial(), data = d)
  cc <- tryCatch(coeftest(f, vcov = vcovHC(f, type = "HC3")), error = function(e) NULL)
  if (is.null(cc)) return(NULL)
  r <- grep("^grp", rownames(cc))
  data.frame(outcome = y, term = sub("^grp", "", rownames(cc)[r]),
             estimate = cc[r, 1], OR = exp(cc[r, 1]), se = cc[r, 2], p = cc[r, 4])
}))
print(allf, digits = 3, row.names = FALSE)
