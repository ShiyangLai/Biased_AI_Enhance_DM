# ==============================================================================
# balance_check_investment.R
# Investment analog of the fact-checking study's balance check: pre-treatment
# descriptive statistics and covariate balance, BIASED (pooled) vs DEFAULT.
#
# Same statistics as the fact-checking SI:
#   continuous  -> absolute standardized mean difference (AMSD) with a pooled-
#                  variance denominator, group means tested with Welch's t
#   categorical -> Cramer's V on the 2 x k table, tested with a chi-squared
#                  test of independence
#   negligible imbalance threshold 0.10 for both
#
# Grouping (per the fact-checking Biased vs Non-Biased contrast):
#   single: Biased = the four risk-biased arms + the neutralized arm;
#           Non-Biased = Default
#   dual:   Biased = dual_balanced + dual_opposition; Non-Biased = dual_nonbiased
#
# The trading-frequency item allows "don't know", carried here as its own
# category rather than as missing data, so no imputation is needed anywhere.
#
# Inputs:  participant_covariates.csv, dual_covariates.csv,
#          active_m2_treatment_data.csv, dual_active_m2.csv ()
# Outputs: balance_investment_single.csv, balance_investment_dual.csv
#   setwd("investment/code"); source("_setup.R"); source("balance_check_investment.R")
# ==============================================================================
suppressPackageStartupMessages({ library(dplyr); library(ggplot2) })

.args <- commandArgs(FALSE); .f <- sub("^--file=", "", .args[grep("^--file=", .args)])
SCRIPT_DIR <- if (length(.f)) dirname(normalizePath(.f)) else getwd()
rd <- function(f) read.csv(file.path(DATA_DIR, f), stringsAsFactors = FALSE)

CONT <- c("Age_enc", "invest_own_score", "risk_pref_score", "time_preference_switch",
          "fin_lit_score", "fin_lit_selfassess_1_enc", "trust_ai_score",
          "pre_active_m2_ann")
CATG <- c("Sex_enc", "work_in_finance_enc", "first_invest_time_enc",
          "trade_freq_12m_enc", "news_follow_freq_enc", "fin_overconfidence_1_enc",
          "portfolio_confidence_1_enc", "market_outlook_2wk_enc",
          "market_volatility_2w_enc")
LABEL <- c(Age_enc = "Age (years)", invest_own_score = "Investment-ownership index",
           risk_pref_score = "Risk preference", time_preference_switch = "Time-preference switch point",
           fin_lit_score = "Financial literacy (objective)",
           fin_lit_selfassess_1_enc = "Financial literacy (self-assessed)",
           trust_ai_score = "Trust in AI", pre_active_m2_ann = "Pre-interaction Active M²",
           Sex_enc = "Sex", work_in_finance_enc = "Works in finance",
           first_invest_time_enc = "Years since first investment",
           trade_freq_12m_enc = "Trades in past 12 months",
           news_follow_freq_enc = "Follows financial news",
           fin_overconfidence_1_enc = "Relative investing skill",
           portfolio_confidence_1_enc = "Portfolio confidence",
           market_outlook_2wk_enc = "Two-week market outlook",
           market_volatility_2w_enc = "Expected volatility")

# AMSD, pooled-variance denominator (Austin 2009); |x_B - x_NB| / sqrt((s2_B + s2_NB)/2)
amsd <- function(x, g) {
  a <- x[g == "Biased"]; b <- x[g == "Non-Biased"]
  a <- a[is.finite(a)];  b <- b[is.finite(b)]
  s2 <- (var(a) + var(b)) / 2
  if (!is.finite(s2) || s2 <= 0) return(NA_real_)
  abs(mean(a) - mean(b)) / sqrt(s2)
}

run_balance <- function(d, tag, out_csv) {
  g <- d$.grp
  cat(sprintf("\n\n################  %s  ################\n", tag))
  cat(sprintf("N = %d  (Biased %d, Non-Biased %d)\n", nrow(d),
              sum(g == "Biased"), sum(g == "Non-Biased")))

  # ── continuous: AMSD + Welch's t ───────────────────────────────────────────
  rc <- do.call(rbind, lapply(CONT, function(v) {
    x <- d[[v]]; ok <- is.finite(x)
    tt <- t.test(x[ok & g == "Biased"], x[ok & g == "Non-Biased"], var.equal = FALSE)
    data.frame(Variable = unname(LABEL[v]), type = "continuous",
               Biased = sprintf("%.2f (%.2f)", mean(x[ok & g == "Biased"]), sd(x[ok & g == "Biased"])),
               NonBiased = sprintf("%.2f (%.2f)", mean(x[ok & g == "Non-Biased"]), sd(x[ok & g == "Non-Biased"])),
               effect = amsd(x, g), stat = unname(tt$statistic),
               df = unname(tt$parameter), p = tt$p.value) }))

  # ── categorical: Cramer's V + chi-squared test of independence ─────────────
  rk <- do.call(rbind, lapply(CATG, function(v) {
    x <- d[[v]]; x[is.na(x)] <- "Don't know"
    tb <- table(g, as.character(x))
    ct <- suppressWarnings(chisq.test(tb))
    k  <- min(dim(tb))                      # = 2 for a 2 x k table
    V_std <- sqrt(unname(ct$statistic) / (sum(tb) * (k - 1)))     # standard Cramer's V
    V_fc  <- sqrt(unname(ct$statistic) / (sum(tb) * (k + 1)))     # fact-checking SI formula
    data.frame(Variable = unname(LABEL[v]), type = "categorical",
               Biased = sprintf("%d levels", ncol(tb)), NonBiased = "",
               effect = V_std, stat = unname(ct$statistic),
               df = unname(ct$parameter), p = ct$p.value, V_fc_formula = V_fc) }))

  rc$V_fc_formula <- NA_real_
  res <- rbind(rc, rk)
  res$p_fdr <- p.adjust(res$p, "fdr")
  res$over_0.10 <- ifelse(res$effect >= 0.10, "*", "")
  cat("\n=== Balance: AMSD (continuous) / Cramer's V (categorical), Biased vs Default ===\n")
  print(res[order(-res$effect), c("Variable", "type", "effect", "over_0.10",
                                  "stat", "df", "p", "p_fdr")],
        row.names = FALSE, digits = 3)
  cat(sprintf("\neffect size >= 0.10: %d of %d (largest %.3f, %s)\n",
              sum(res$effect >= 0.10, na.rm = TRUE), nrow(res),
              max(res$effect, na.rm = TRUE),
              res$Variable[which.max(res$effect)]))
  cat(sprintf("nominally significant (p < .05): %d; after BH-FDR: %d\n",
              sum(res$p < .05), sum(res$p_fdr < .05)))
  cat("\n[Cramer's V under the fact-checking SI formula sqrt(chi2/(n(k+1))):]\n")
  print(rk[order(-rk$V_fc_formula), c("Variable", "effect", "V_fc_formula")],
        row.names = FALSE, digits = 3)
  write.csv(res, file.path(DATA_DIR, out_csv), row.names = FALSE)
  invisible(res)
}

BIASED_SINGLE <- c("Somewhat Risk-Averse", "Extremely Risk-Averse",
                   "Somewhat Risk-Seeking", "Extremely Risk-Seeking", "Risk-Neutral")
s <- rd("active_m2_treatment_data.csv") %>%
  select(participantId, ai_group, pre_active_m2_ann) %>%
  inner_join(rd("participant_covariates.csv"), by = "participantId") %>%
  mutate(.grp = ifelse(ai_group %in% BIASED_SINGLE, "Biased", "Non-Biased"))
rs <- run_balance(s, "SINGLE-ASSISTANT: pooled biased (incl. neutralized) vs Default",
                  "balance_investment_single.csv")

dd <- rd("dual_active_m2.csv") %>%
  select(participantId, dual_condition, pre_active_m2_ann) %>%
  inner_join(rd("dual_covariates.csv"), by = "participantId") %>%
  mutate(.grp = ifelse(dual_condition == "dual_nonbiased", "Non-Biased", "Biased"))
rd_ <- run_balance(dd, "DUAL-ASSISTANT: balanced + opposition vs dual default",
                   "balance_investment_dual.csv")
cat("\nLove plots are drawn from these CSVs by figure_balance_investment.py,\n",
    "which ports the fact-checking notebook's plotting cell verbatim.\n", sep = "")
