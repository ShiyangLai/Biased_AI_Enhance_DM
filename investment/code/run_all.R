# ==============================================================================
# run_all.R — reproduce every figure and statistic in the investment study.
#
#   setwd("investment/code"); source("run_all.R")
#
# Unlike the fact-checking pipeline, these scripts are independent: each reads
# the prepared CSVs in ../data and writes its own figures to ../figures. The
# order below is thematic, not a dependency chain, so any single script can be
# run on its own after source("_setup.R").
# ==============================================================================
source("_setup.R")

ORDER <- c(
  # --- sample description and randomisation checks --------------------------
  "descriptives_table_investment.R",     # Table S1, continuous/binary measures
  "balance_check_investment.R",          # covariate balance, biased vs default

  # --- single assistant: performance ----------------------------------------
  "plot_active_m2_treatment.R",          # Active M2 ladder, pooled and by wave
  "plot_grs_treatment.R",                # ex-ante efficiency (GRS F) by frontier
  "wave_favored_ai_type.R",              # which orientation each wave rewarded
  "plot_active_m2_priming.R",            # label-only priming variant

  # --- bias magnitude and direction -----------------------------------------
  "bias_magnitude_outcomes.R",           # performance / participation / confidence
  "ai_advice_by_bias_magnitude.R",       # the assistant's own portfolio quality
  "bias_side_performance.R",             # echo chamber vs opposition
  "bias_side_grs.R",                     # ... on ex-ante efficiency
  "persuasion_backfire_performance.R",   # four-cell decomposition, single

  # --- counterfactual market regimes ----------------------------------------
  "counterfactual_bull_m2.R",            # bull/neutral/bear re-scoring
  "active_m2_treatment_bull_overlay.R",  # main-text overlay, seeking arms
  "bias_side_counterfactual.R",          # echo/opposition under both regimes

  # --- portfolio structure and diversity ------------------------------------
  "portfolio_structure.R",               # cross-participant diversity, single
  "diversity_single_vs_dual.R",          # ... across all five conditions

  # --- engagement and perception --------------------------------------------
  "engagement_by_arm.R",                 # reply turns, engagement profile
  "engagement_coef_heatmap_investment.R",# 5 conditions x 5 engagement dimensions
  "reply_turns_single_vs_dual.R",        # follow-up participation
  "perception_outcomes.R",               # confidence and perceived improvement
  "perceived_ai_role.R",                 # tool vs influencing agent

  # --- dual assistants -------------------------------------------------------
  "active_m2_single_vs_dual.R",          # five-condition performance comparison
  "persuasion_backfire_dual.R",          # six-cell decomposition, dual
  "advisor_selection_dual.R",            # selection under conflicting advice

  # --- extended data ---------------------------------------------------------
  "neutral_arm_analysis.R",              # the neutralized arm on every outcome
  "positioning_map_cross_domain.R"       # investment against fact-checking
)

for (f in ORDER) {
  cat("\n", strrep("=", 78), "\n>>> ", f, "\n", strrep("=", 78), "\n", sep = "")
  source(f)
}
cat("\nDone. Figures are in ../figures/\n")
