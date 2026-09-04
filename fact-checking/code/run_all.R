# ==============================================================================
# run_all.R — reproduce every figure and statistic in the fact-checking study.
#
#   setwd("fact-checking/code"); source("run_all.R")
#
# Scripts share a global environment on purpose: preprcessing.R builds the
# analysis frames and later scripts add derived columns to them, so the order
# below matters. Figures are written to ../figures/ ; the regression tables are
# printed to the console.
# ==============================================================================
source("_setup.R")

ORDER <- c(
  # --- data pipeline --------------------------------------------------------
  "preprcessing.R",

  # --- single assistant: five treatment arms (main text) ---------------------
  "first_figure_single_a1_separated.R",   # post-interaction performance
  "first_figure_single_a2_separated.R",   # evaluative bias across news slants
  "first_figure_single_a3_separated.R",   # conversation length and engagement
  "first_single_figure_a4.R",             # persuasion / backfire decomposition
  "first_single_figure_a4_separated.R",   # ... split by assistant stance

  # --- bias magnitude --------------------------------------------------------
  "second_figure_b1.R",                   # performance / length / perception
  "second_figure_b2.R",                   # assistant's own fact-checking accuracy

  # --- willingness to recommend ---------------------------------------------
  "discussion_1.R",

  # --- participant-assistant stance relationship -----------------------------
  "third_figure_c1.R",

  # --- dual assistants -------------------------------------------------------
  "forth_figure_d1.R",                    # performance, single vs dual arms
  "export_five_arm_data.R",               # writes ../data/five_arm_single_dual.csv
  "forth_figure_d2.R",                    # perceived improvement (MCMCglmm)
  "forth_figure_d3.R",
  "forth_figure_d4.R",                    # conversation length
  "dual_ai_decomposition_v2.R",           # six-cell persuasion decomposition

  # --- engagement ------------------------------------------------------------
  "engagement_coef_heatmap.R",            # 7 arms x 5 engagement dimensions

  # --- extended data ---------------------------------------------------------
  "extended_neutral_comparison.R",        # neutralized arm vs the other three
  "ai_correctness_by_arm.R",              # accuracy density, five arms
  "dual_selection_quality.R"              # advisor selection under conflict
)

for (f in ORDER) {
  cat("\n", strrep("=", 78), "\n>>> ", f, "\n", strrep("=", 78), "\n", sep = "")
  source(f)
}
cat("\nDone. Figures are in ../figures/\n")
