# ==============================================================================
# run_all.R — reproduce every figure and table in the education study.
#
#   setwd("education/code"); source("run_all.R")
#
# Scripts are independent: each reads the prepared CSVs in ../data and writes
# its own figures and tables to ../figures. The order below is thematic, with
# one dependency: table_delegation.R and table_verification_share.R read the
# contrasts file that delegation_by_arm.R writes, so that script runs first.
# ==============================================================================
source("_setup.R")

ORDER <- c(
  # --- sample description and randomisation checks --------------------------
  "descriptives_table_education.R",      # Table S11: continuous and binary measures
  "balance_check_education.R",           # covariate balance, novelty vs reliability

  # --- performance -----------------------------------------------------------
  "plot_performance_treatment.R",        # three-arm coding / memo ladders
  "bias_magnitude_outcomes.R",           # Fig. 2c, 3c, 3d: performance, turns, helpfulness
  "bias_side_performance.R",             # Fig. 2g: echo chamber vs opposition

  # --- engagement, delegation and perception ---------------------------------
  "engagement_coef_heatmap_education.R", # arms x five annotated engagement dimensions
  "delegation_by_arm.R",                 # Extended Data Fig. 2d (writes the contrasts file)

  # --- downstream: final project --------------------------------------------
  "final_project_models.R",              # dose / exposure models (console + CSVs)
  "final_project_outcomes.R",            # Extended Data Fig. 4j and the SI dose figures

  # --- stand-alone assistant runs -------------------------------------------
  "ai_standalone_performance.R",         # Extended Data Fig. 6c

  # --- extended data: the neutralized arm -----------------------------------
  "neutral_arm_performance.R",           # Extended Data Fig. 4i
  "neutral_arm_engagement.R",            # Extended Data Fig. 4k
  "neutral_arm_helpfulness.R",           # Extended Data Fig. 4l

  # --- robustness ------------------------------------------------------------
  "student_effects_robustness.R",        # clustered / random intercept / fixed effect
  "perceived_spec_check.R",              # ordinal vs linear, perceived improvement
  "usefulness_spec_check.R",             # ordinal vs linear, usefulness

  # --- SI regression tables --------------------------------------------------
  "table_performance_three_arm.R",
  "table_performance_by_direction.R",
  "table_echo_chamber.R",
  "table_engagement_three_arm.R",
  "table_helpfulness_three_arm.R",
  "table_delegation.R",                  # after delegation_by_arm.R
  "table_verification_share.R",          # after delegation_by_arm.R
  "table_performance_four_arm.R",
  "table_engagement_helpfulness_four_arm.R",
  "table_final_project.R",
  "table_grader_weight_ladders.R"
)

for (f in ORDER) {
  cat("\n", strrep("=", 78), "\n>>> ", f, "\n", strrep("=", 78), "\n", sep = "")
  source(f)
}
cat("\nDone. Figures and tables are in ../figures/\n")
