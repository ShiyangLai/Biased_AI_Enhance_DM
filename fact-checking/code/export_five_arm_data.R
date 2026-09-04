# ============================================================================
# EXPORT — single vs dual 5-arm dataset (performance + perceived improvement)
#
# Run this in the SAME R session where `combined_data` already exists
# (i.e., after running forth_figure_d1.R, which builds the 5-arm combined_data:
#  Single_AI_Non_Biased, Single_AI_Biased, Dual_AI_Non_Biased,
#  Dual_AI_Opposition, Dual_AI_Balanced).
#
# It writes `five_arm_single_dual.csv` to the working directory. Copy that file
# into the other project alongside five_arm_analysis_standalone.R.
# ============================================================================
library(dplyr)
library(readr)

stopifnot(exists("combined_data"))

keep <- c("ExperimentType", "UID", "NID",
          "PrePerformance", "PostPerformance",      # performance outcome + covariate
          "PerceivedImproveCode", "AIInterMean",    # perceived improvement / meaningfulness
          "UStanceLabel", "UIdeo", "AICorrectness")  # covariates

missing <- setdiff(keep, names(combined_data))
if (length(missing)) warning("Columns not found and not exported: ", paste(missing, collapse = ", "))

five_arm <- combined_data %>%
  dplyr::select(any_of(keep)) %>%
  mutate(ExperimentType = as.character(ExperimentType)) %>%
  dplyr::filter(ExperimentType %in% c("Single_AI_Non_Biased", "Single_AI_Biased",
                               "Dual_AI_Non_Biased", "Dual_AI_Opposition", "Dual_AI_Balanced"))

# Write to the project's Data/ folder (absolute path, so it's easy to find).
# Change `out` if you want it elsewhere.
out <- "../data/five_arm_single_dual.csv"
write_csv(five_arm, out)

cat("Wrote", out, "—", nrow(five_arm), "rows\n")
print(table(five_arm$ExperimentType))
