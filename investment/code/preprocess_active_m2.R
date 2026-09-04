# ==============================================================================
# preprocess_active_m2.R
# Load & prepare the pre-registered Active M² treatment-effect dataset.
#
# Input CSV (produced by the notebook export cell §13) must contain:
#   participantId, ai_group, wave, pre_active_m2_ann, post_active_m2_ann
# One row per eligible single-AI participant (14-day window elapsed).
# ==============================================================================
suppressPackageStartupMessages(library(dplyr))

# Risk gradient, most-averse -> most-seeking (used for factor order & y-axis).
# NOTE: Risk-Neutral is EXCLUDED from this analysis (rows dropped by the %in% filter below).
ARM_LEVELS <- c(
  "Extremely Risk-Averse", "Somewhat Risk-Averse",
  "Default", "Somewhat Risk-Seeking", "Extremely Risk-Seeking"
)

load_active_m2 <- function(path = "active_m2_treatment_data.csv") {
  df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  need <- c("ai_group", "wave", "pre_active_m2_ann", "post_active_m2_ann")
  stopifnot(all(need %in% names(df)))

  df <- df %>%
    filter(
      !is.na(post_active_m2_ann), !is.na(pre_active_m2_ann),
      ai_group %in% ARM_LEVELS
    ) %>%
    mutate(
      # keep the risk-gradient order; Default is the treatment reference
      ai_group = factor(ai_group, levels = ARM_LEVELS),
      ai_group = relevel(ai_group, ref = "Default"),
      wave     = factor(wave)
    )

  # Clustering variable: evaluation window (ISO year-week of eval_start)
  if ("eval_start" %in% names(df)) {
    df$eval_week <- factor(strftime(as.Date(df$eval_start), "%G-%V"))
  } else {
    warning("eval_start not in CSV -> eval_week clustering unavailable; re-run the §13 export.")
    df$eval_week <- factor(NA)
  }

  message(sprintf("Loaded %d participants across %d arms, %d waves.",
                  nrow(df), nlevels(droplevels(df$ai_group)), nlevels(df$wave)))
  print(table(as.character(df$ai_group)))
  df
}

