# ==============================================================================
# descriptives_table_education.R — SI descriptive-statistics table for the classroom
# experiment, in the same layout as the investment experiment's Table S-x
# (Source | Variable | Mean | SD | Min | Median | Max), but panelled by
# TIME-INVARIANT vs TIME-VARYING instead of Single AI / Dual AI.
#
# Panel A — time-invariant, unit = STUDENT (N = 39). All 24 numeric pre-survey
#   covariates are constant within student (verified), so they are summarised at
#   the student level. Summarising them over the 312 student-weeks would repeat
#   each student ~8 times and shrink every SD.
# Panel B — time-varying, unit = STUDENT-WEEK (N = 312).
#
# The two nominal covariates (academic level, prior IDE-integrated tool) are NOT
# in this table; they go in the companion categorical figure.
#
# An N column is carried per row because, unlike the investment table, the
# denominators genuinely differ: prior ML experience is missing for 2 students,
# grades exist only for submitted weeks, and treatment order is defined only on
# the 39 randomized treatment weeks.
#
# Inputs : data/supplementary_dnr_student_week.csv  (312 D/N/R student-weeks,
#            outcomes = 3-grader mean of GPT + Gemini + human)
#          data/telemetry_student_week.csv         (exact session telemetry)
# Outputs: figures/descriptives_table_education.csv
#          figures/descriptives_table_education.md
#          figures/descriptives_table_education.tex
#   setwd("education/code"); source("_setup.R"); source("descriptives_table_education.R")
# ==============================================================================
suppressPackageStartupMessages({ library(dplyr); library(readr) })

IN <- DATA_DIR; OUT <- FIG_DIR

dnr <- read.csv(file.path(IN, "supplementary_dnr_student_week.csv"),
                stringsAsFactors = FALSE, check.names = FALSE)

# ── Panel A spine: one row per student ────────────────────────────────────────
INVARIANT <- c("prior_prog", "prior_ml", "n_languages", "use_learning", "use_coding",
               "use_writing", "hours_ai_7d", "verify_freq", "eval_confidence",
               "n_ai_tools", "claude_freq", "n_ai_settings", "n_ai_tasks",
               "pref_reliable", "pref_novel", "pref_safe_pressure", "enjoy_explore",
               "pref_one_answer", "pref_several", "stop_good_enough", "effort_better",
               "comfort_reject", "worry_dependence", "expect_learn")
varying <- vapply(INVARIANT, function(c)
  any(tapply(dnr[[c]], dnr$student_id, function(x) length(unique(x))) > 1), logical(1))
stopifnot(!any(varying))                       # guard: panel A must be student-level
stu <- dnr[!duplicated(dnr$student_id), c("student_id", INVARIANT)]

# ── Panel B spine: one row per student-week, telemetry rebuilt from the raw log ─
tel <- read_csv(file.path(IN, "telemetry_student_week.csv"), show_col_types = FALSE)
sw <- dnr %>% left_join(tel, by = c("student_id", "week")) %>%
  # no rows in the log = no conversation that week, which is a zero, not missing
  mutate(across(c(conv_round, conv_length), ~ifelse(is.na(.x), 0, .x)),
         # 0 = assignment not submitted; grades are summarised over submitted weeks
         coding = ifelse(!is.na(combined_all_asgn_score) & combined_all_asgn_score > 0,
                         combined_all_asgn_score, NA),
         memo   = ifelse(!is.na(combined_all_memo_score) & combined_all_memo_score > 0,
                         combined_all_memo_score, NA))

# ── Row specifications: source label, variable label, data vector ─────────────
row_spec <- function(source, label, x) {
  x <- suppressWarnings(as.numeric(x)); x <- x[!is.na(x)]
  data.frame(Source = source, Variable = label, N = length(x),
             Mean = mean(x), SD = sd(x), Min = min(x),
             Median = median(x), Max = max(x), stringsAsFactors = FALSE)
}
A <- function(src, lab, code) row_spec(src, lab, stu[[code]])

BACK <- "Pre-survey: background"; USE30 <- "Pre-survey: AI use, past 30 d"
EXPR <- "Pre-survey: AI experience"; AGREE <- "Pre-survey: agreement (1-7)"

panelA <- bind_rows(
  A(BACK,  "Prior programming experience (1-3)",      "prior_prog"),
  A(BACK,  "Prior ML/AI experience (1-3)",            "prior_ml"),
  A(BACK,  "Languages/tools used (count)",            "n_languages"),
  A(USE30, "AI use: learning/explanations (0-4)",     "use_learning"),
  A(USE30, "AI use: coding/programming (0-4)",        "use_coding"),
  A(USE30, "AI use: writing (0-4)",                   "use_writing"),
  A(EXPR,  "AI hours, past 7 d (bin midpoint)",       "hours_ai_7d"),
  A(EXPR,  "Verifies AI outputs (0-4)",               "verify_freq"),
  A(EXPR,  "Confidence evaluating AI code (1-7)",     "eval_confidence"),
  A(EXPR,  "AI coding tools used (count)",            "n_ai_tools"),
  A(EXPR,  "Prior Claude Code frequency (0-4)",       "claude_freq"),
  A(EXPR,  "AI tool settings used (count)",           "n_ai_settings"),
  A(EXPR,  "AI coding tasks (count)",                 "n_ai_tasks"),
  A(AGREE, "Prefers reliable/standard solutions",     "pref_reliable"),
  A(AGREE, "Prefers new/unconventional approaches",   "pref_novel"),
  A(AGREE, "Chooses safest under time pressure",      "pref_safe_pressure"),
  A(AGREE, "Enjoys exploring alternatives",           "enjoy_explore"),
  A(AGREE, "Prefers one best answer",                 "pref_one_answer"),
  A(AGREE, "Prefers several candidate strategies",    "pref_several"),
  A(AGREE, "Stops searching when good enough",        "stop_good_enough"),
  A(AGREE, "Willing to spend extra effort",           "effort_better"),
  A(AGREE, "Comfortable rejecting AI suggestions",    "comfort_reject"),
  A(AGREE, "Worries about AI dependence",             "worry_dependence"),
  A(AGREE, "Expects AI to help learning",             "expect_learn"))

# Telemetry is heavily zero-inflated (only 125 of 312 weeks have any conversation),
# so it is reported as a use indicator plus both the all-weeks and use-conditioned
# distributions; an unconditional mean alone would sit far above a median of zero.
used <- sw$conv_round > 0
panelB <- bind_rows(
  row_spec("Randomization",      "Assigned week (1-9)",              sw$week),
  row_spec("Randomization",      "Novelty-biased week (0/1)",        as.integer(sw$policy == "NOVELTY_BIASED")),
  row_spec("Randomization",      "Reliability-biased week (0/1)",    as.integer(sw$policy == "RELIABILITY_BIASED")),
  row_spec("Randomization",      "Biased week was student's first (0/1)", as.integer(sw$treatment_order == 1)),
  row_spec("Assignment grading", "Coding assignment submitted (0/1)", as.integer(!is.na(sw$coding))),
  row_spec("Assignment grading", "Coding grade (0-10)",              sw$coding),
  row_spec("Assignment grading", "Memo submitted (0/1)",             as.integer(!is.na(sw$memo))),
  row_spec("Assignment grading", "Memo grade (0-10)",                sw$memo),
  row_spec("Session telemetry",  "Any assistant use (0/1)",          as.integer(used)),
  row_spec("Session telemetry",  "Reply turns, all weeks",           sw$conv_round),
  row_spec("Session telemetry",  "Reply turns, weeks with use",      sw$conv_round[used]),
  row_spec("Session telemetry",  "Words written, all weeks",         sw$conv_length),
  row_spec("Session telemetry",  "Words written, weeks with use",    sw$conv_length[used]))

PANEL_A_TITLE <- sprintf("Time-invariant: student level (N = %d)", nrow(stu))
PANEL_B_TITLE <- sprintf("Time-varying: student-week level (N = %s)",
                         format(nrow(sw), big.mark = ","))
out <- bind_rows(cbind(Panel = PANEL_A_TITLE, panelA),
                 cbind(Panel = PANEL_B_TITLE, panelB))
write_csv(out, file.path(DATA_DIR, "descriptives_table_education.csv"))

# ── Rendered markdown + LaTeX (booktabs), 3 decimals as in the reference ──────
f3 <- function(x) formatC(x, format = "f", digits = 3)
NUMCOLS <- c("Mean", "SD", "Min", "Median", "Max")

md <- c("| Source | Variable | N | Mean | SD | Min | Median | Max |",
        "|---|---|---:|---:|---:|---:|---:|---:|")
tex <- c("\\begin{tabular}{llrrrrrr}", "\\toprule",
         "\\textbf{Source} & \\textbf{Variable} & \\textbf{N} & \\textbf{Mean} & \\textbf{SD} & \\textbf{Min} & \\textbf{Median} & \\textbf{Max} \\\\")
for (ttl in c(PANEL_A_TITLE, PANEL_B_TITLE)) {
  blk <- out[out$Panel == ttl, ]
  md  <- c(md, sprintf("| **%s** | | | | | | | |", ttl))
  tex <- c(tex, "\\midrule",
           sprintf("\\multicolumn{8}{c}{\\textit{%s}} \\\\", gsub("&", "\\\\&", ttl)),
           "\\midrule")
  for (i in seq_len(nrow(blk))) {
    v <- vapply(NUMCOLS, function(k) f3(blk[[k]][i]), character(1))
    md  <- c(md,  sprintf("| %s | %s | %d | %s |", blk$Source[i], blk$Variable[i],
                          blk$N[i], paste(v, collapse = " | ")))
    tex <- c(tex, sprintf("%s & \\textit{%s} & %d & %s \\\\", blk$Source[i], blk$Variable[i],
                          blk$N[i], paste(v, collapse = " & ")))
  }
}
tex <- c(tex, "\\bottomrule", "\\end{tabular}")
writeLines(md,  file.path(OUT, "descriptives_table_education.md"))
writeLines(tex, file.path(OUT, "descriptives_table_education.tex"))

cat(paste(md, collapse = "\n"), "\n")
cat(sprintf("\nSaved s1_descriptive_stats.{csv,md,tex} in %s\n", OUT))
cat("Done: descriptives_table_education.R\n")
