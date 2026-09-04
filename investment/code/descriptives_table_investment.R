# ==============================================================================
# descriptives_table_investment.R
# Table S1 for the investment experiment: descriptive statistics of continuous
# (or binary) variables, by data source, for the single- and dual-assistant
# analytic samples. Investment analog of the fact-checking Table S1, with the
# variable list adapted to this design (portfolio outcomes rather than
# item-level correctness, and the pre/post allocation questionnaire items).
#
# Sources:
#   CloudResearch                 demographics from the assignment files
#   Pre-interaction questionnaire elicited risk preference, financial literacy,
#                                 trust in AI, confidence in the initial portfolio
#   Conversation text             follow-up turns and words, and the Active M2 of
#                                 the assistant's own recommended portfolio
#                                 (both assistants in the dual experiment)
#   Pre-/Post-Evaluation          the participant's own portfolio scored on their
#                                 14-day evaluation window
#   Post-interaction questionnaire confidence, perceived improvement, expected
#                                 performance
#
# Inputs:  active_m2_treatment_data.csv, dual_active_m2.csv,
#          participant_covariates.csv, dual_covariates.csv,
#          perceived_improvement.csv, ai_only_by_wave.csv, daily_returns.csv,
#          ../engagement_annotations{,_dual}.csv, ../ai_portfolios_dual_extracted.csv
# Outputs: descriptives_table_investment.csv (long, with a Sample column)
#   setwd("investment/code"); source("_setup.R"); source("descriptives_table_investment.R")
# ==============================================================================
suppressPackageStartupMessages({ library(dplyr) })

.args <- commandArgs(FALSE); .f <- sub("^--file=", "", .args[grep("^--file=", .args)])
SCRIPT_DIR <- if (length(.f)) dirname(normalizePath(.f)) else getwd()
NB_DIR <- DATA_DIR
rd  <- function(f) read.csv(file.path(DATA_DIR, f), check.names = FALSE, stringsAsFactors = FALSE)
rdn <- function(f) read.csv(file.path(DATA_DIR, f),    check.names = FALSE, stringsAsFactors = FALSE)

ASSETS <- c("SHY","IEF","LQD","GLD","VNQ","SPY","XLF","XLE","XLI","XLP","XLU",
            "AAPL","MSFT","AMZN","GOOGL","NVDA","TSLA","SHOP","SNOW","PLTR","DKNG",
            "RIVN","CRSP","BTC","ETH")
active_m2 <- function(w, R) {
  s <- sum(w, na.rm = TRUE); if (!is.finite(s) || s <= 0) return(NA_real_)
  w <- ifelse(is.na(w), 0, w) / s
  a <- as.numeric(R %*% w) - R[, "SPY"]; sa <- sd(a); sm <- sd(R[, "SPY"])
  if (is.na(sa) || sa == 0 || sm == 0) return(NA_real_)
  (mean(a) / sa) * sm * sqrt(252) }
score_window <- function(W, starts, ret) vapply(seq_len(nrow(W)), function(i) {
  Ri <- as.matrix(ret[ret$Date >= starts[i] & ret$Date <= (starts[i] + 14), ASSETS, drop = FALSE])
  if (nrow(Ri) < 3) NA_real_ else active_m2(as.numeric(W[i, ]), Ri) }, numeric(1))

ret <- rd("daily_returns.csv"); ret$Date <- as.Date(ret$Date)

# ── demographics (de-identified: participant id, age, sex) ───────────────────
asg <- rd("demographics.csv") %>%
  transmute(participantId, cr_age = age, cr_male = male) %>%
  distinct(participantId, .keep_all = TRUE)

pim <- rd("perceived_improvement.csv")

# ── single-assistant sample ───────────────────────────────────────────────────
ai1 <- rd("ai_only_by_wave.csv"); ai1$eval_start <- as.Date(ai1$eval_start)
ai1$ai_m2 <- score_window(ai1[, paste0("ai_w_", ASSETS)], ai1$eval_start, ret)
eg1 <- rdn("engagement_annotations.csv") %>%
  filter(status %in% c("ok", "no_followup")) %>%
  select(participantId, n_followup_turns, followup_words)

S <- rd("active_m2_treatment_data.csv") %>%
  select(participantId, pre_active_m2_ann, post_active_m2_ann) %>%
  left_join(rd("participant_covariates.csv"), by = "participantId") %>%
  left_join(ai1 %>% select(participantId, ai_m2), by = "participantId") %>%
  left_join(eg1, by = "participantId") %>%
  left_join(pim %>% filter(experiment == "single") %>%
              select(participantId, pre_conf, post_conf, perceived_improve, perf_exp),
            by = "participantId") %>%
  left_join(asg, by = "participantId")

# ── dual-assistant sample ─────────────────────────────────────────────────────
D0 <- rd("dual_active_m2.csv"); D0$eval_start <- as.Date(D0$eval_start)
aid <- rdn("ai_portfolios_dual_extracted.csv"); names(aid)[names(aid) == "pid"] <- "participantId"
aid <- aid %>% select(participantId, all_of(c(paste0("ai1_w_", ASSETS), paste0("ai2_w_", ASSETS))))
Dm <- D0 %>% left_join(aid, by = "participantId")
Dm$ai1_m2 <- score_window(Dm[, paste0("ai1_w_", ASSETS)], Dm$eval_start, ret)
Dm$ai2_m2 <- score_window(Dm[, paste0("ai2_w_", ASSETS)], Dm$eval_start, ret)
eg2 <- rdn("engagement_annotations_dual.csv") %>%
  filter(status %in% c("ok", "no_followup")) %>%
  select(participantId, n_followup_turns, followup_words)

D <- Dm %>% select(participantId, pre_active_m2_ann, post_active_m2_ann, ai1_m2, ai2_m2) %>%
  left_join(rd("dual_covariates.csv"), by = "participantId") %>%
  left_join(eg2, by = "participantId") %>%
  left_join(pim %>% filter(experiment == "dual") %>%
              select(participantId, pre_conf, post_conf, perceived_improve, perf_exp),
            by = "participantId") %>%
  left_join(asg, by = "participantId")

# ── table ─────────────────────────────────────────────────────────────────────
SPEC <- list(
  c("CloudResearch",                  "Age (years)",                          "cr_age"),
  c("CloudResearch",                  "Male",                                 "cr_male"),
  c("Pre-interaction questionnaire",  "Risk preference",                      "risk_pref_score"),
  c("Pre-interaction questionnaire",  "Financial literacy",                   "fin_lit_score"),
  c("Pre-interaction questionnaire",  "Trust in AI",                          "trust_ai_score"),
  c("Pre-interaction questionnaire",  "Portfolio confidence",                 "pre_conf"),
  c("Conversation text",              "Follow-up turns",                      "n_followup_turns"),
  c("Conversation text",              "Follow-up words",                      "followup_words"),
  c("Conversation text",              "AI-recommended portfolio Active M2",   "ai_m2"),
  c("Conversation text",              "AI 1 recommended portfolio Active M2", "ai1_m2"),
  c("Conversation text",              "AI 2 recommended portfolio Active M2", "ai2_m2"),
  c("Pre-evaluation",                 "Pre-interaction Active M2",            "pre_active_m2_ann"),
  c("Post-evaluation",                "Post-interaction Active M2",           "post_active_m2_ann"),
  c("Post-interaction questionnaire", "Portfolio confidence",                 "post_conf"),
  c("Post-interaction questionnaire", "Perceived improvement",                "perceived_improve"),
  c("Post-interaction questionnaire", "Expected performance",                 "perf_exp"))

describe <- function(d, label) {
  rows <- lapply(SPEC, function(s) {
    if (!s[3] %in% names(d)) return(NULL)
    x <- suppressWarnings(as.numeric(d[[s[3]]])); x <- x[is.finite(x)]
    if (!length(x)) return(NULL)
    data.frame(Sample = label, Source = s[1], Variable = s[2], N = length(x),
               Mean = mean(x), SD = sd(x), Min = min(x),
               Median = median(x), Max = max(x), stringsAsFactors = FALSE) })
  do.call(rbind, rows) }

tab <- rbind(describe(S, sprintf("Single AI (N = %s)", format(nrow(S), big.mark = ","))),
             describe(D, sprintf("Dual AI (N = %s)",   format(nrow(D), big.mark = ","))))
tab[, c("Mean","SD","Min","Median","Max")] <- round(tab[, c("Mean","SD","Min","Median","Max")], 3)
for (s in unique(tab$Sample)) {
  cat(sprintf("\n%s\n", s))
  print(tab[tab$Sample == s, c("Source","Variable","N","Mean","SD","Min","Median","Max")],
        row.names = FALSE) }
write.csv(tab, file.path(DATA_DIR, "descriptives_table_investment.csv"), row.names = FALSE)
cat(sprintf("\nSaved descriptives_table_investment.csv in %s\n", SCRIPT_DIR))
