# =============================================================================
# a4 analysis, but with the AI treatment arm SEPARATED: "Biased" is split into
# Democrat and Republican, giving three arms (Default = reference, Democrat,
# Republican). Everything else (outcome categorization, proportions, the
# fixed-effects logit) is unchanged.
# =============================================================================

tau <- 0.1            # minimum change to count as persuasion/backfire

# Three-level treatment arm (Default = reference). Neutral treatment excluded.
single_ai_processed_ <- single_ai_processed_ %>%
  dplyr::filter(AIStanceLabel_S %in% c("Default", "Democrat", "Republican")) %>%
  mutate(BiasedType = factor(
    case_when(
      AIStanceLabel_S == "Default"    ~ "Default",
      AIStanceLabel_S == "Democrat"   ~ "Democrat",
      AIStanceLabel_S == "Republican" ~ "Republican"
    ),
    levels = c("Default", "Democrat", "Republican")
  ))

labeled <- single_ai_processed_ %>%
  mutate(
    ## Direction of AI advantage before the exchange
    ai_ahead   = AICorrectness - PrePerformance,     # >0 → AI better; <0 → participant better

    ## Change in participant accuracy
    delta      = PostPerformance - PrePerformance,   # >0 → improved; <0 → worsened

    category = case_when(
      ai_ahead  >=  tau & delta  >=  tau  ~ "Positive persuasion",
      ai_ahead  <=  -tau & delta  <= -tau  ~ "Negative persuasion",
      ai_ahead  <= -tau & delta  >=  tau  ~ "Positive backfire",
      ai_ahead  >= tau & delta  <= -tau  ~ "Negative backfire",
      TRUE                              ~ "No shift"              # ties or small moves
    )
  )

## Proportion of each outcome in Default vs Democrat vs Republican groups
props <- labeled %>%
  dplyr::filter(category != "No shift") %>%      # focus on the four substantive outcomes
  count(BiasedType, category) %>%
  group_by(BiasedType) %>%
  mutate(prop = n / sum(n)) %>%
  pivot_wider(names_from = category,
              values_from = prop,
              values_fill = 0)

print(props, digits = 3)

# =============================================================================
# Aggregate outcome percentages by arm:
#   persuasion vs. backfire   and   positive vs. negative
# =============================================================================
agg_pct <- function(d) d %>% summarise(
  n              = dplyr::n(),
  Persuasion_pct = 100 * mean(category %in% c("Positive persuasion", "Negative persuasion")),
  Backfire_pct   = 100 * mean(category %in% c("Positive backfire",   "Negative backfire")),
  Positive_pct   = 100 * mean(category %in% c("Positive persuasion", "Positive backfire")),
  Negative_pct   = 100 * mean(category %in% c("Negative persuasion", "Negative backfire")),
  .groups = "drop"
)

## (a) Share of the four substantive shifts (No shift excluded) — matches `props`.
##     Within each row: Persuasion + Backfire = 100% and Positive + Negative = 100%.
shifts <- labeled %>% dplyr::filter(category != "No shift")
totals_shift <- bind_rows(
  agg_pct(group_by(shifts, BiasedType)) %>% mutate(BiasedType = as.character(BiasedType)),
  agg_pct(shifts) %>% mutate(BiasedType = "Overall")
)
cat("\n=== Outcome composition among substantive shifts (No shift excluded), % ===\n")
print(totals_shift, digits = 3)

## (b) Share of ALL observations (No shift included) — i.e., prevalence.
##     AnyShift_pct = Persuasion_pct + Backfire_pct = 100% - %No shift.
totals_all <- bind_rows(
  agg_pct(group_by(labeled, BiasedType)) %>% mutate(BiasedType = as.character(BiasedType)),
  agg_pct(labeled) %>% mutate(BiasedType = "Overall")
) %>%
  mutate(AnyShift_pct = Persuasion_pct + Backfire_pct)
cat("\n=== Outcome prevalence among ALL observations (No shift included), % ===\n")
print(totals_all, digits = 3)

temp <- labeled %>%                                    # ‘labeled’ already has the four outcomes
  mutate(
    # individual outcome flags
    pos_backfire    = category == "Positive backfire",
    neg_backfire    = category == "Negative backfire",
    pos_persuasion  = category == "Positive persuasion",
    neg_persuasion  = category == "Negative persuasion",

    # combined flags
    pos = pos_backfire | pos_persuasion,   # any “helpful” shift
    neg = neg_backfire | neg_persuasion,    # any “harmful” shift

    backfire = pos_backfire | neg_backfire,
    persuasion = pos_persuasion | neg_persuasion
  )

temp <- temp %>%
  mutate(UID = factor(UID),               # <- critical
         NID = factor(NID))

fe_fit <- feglm(
  neg_persuasion ~ BiasedType + PrePerformance | NID,       # “| NID” adds headline fixed effects
  family  = binomial(),
  cluster = ~ UID,                       # cluster-robust VCOV at participant level
  data    = temp
)

summary(fe_fit)

tidy(fe_fit, conf.int = TRUE)

# Odds ratios for each partisan arm vs Default (pulled from the fit rather than
# hard-coded, since there are now two BiasedType coefficients instead of one).
or_table <- tidy(fe_fit, conf.int = TRUE) %>%
  dplyr::filter(grepl("BiasedType", term)) %>%
  mutate(
    OR       = exp(estimate),
    CI_lower = exp(conf.low),
    CI_upper = exp(conf.high)
  ) %>%
  dplyr::select(term, estimate, std.error, OR, CI_lower, CI_upper, p.value)

print(or_table, digits = 3)

