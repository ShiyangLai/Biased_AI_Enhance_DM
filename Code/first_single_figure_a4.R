tau <- 0.1            # minimum change to count as persuasion/backfire

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

## Proportion of each outcome in Biased vs Non-Biased groups
props <- labeled %>%
  filter(category != "No shift") %>%      # focus on the four substantive outcomes
  count(BiasedType, category) %>%
  group_by(BiasedType) %>%
  mutate(prop = n / sum(n)) %>%
  pivot_wider(names_from = category,
              values_from = prop,
              values_fill = 0)

print(props, digits = 3)

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

beta <- -0.589
se <- 0.180
OR <- exp(beta)
CI_lower <- exp(beta-1.96*se)
CI_upper <- exp(beta+1.96*se)

OR
CI_lower
CI_upper
