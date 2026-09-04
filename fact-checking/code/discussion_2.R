# BiasedType is redefined by several upstream scripts (the c-series leaves it as
# Default/Republican/Democrat). This analysis contrasts biased against
# non-biased assistants and cohens_d() requires exactly two levels, so define
# the binary version locally rather than inheriting session state.
single_ai_processed_$BiasedType <- factor(
  ifelse(single_ai_processed_$AIStanceLabel_S == "Default", "Non-Biased", "Biased"),
  levels = c("Non-Biased", "Biased"))

single_ai_processed_$OneRoundConv <- ifelse(single_ai_processed_$ConvRound == 1, 1, 0)

conv_round_model <- lm(ConvRound ~ BiasedType,
                         data = single_ai_processed_, na.action = na.omit)
summary(conv_round_model)

emm <- emmeans(conv_round_model, ~ BiasedType)
contrasts <- pairs(emm, infer = TRUE)
print(contrasts)

report <- cohens_d(ConvRound ~ BiasedType,
         data = single_ai_processed_,
         hedges.correction = TRUE)

temp1 <- single_ai_processed_[single_ai_processed_$BiasedType == "Biased", ]
temp2 <- single_ai_processed_[single_ai_processed_$BiasedType == "Non-Biased", ]

temp1$PreEvaCodeConf <- ifelse(temp1$PreEvaCode == -1, 
                               (1 - temp1$PreConfCode) * 0.5,
                               ifelse(temp1$PreEvaCode == 1, 
                                      0.5 + temp1$PreConfCode * 0.5, 
                                      0.5))
temp1$PostEvaCodeConf <- ifelse(temp1$PostEvaCode == -1, 
                                (1 - temp1$PostConfCode) * 0.5,
                                ifelse(temp1$PostEvaCode == 1, 
                                       0.5 + temp1$PostConfCode * 0.5, 
                                       0.5))

temp2$PreEvaCodeConf <- ifelse(temp2$PreEvaCode == -1, 
                               (1 - temp2$PreConfCode) * 0.5,
                               ifelse(temp2$PreEvaCode == 1, 
                                      0.5 + temp2$PreConfCode * 0.5, 
                                      0.5))
temp2$PostEvaCodeConf <- ifelse(temp2$PostEvaCode == -1, 
                                (1 - temp2$PostConfCode) * 0.5,
                                ifelse(temp2$PostEvaCode == 1, 
                                       0.5 + temp2$PostConfCode * 0.5, 
                                       0.5))
temp1$Convergence <- abs(temp1$PreEvaCodeConf - temp1$AIEva) - abs(temp1$PostEvaCodeConf - temp1$AIEva)
temp2$Convergence <- abs(temp2$PreEvaCodeConf - temp2$AIEva) - abs(temp2$PostEvaCodeConf - temp2$AIEva)

t.test(temp1$Convergence, temp2$Convergence)

report <- cohens_d(temp1$Convergence, temp2$Convergence,
         hedges.correction = TRUE)   # returns Hedges' g with CI

