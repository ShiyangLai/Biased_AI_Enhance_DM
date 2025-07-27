single_ai_processed$AnswerChanged <- ifelse(single_ai_processed$PreEva != single_ai_processed$PostEva,
                                            1, 0)

single_ai_processed <- single_ai_processed %>%
  mutate(AIEva_cat = case_when(
    AIEva <= 0.33 ~ -1,
    AIEva <= 0.66 ~ 0,
    TRUE          ~ 1
  ))

single_ai_processed <- single_ai_processed %>%
  mutate(PostEva_cat = ((PostEvaCode + 1) / 2) * PostConfCode)

single_ai_processed <- single_ai_processed %>%
  mutate(PreEva_cat = ((PreEvaCode + 1) / 2) * PreConfCode)

single_ai_processed$AIHumanAgree <- ifelse(single_ai_processed$PostEvaCode == single_ai_processed$AIEva_cat, 1, 0)

single_ai_processed$AIHumanAgreePre <- ifelse(single_ai_processed$PreEvaCode == single_ai_processed$AIEva_cat, 1, 0)
single_ai_processed$AIHumanGAPPre <- abs(single_ai_processed$PreEva_cat - single_ai_processed$AIEva)

single_ai_processed$PrePostGAP <- abs(single_ai_processed$PostEva_cat - single_ai_processed$PreEva_cat)

single_ai_processed <- single_ai_processed %>%
  mutate(GAP_change = AIHumanGAPPre - AIHumanGAP)

model_change <- lm(AIHumanAgree ~ AIHumanAgreePre + as.factor(AIStanceLabel)*as.factor(NID) + PreConfCode,
                   data = single_ai_processed)

# Calculate clustered standard errors
vcov_ai <- vcovCL(model_change, cluster = single_ai_processed$UID)

# Get coefficient tests with clustered SEs
clustered_results_ai <- coeftest(model_change, vcov = vcov_ai)

cat("=== AI CORRECTNESS MODEL RESULTS ===\n")
print(clustered_results_ai)

# =====================================
# Marginal means and aggregation
# =====================================
# Get marginal means for each AIStanceLabel level
emm_ai <- emmeans(model_change, ~ AIStanceLabel, vcov. = vcov_ai)

cat("\n=== MARGINAL MEANS BY AIStanceLabel ===\n")
cat("AI CORRECTNESS:\n")
print(emm_ai)

# Aggregation into bias magnitude categories
# Use the same bias contrast function from before
stance_levels <- levels(complete_data$AIStanceLabel)
cat("\nAvailable AIStanceLabel levels:", paste(stance_levels, collapse = ", "), "\n")

# Create contrast weights for aggregation
create_bias_contrasts <- function(levels) {
  # Find which levels correspond to each bias category
  no_bias_levels <- levels[grepl("Neutral|Default", levels, ignore.case = TRUE)]
  moderate_levels <- levels[grepl("Somewhat", levels, ignore.case = TRUE)]
  strong_levels <- levels[grepl("Strong", levels, ignore.case = TRUE)]
  
  cat("Detected level groupings:\n")
  cat("No Bias:", paste(no_bias_levels, collapse = ", "), "\n")
  cat("Moderate Bias:", paste(moderate_levels, collapse = ", "), "\n")
  cat("Strong Bias:", paste(strong_levels, collapse = ", "), "\n")
  
  # Create contrast vectors for each bias magnitude
  contrasts_list <- list()
  
  # No Bias contrast (average of neutral/default levels)
  if(length(no_bias_levels) > 0) {
    no_bias_contrast <- rep(0, length(levels))
    names(no_bias_contrast) <- levels
    no_bias_contrast[no_bias_levels] <- 1 / length(no_bias_levels)
    contrasts_list[["No_Bias"]] <- no_bias_contrast
  }
  
  # Moderate Bias contrast (average of somewhat levels)
  if(length(moderate_levels) > 0) {
    moderate_contrast <- rep(0, length(levels))
    names(moderate_contrast) <- levels
    moderate_contrast[moderate_levels] <- 1 / length(moderate_levels)
    contrasts_list[["Moderate_Bias"]] <- moderate_contrast
  }
  
  # Strong Bias contrast (average of strong levels)
  if(length(strong_levels) > 0) {
    strong_contrast <- rep(0, length(levels))
    names(strong_contrast) <- levels
    strong_contrast[strong_levels] <- 1 / length(strong_levels)
    contrasts_list[["Strong_Bias"]] <- strong_contrast
  }
  
  return(contrasts_list)
}

# Create contrast list
bias_contrasts <- create_bias_contrasts(stance_levels)
cat("\n=== BIAS MAGNITUDE CONTRASTS ===\n")
print(bias_contrasts)

# Apply contrasts to get aggregated estimates
bias_ai <- contrast(emm_ai, method = bias_contrasts)

cat("\n=== AGGREGATED AI CORRECTNESS BY BIAS MAGNITUDE ===\n")
print(bias_ai)

# Pairwise comparisons between bias magnitude categories
pairwise_ai <- pairs(bias_ai, adjust = "bonferroni")

cat("\n=== PAIRWISE COMPARISONS (BONFERRONI ADJUSTED) ===\n")
cat("AI CORRECTNESS:\n")
print(pairwise_ai)


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

