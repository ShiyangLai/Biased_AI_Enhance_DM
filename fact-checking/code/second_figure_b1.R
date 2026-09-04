# ==============================================================================
# second_figure_b1.R — AI bias MAGNITUDE (No Bias / Moderate / Strong) and
# outcomes. Analysis unchanged; visualization restyled to match the sibling
# investment script (bias_magnitude_outcomes.R): stacked square panels, line +
# nested 90/95/99 CI boxes, three-arm comparison in every panel (no
# direction split).
#
# Panels (top to bottom), with bar-middle markers:
#   1. Objective improvement  (PostPerformance, lmer)      green,  STAR
#   2. Conversation length    (ConvLength, a3-style lm+lmer) brown, TRIANGLE
#   3. Perceived improvement  (ordinal, MCMCglmm)           orange, RECT
# Meaningfulness stays as a console-only analysis (no panel).
# ==============================================================================
set.seed(123)

# ======================================
# Model specification using BiasedCat
# ======================================
# Create BiasedCat variable.
# NOTE: previously tested BiasedType == "Non-Biased", but BiasedType now holds
# Default/Republican/Democrat — that check silently sent Default rows to
# "Moderate Bias". Use AIStanceLabel(_S) directly.
make_biased_cat <- function(df) {
  factor(ifelse(df$AIStanceLabel_S == "Default", "No Bias",
                ifelse(df$AIStanceLabel %in% c("Strong Republican", "Strong Democrat"),
                       "Strong Bias", "Moderate Bias")),
         levels = c("No Bias", "Moderate Bias", "Strong Bias"))
}
single_ai_processed_$BiasedCat <- make_biased_cat(single_ai_processed_)

# `complete_data` historically lives in the session; make the script
# self-sufficient and guarantee BiasedCat is present and current.
if (!exists("complete_data")) {
  cat("complete_data not found - deriving from single_ai_processed_\n")
  complete_data <- single_ai_processed_
}
complete_data$BiasedCat <- make_biased_cat(complete_data)

# Check available levels in BiasedCat
cat("Unique values in BiasedCat:", paste(unique(single_ai_processed_$BiasedCat), collapse = ", "), "\n")
cat("N by bias magnitude:\n"); print(table(complete_data$BiasedCat))

# Create numeric version of AIInterMean (dplyr:: namespaced — car::recode masks it)
complete_data <- complete_data %>%
  mutate(AIInterMean_numeric = dplyr::recode(AIInterMean,
                                      "Not meaningful at all" = 1,
                                      "Slightly meaningful" = 2,
                                      "Moderately meaningful" = 3,
                                      "Very meaningful" = 4,
                                      "Extremely meaningful" = 5
  ))

# ------- OLS + Robust SD -------
model_real <- lm(PostPerformance ~ BiasedCat + as.factor(NID) + PrePerformance +
                   UIdeo + UStanceLabel + AICorrectness,
                 data = complete_data)

complete_data$PerceivedImproveCode <- factor(complete_data$PerceivedImproveCode, ordered = TRUE)

model_perceived <- clm(
  PerceivedImproveCode ~ BiasedCat + as.factor(NID) + PrePerformance +
    UIdeo + UStanceLabel + AICorrectness,
  data = complete_data, na.action = na.omit
)

complete_data$AIInterMean_numeric <- factor(complete_data$AIInterMean_numeric, ordered = TRUE)

model_meaningful <- clm(
  AIInterMean_numeric ~ BiasedCat + as.factor(NID) + PrePerformance + UIdeo +
    UStanceLabel + AICorrectness,
  data = complete_data, na.action = na.omit
)

# Conversation length (follows first_figure_single_a3_separated.R, with
# BiasedCat as the three-arm treatment). ConvLength must be on the model data.
conv_data <- if ("ConvLength" %in% names(complete_data)) complete_data else single_ai_processed_
if (!"ConvLength" %in% names(conv_data)) stop("ConvLength not found on complete_data or single_ai_processed_")

model_convlen_lm <- lm(ConvLength ~ BiasedCat + as.factor(NID) + PrePerformance +
                         UIdeo + UStanceLabel + AICorrectness,
                       data = conv_data)

# Calculate clustered standard errors for all models
vcov_real <- vcovCL(model_real, cluster = complete_data$UID)
vcov_perceived <- vcovCL(model_perceived, cluster = complete_data$UID)
vcov_meaningful <- vcovCL(model_meaningful, cluster = complete_data$UID)
vcov_convlen <- vcovCL(model_convlen_lm, cluster = conv_data$UID)

# Get coefficient tests with clustered SEs
clustered_results_real <- coeftest(model_real, vcov = vcov_real)
clustered_results_perceived <- coeftest(model_perceived, vcov = vcov_perceived)
clustered_results_meaningful <- coeftest(model_meaningful, vcov = vcov_meaningful)
clustered_results_convlen <- coeftest(model_convlen_lm, vcov = vcov_convlen)

cat("=== MODEL RESULTS ===\n")
cat("ACTUAL PERFORMANCE MODEL:\n")
print(clustered_results_real)
summary(model_real)

cat("\nCONVERSATION LENGTH MODEL (OLS + clustered SE):\n")
print(clustered_results_convlen)
summary(model_convlen_lm)
df.residual(model_convlen_lm)
performance::r2(model_convlen_lm)

cat("\nPERCEIVED IMPROVEMENT MODEL:\n")
print(clustered_results_perceived)
summary(model_perceived)
df.residual(model_perceived)
performance::r2(model_perceived)
link_name <- model_perceived$link
resid_sd  <- switch(link_name,
                    "logit"  = pi / sqrt(3),
                    "probit" = 1,
                    "cloglog" = 1,
                    "loglog"  = 1,
                    "cauchit" = 1)     # long tails, scale fixed to 1
resid_sd

cat("\nMEANINGFULNESS MODEL:\n")
print(clustered_results_meaningful)
summary(model_meaningful)
df.residual(model_meaningful)
performance::r2(model_meaningful)
link_name <- model_meaningful$link
resid_sd  <- switch(link_name,
                    "logit"  = pi / sqrt(3),
                    "probit" = 1,
                    "cloglog" = 1,
                    "loglog"  = 1,
                    "cauchit" = 1)     # long tails, scale fixed to 1
resid_sd

# Fit the models with BiasedCat using mixed effects
# Model 1: Real Performance (Linear Mixed Effects)
model_real <- lmer(PostPerformance ~ BiasedCat + as.factor(NID) + PrePerformance + (1|UID) +
                     UIdeo + UStanceLabel + AICorrectness,
                   data = complete_data)

# Model 1b: Conversation length (Linear Mixed Effects; a3's mixed spec)
model_convlen <- lmer(ConvLength ~ BiasedCat + PrePerformance + as.factor(NID) + (1|UID),
                      data = conv_data)

# MCMCglmm hard-errors on NA in ANY fixed predictor (not just AICorrectness —
# the data has NA UIdeo rows), so filter on the full predictor set. This is the
# same row set clm's na.action = na.omit uses for the identical RHS.
mcmc_predictors <- c("BiasedCat", "NID", "PrePerformance", "UIdeo",
                     "UStanceLabel", "AICorrectness", "UID")
mcmc_data <- complete_data[complete.cases(complete_data[, mcmc_predictors]), ]

# NOTE: default priors, as in the original analysis. Fixing the residual at 1
# (sibling-style) was tried and BLOWS UP here: with random = ~UID and only ~3
# obs per participant, the UID variance is weakly identified and the chain
# drifts to huge values, inflating the latent scale (means ~30, CIs +/-35).
# The sibling can fix R only because it has no random effect.

# Model 2: Perceived Improvement (Bayesian ordinal mixed models)
mcmc_data$PerceivedImproveCode <- factor(mcmc_data$PerceivedImproveCode, ordered = TRUE)
model_perceived <- MCMCglmm(PerceivedImproveCode ~ BiasedCat + as.factor(NID) + PrePerformance +
                              UIdeo + UStanceLabel + AICorrectness,
                            random = ~UID,
                            family = "ordinal",
                            nitt = 25000, thin = 10, burnin = 5000,
                            data = mcmc_data)

# Model 3: Meaningfulness (Bayesian ordinal mixed models)
mcmc_data$AIInterMean_numeric <- factor(mcmc_data$AIInterMean_numeric, ordered = TRUE)
model_meaningful <- MCMCglmm(AIInterMean_numeric ~ BiasedCat + as.factor(NID) + PrePerformance +
                               UIdeo + UStanceLabel + AICorrectness,
                             random = ~UID,
                             family = "ordinal",
                             nitt = 25000, thin = 10, burnin = 5000,
                             data = mcmc_data)

# Display results
cat("=== MIXED EFFECTS MODEL RESULTS ===\n")

cat("ACTUAL PERFORMANCE MODEL (Linear Mixed Effects):\n")
print(summary(model_real))
summary(model_real)$sigma
df.residual(model_real)
performance::r2(model_real)

cat("CONVERSATION LENGTH MODEL (Linear Mixed Effects):\n")
print(summary(model_convlen))
summary(model_convlen)$sigma
df.residual(model_convlen)
performance::r2(model_convlen)

cat("PERCEIVED IMPROVEMENT MODEL (Cumulative Link Mixed Model):\n")
print(summary(model_perceived))
se_fixed <- apply(model_perceived$Sol, 2, sd)
se_fixed
sqrt(mean(model_perceived$VCV[ , "units"]))   # posterior mean, not a single draw
X  <- model_perceived$X                 # design matrix for fixed effects
B  <- as.matrix(model_perceived$Sol)            # iterations × coefficients
eta <- X %*% t(B)
VF  <- apply(eta, 2, var)          # fixed-effect variance per draw
VR  <- model_perceived$VCV[ , "UID"]      # random-effect variance per draw
VE <- 1                                  # residual var. (probit link)
R2_marginal    <- VF / (VF + VR + VE)               # fixed only
R2_conditional <- (VF + VR) / (VF + VR + VE)        # fixed + random
cbind(
  Marginal    = c(mean(R2_marginal),
                  quantile(R2_marginal, c(.025, .5, .975))),
  Conditional = c(mean(R2_conditional),
                  quantile(R2_conditional, c(.025, .5, .975)))
)

cat("MEANINGFULNESS MODEL (Cumulative Link Mixed Model):\n")
print(summary(model_meaningful))
se_fixed <- apply(model_meaningful$Sol, 2, sd)
se_fixed
sqrt(mean(model_meaningful$VCV[ , "units"]))   # posterior mean, not a single draw
X  <- model_meaningful$X                 # design matrix for fixed effects
B  <- as.matrix(model_meaningful$Sol)            # iterations × coefficients
eta <- X %*% t(B)
VF  <- apply(eta, 2, var)          # fixed-effect variance per draw
VR  <- model_meaningful$VCV[ , "UID"]      # random-effect variance per draw
VE <- 1                                  # residual var. (probit link)
R2_marginal    <- VF / (VF + VR + VE)               # fixed only
R2_conditional <- (VF + VR) / (VF + VR + VE)        # fixed + random
cbind(
  Marginal    = c(mean(R2_marginal),
                  quantile(R2_marginal, c(.025, .5, .975))),
  Conditional = c(mean(R2_conditional),
                  quantile(R2_conditional, c(.025, .5, .975)))
)

# ======================================================
# Marginal means for BiasedCat and pairwise comparison
# ======================================================
# Function for marginal means, comparisons, and Hedge's g
get_model_results_with_hedges_g <- function(model, model_name, data = complete_data) {

  # Helper function for MCMCglmm posterior calculations.
  # Levels are intercept + BiasedCat effect + PrePerformance at its mean — the
  # original b1 construction (matches the established previous results). Other
  # covariates sit at reference/zero; they shift all arms equally, so pairwise
  # contrasts are identical either way.
  calc_mcmc_means <- function(posterior_samples, data) {
    intercept <- posterior_samples[, "(Intercept)"]
    preperf_effect <- if("PrePerformance" %in% colnames(posterior_samples)) {
      posterior_samples[, "PrePerformance"] * mean(data$PrePerformance, na.rm = TRUE)
    } else { 0 }

    # Calculate means for each bias level
    means <- list()
    means[["No Bias"]] <- intercept + preperf_effect

    for (level in c("Moderate Bias", "Strong Bias")) {
      coef_name <- paste0("BiasedCat", level)
      if (coef_name %in% colnames(posterior_samples)) {
        means[[level]] <- intercept + posterior_samples[, coef_name] + preperf_effect
      } else {
        means[[level]] <- means[["No Bias"]]  # If coefficient doesn't exist
      }
    }
    return(means)
  }

  # Hedge's g correction factor
  hedges_correction <- function(n1, n2) {
    df <- n1 + n2 - 2
    if (df > 0) {
      return(1 - (3 / (4 * df - 1)))
    } else {
      return(1)  # Fallback if df calculation fails
    }
  }

  # Calculate sample sizes for each BiasedCat level
  n_by_group <- table(data$BiasedCat)

  # Calculate marginal means and comparisons based on model type
  if (class(model)[1] == "MCMCglmm") {
    # MCMCglmm approach
    posterior_samples <- model$Sol
    means <- calc_mcmc_means(posterior_samples, data)

    # Use appropriate pooled SD for ordinal models
    if (model$family[1] == "ordinal") {
      # For ordinal models, use empirical SD from observed data
      response_var <- all.vars(model$Fixed$formula)[1]
      observed_values <- as.numeric(data[[response_var]])
      pooled_sd <- sd(observed_values, na.rm = TRUE)
      pooled_sd_samples <- rep(pooled_sd, nrow(posterior_samples))
      cat("Using empirical pooled SD for ordinal model:", round(pooled_sd, 4), "\n")
    } else {
      # For continuous models, use residual variance
      residual_var_samples <- model$VCV[, "units"]
      pooled_sd_samples <- sqrt(residual_var_samples)
      pooled_sd <- mean(pooled_sd_samples)
      cat("Using model residual SD for continuous model:", round(pooled_sd, 4), "\n")
    }

    mean_pooled_sd <- mean(pooled_sd_samples)

    # Marginal means (posterior mean + SD kept for sibling-style viz)
    marginal_means <- data.frame(
      BiasedCat = names(means),
      Mean = sapply(means, mean),
      SD = sapply(means, sd),
      Lower_CI = sapply(means, function(x) quantile(x, 0.025)),
      Upper_CI = sapply(means, function(x) quantile(x, 0.975)),
      row.names = NULL
    )

    # Pairwise comparisons with Hedge's g
    comparisons <- data.frame(
      Contrast = character(0),
      Estimate = numeric(0),
      Lower_CI = numeric(0),
      Upper_CI = numeric(0),
      Prob_Greater_Zero = numeric(0),
      Hedges_g = numeric(0),
      Hedges_g_Lower = numeric(0),
      Hedges_g_Upper = numeric(0)
    )

    # All pairwise differences
    levels <- names(means)
    for (i in 1:(length(levels)-1)) {
      for (j in (i+1):length(levels)) {
        diff_samples <- means[[j]] - means[[i]]

        # Calculate Hedge's g for each posterior sample
        hedges_g_samples <- diff_samples / pooled_sd_samples

        # Apply Hedge's correction
        n1 <- n_by_group[levels[i]]
        n2 <- n_by_group[levels[j]]
        correction <- hedges_correction(n1, n2)
        hedges_g_samples <- hedges_g_samples * correction

        comparisons <- rbind(comparisons, data.frame(
          Contrast = paste(levels[j], "-", levels[i]),
          Estimate = mean(diff_samples),
          Lower_CI = quantile(diff_samples, 0.025),
          Upper_CI = quantile(diff_samples, 0.975),
          Prob_Greater_Zero = mean(diff_samples > 0),
          Hedges_g = mean(hedges_g_samples),
          Hedges_g_Lower = quantile(hedges_g_samples, 0.025),
          Hedges_g_Upper = quantile(hedges_g_samples, 0.975)
        ))
      }
    }

    # Apply FDR correction instead of Bonferroni
    n_comparisons <- nrow(comparisons)
    # For Bayesian, convert "probability of difference > 0" to two-tailed p-value equivalent
    p_values_equiv <- 2 * pmin(comparisons$Prob_Greater_Zero, 1 - comparisons$Prob_Greater_Zero)

    # Apply Benjamini-Hochberg FDR correction
    comparisons$FDR_adjusted_p <- p.adjust(p_values_equiv, method = "fdr")

  } else {
    # Standard emmeans approach for lmer/clmm models
    emm <- emmeans(model, ~ BiasedCat)
    pairs_result <- pairs(emm, adjust = "fdr")

    # Extract marginal means
    marginal_means <- as.data.frame(emm)

    # Extract pairwise comparisons
    comparisons <- as.data.frame(pairs_result)

    # Calculate Hedge's g for standard models
    if (class(model)[1] %in% c("lmerMod", "lmerModLmerTest")) {
      # For lmer models, get residual standard deviation
      pooled_sd <- sigma(model)  # Residual standard deviation

      # Add Hedge's g to comparisons
      comparisons$Hedges_g <- numeric(nrow(comparisons))
      comparisons$Hedges_g_Lower <- numeric(nrow(comparisons))
      comparisons$Hedges_g_Upper <- numeric(nrow(comparisons))

      for (i in 1:nrow(comparisons)) {
        # Extract group names from contrast
        contrast_parts <- strsplit(as.character(comparisons$contrast[i]), " - ")[[1]]
        group1 <- trimws(contrast_parts[1])
        group2 <- trimws(contrast_parts[2])

        # Get sample sizes
        n1 <- n_by_group[group1]
        n2 <- n_by_group[group2]

        # Calculate Hedge's g
        cohens_d <- comparisons$estimate[i] / pooled_sd
        correction <- hedges_correction(n1, n2)
        hedges_g <- cohens_d * correction

        # Calculate confidence interval for Hedge's g
        se_hedges_g <- sqrt((n1 + n2)/(n1 * n2) + hedges_g^2/(2 * (n1 + n2 - 2)))

        comparisons$Hedges_g[i] <- hedges_g
        comparisons$Hedges_g_Lower[i] <- hedges_g - 1.96 * se_hedges_g
        comparisons$Hedges_g_Upper[i] <- hedges_g + 1.96 * se_hedges_g
      }
    } else {
      # For other model types, indicate that Hedge's g calculation is not implemented
      comparisons$Hedges_g <- NA
      comparisons$Hedges_g_Lower <- NA
      comparisons$Hedges_g_Upper <- NA
    }
  }

  return(list(
    model_name = model_name,
    marginal_means = marginal_means,
    comparisons = comparisons,
    pooled_sd = if(exists("mean_pooled_sd")) mean_pooled_sd else if(exists("pooled_sd")) pooled_sd else NA
  ))
}

# Apply to all models and store results with Hedge's g
results_with_hedges <- list()

# Process each model. `data` must be the frame each model was FIT on (drives
# group Ns for the Hedges correction and the ordinal pooled SD) — the MCMC
# models use mcmc_data, not complete_data.
models <- list(
  "real" = list(model = model_real, name = "Actual Performance", data = complete_data),
  "convlen" = list(model = model_convlen, name = "Conversation Length", data = conv_data),
  "perceived" = list(model = model_perceived, name = "Perceived Improvement", data = mcmc_data),
  "meaningful" = list(model = model_meaningful, name = "Meaningfulness", data = mcmc_data)
)

for (model_key in names(models)) {
  results_with_hedges[[model_key]] <- get_model_results_with_hedges_g(
    models[[model_key]]$model, models[[model_key]]$name, data = models[[model_key]]$data)
}

# Display results with Hedge's g
cat("=== MARGINAL MEANS BY BiasedCat ===\n")
for (model_key in names(results_with_hedges)) {
  cat("\n", toupper(results_with_hedges[[model_key]]$model_name), ":\n")
  print(results_with_hedges[[model_key]]$marginal_means)
}

cat("\n=== PAIRWISE COMPARISONS WITH HEDGE'S G EFFECT SIZES (FDR corrected) ===\n")
for (model_key in names(results_with_hedges)) {
  cat("\n", toupper(results_with_hedges[[model_key]]$model_name), ":\n")
  result_df <- results_with_hedges[[model_key]]$comparisons

  # Format for better display
  if ("Hedges_g" %in% colnames(result_df)) {
    # Round Hedge's g values for cleaner display
    result_df$Hedges_g <- round(result_df$Hedges_g, 3)
    result_df$Hedges_g_Lower <- round(result_df$Hedges_g_Lower, 3)
    result_df$Hedges_g_Upper <- round(result_df$Hedges_g_Upper, 3)
  }

  print(result_df)

  # Show pooled SD used for calculation
  if (!is.na(results_with_hedges[[model_key]]$pooled_sd)) {
    cat("  Pooled SD used:", round(results_with_hedges[[model_key]]$pooled_sd, 3), "\n")
  }
}

# =================================
# Mechanism test: does reduced PERCEIVED improvement mediate the performance gain?
# =================================
# Motivating claim: "biased AI benefits BY reducing perceived helpfulness."
# Treatment = bias magnitude (0 = No Bias, 1 = Moderate, 2 = Strong);
# mediator = PerceivedImproveCode; outcome = PostPerformance.
# RESULT (current data): the a-path is significant (bias lowers perceived
# improvement) but the b-path is NOT (perceived improvement does not predict
# performance), so ACME ~ 0 and the total effect is essentially all direct.
# => The data support a DISSOCIATION, not mediation. Do not claim the perception
#    drop causes the performance gain.
# CAVEAT: mediator and outcome are both measured post-interaction, so this is a
# correlational mediation; it can refute but not establish a causal pathway.
med_dat <- complete_data
med_dat$BiasMag <- as.numeric(factor(med_dat$BiasedCat,
                    levels = c("No Bias", "Moderate Bias", "Strong Bias"))) - 1
med_dat$PI <- as.numeric(as.character(med_dat$PerceivedImproveCode))
med_dat$NIDf <- as.factor(med_dat$NID)
med_cols <- c("BiasMag", "PI", "NIDf", "PrePerformance", "UIdeo", "UStanceLabel",
              "PostPerformance", "UID")
med_dat <- med_dat[complete.cases(med_dat[, med_cols]), ]

med_mediator <- lm(PI ~ BiasMag + NIDf + PrePerformance + UIdeo + UStanceLabel,
                   data = med_dat)
med_outcome  <- lm(PostPerformance ~ BiasMag + PI + NIDf + PrePerformance + UIdeo +
                     UStanceLabel, data = med_dat)
cat("\n=== Mediation paths (bias magnitude -> perceived improvement -> performance) ===\n")
cat("a path (treatment -> mediator):\n");  print(coef(summary(med_mediator))["BiasMag", ])
cat("b path (mediator -> outcome | T):\n"); print(coef(summary(med_outcome))["PI", ])
cat("c' direct (treatment -> outcome):\n"); print(coef(summary(med_outcome))["BiasMag", ])

set.seed(123)
med_fit <- mediate(med_mediator, med_outcome, treat = "BiasMag", mediator = "PI",
                   boot = FALSE, sims = 1000,
                   control.value = 0, treat.value = 2,   # No Bias -> Strong Bias
                   cluster = med_dat$UID)
cat("\n=== Causal mediation: No Bias -> Strong Bias (UID-clustered) ===\n")
print(summary(med_fit))

# =================================
# Prepare data for visualization (sibling style: normal-approx 90/95/99 CIs)
# =================================
LV <- c("No Bias", "Moderate Bias", "Strong Bias")

# multi-level CI helper (90/95/99), as in bias_magnitude_outcomes.R
add_cis_norm <- function(mean, se) data.frame(
  lo90 = mean - se * qnorm(.95),  hi90 = mean + se * qnorm(.95),
  lo95 = mean - se * qnorm(.975), hi95 = mean + se * qnorm(.975),
  lo99 = mean - se * qnorm(.995), hi99 = mean + se * qnorm(.995))

# emmeans-based viz (lmer panels): mean +/- z * SE
viz_from_emm <- function(mm) {
  data.frame(BiasedCat = as.character(mm$BiasedCat), emmean = mm$emmean,
             add_cis_norm(mm$emmean, mm$SE))
}
# MCMCglmm-based viz: posterior mean +/- z * posterior SD (normal approximation:
# symmetric, so the plotted marker sits exactly at the box center, matching the
# lm/lmer panels' construction; pairwise tests above remain exact-posterior)
viz_from_mcmc <- function(mm) {
  data.frame(BiasedCat = as.character(mm$BiasedCat), emmean = mm$Mean,
             add_cis_norm(mm$Mean, mm$SD))
}

viz_real    <- viz_from_emm(results_with_hedges$real$marginal_means)
viz_convlen <- viz_from_emm(results_with_hedges$convlen$marginal_means)
viz_perc    <- viz_from_mcmc(results_with_hedges$perceived$marginal_means)

cat("\n=== VISUALIZATION DATA (emmean and 95% band) ===\n")
for (nm in c("viz_real", "viz_convlen", "viz_perc")) {
  cat("\n", nm, ":\n"); print(get(nm)[, c("BiasedCat", "emmean", "lo95", "hi95")], digits = 4)
}

# ==================================
# Visualization setup (strictly follows bias_magnitude_outcomes.R)
# ==================================
# Panel 1 — objective improvement (green)
color_actual <- "#006400"      # Dark green
color_actual_90 <- "#228B22"   # Forest green
color_actual_95 <- "#66C266"   # Light forest green
color_actual_99 <- "#BBE5BB"   # Soft mint green

# Panel 2 — conversation length (brown)
color_convlen <- "#654321"    # Dark brown
color_convlen_90 <- "#8B4513" # Saddle brown
color_convlen_95 <- "#A0522D" # Sienna
color_convlen_99 <- "#D2B48C" # Tan

# Panel 3 — perceived improvement (orange)
color_perceived <- "#D2691E"    # Chocolate (dark orange-brown)
color_perceived_90 <- "#FF8C00" # Dark orange
color_perceived_95 <- "#FFA500" # Orange
color_perceived_99 <- "#FFB347" # Peach (light but visible orange)

BW <- 0.10; bw90 <- BW*1.4; bw95 <- BW*1.0; bw99 <- BW*0.7
prep <- function(v) { v$x <- match(v$BiasedCat, LV) - 1; v }        # 0/1/2

# exact five-pointed star polygon centered on (cx, cy); rx/ry in data units
mk_star <- function(cx, cy, rx, ry, id) {
  ao <- pi/2 + 2*pi*(0:4)/5; ai <- pi/2 + pi/5 + 2*pi*(0:4)/5
  a  <- as.vector(rbind(ao, ai)); r <- rep(c(1, 0.382), 5)
  data.frame(x = cx + cos(a)*rx*r, y = cy + sin(a)*ry*r, g = id)
}

panel <- function(v, dark, c90, c95, c99, ylab, shp, acc = 0.01, show_x = TRUE, ytit_r = 8, ybr = waiver(), ymul = 1,
                  ylim = NULL) {
  # ylim: optional c(lower, upper) for the y-axis. Applied via coord_cartesian,
  # so out-of-range box portions are clipped visually, not dropped. NULL keeps
  # the automatic range (data span + 5%/30% expansion).
  v <- prep(v)
  p <- ggplot(v) +
    geom_rect(aes(xmin = x - bw99, xmax = x + bw99, ymin = lo99, ymax = hi99), fill = c99) +
    geom_rect(aes(xmin = x - bw95, xmax = x + bw95, ymin = lo95, ymax = hi95), fill = c95) +
    geom_rect(aes(xmin = x - bw90, xmax = x + bw90, ymin = lo90, ymax = hi90), fill = c90) +
    geom_line(aes(x = x, y = emmean), color = dark, linewidth = 1.1)
  if (identical(shp, "star")) {                 # polygon star: exactly centered
    # square panel (aspect.ratio = 1): x width = 2.95 units; y = displayed range
    # x 1.35 (expansion 5% bottom + 30% top) -> regular star, no distortion.
    # With a manual ylim, the displayed range is diff(ylim) instead of the data span.
    yspan <- (if (is.null(ylim)) diff(range(c(v$lo99, v$hi99))) else diff(ylim)) * 1.35
    rx <- 0.078; ry <- rx * yspan / 2.95
    stars <- do.call(rbind, lapply(seq_len(nrow(v)),
               function(i) mk_star(v$x[i], v$emmean[i], rx, ry, i)))
    p <- p + geom_polygon(data = stars, aes(x = x, y = y, group = g),
                          fill = dark, color = NA)
  } else {
    p <- p + geom_point(aes(x = x, y = emmean), color = dark, shape = shp, size = 2.2)
  }
  if (!is.null(ylim)) p <- p + coord_cartesian(ylim = ylim)
  p <- p +
    scale_x_continuous(breaks = 0:2, labels = c("Default", "Moderate Bias", "Strong Bias"), limits = c(-0.35, 2.6),
                       name = "AI Bias Magnitude") +
    scale_y_continuous(name = ylab,
                       labels = function(y) scales::number_format(accuracy = acc)(y * ymul),
                       breaks = ybr,
                       expand = expansion(mult = c(0.05, 0.30))) +  # top headroom
    theme_classic() +
    theme(aspect.ratio = 1,                                # square panel frame
          text = element_text(family = "Avenir", color = "black"),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
          axis.line = element_blank(),
          axis.title.x = element_text(family = "Avenir", size = 9, margin = margin(t = 8)),
          axis.title.y = element_text(family = "Avenir", size = 9, margin = margin(r = ytit_r)),
          axis.text = element_text(family = "Avenir", size = 8, color = "black"),
          axis.ticks = element_line(color = "black", linewidth = 0.4),
          axis.ticks.length = unit(2.5, "pt"), panel.grid = element_blank(),
          plot.margin = margin(t = 10, r = 15, b = 6, l = 10))
  if (!show_x) p <- p + theme(axis.title.x = element_blank(),
                              axis.text.x  = element_blank(),
                              axis.ticks.x = element_blank())
  p
}

# ==================================
# Create plot — markers: star (objective) / triangle (conv length) / rect (perceived)
# ==================================
# Uniform ytit_r so the three y-axis titles sit on the same vertical line
# (patchwork aligns the panel frames; unequal title margins would offset them).
p1 <- panel(viz_real, color_actual, color_actual_90, color_actual_95, color_actual_99,
            "Post-Interaction Performance", "star",
            acc = 0.01, show_x = FALSE, ytit_r = 12, ylim = c(0.55, 0.75))                  # five-pointed star
p2 <- panel(viz_convlen, color_convlen, color_convlen_90, color_convlen_95, color_convlen_99,
            "Conversation Length", 17,
            acc = 0.1, show_x = FALSE, ytit_r = 12, ylim = c(12, 33))                   # solid triangle
p3 <- panel(viz_perc, color_perceived, color_perceived_90, color_perceived_95, color_perceived_99,
            "Perceived Improvement", 15,
            acc = 0.01, ytit_r = 8, ylim = c(-3, 10))                                  # solid rect

# Combine all three plots vertically
p_combined <- p1 / p2 / p3

# Display the combined plot
print(p_combined)

ggsave("../figures/second_figure_b1.png", p_combined, width = 4, height = 8, dpi = 500)
cat("\nSaved second_figure_b1.png in", getwd(), "\n")
