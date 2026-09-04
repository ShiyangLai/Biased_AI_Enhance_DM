library(dplyr)
library(ggplot2)
library(tidyr)
library(broom)
library(scales)
library(emmeans)
library(stringr)
library(sandwich)
library(lmtest)
library(cowplot)
library(ordinal)
library(boot)
library(mediation)
library(patchwork)
library(lme4)
library(car)
library(effectsize)
library(performance)
library(broom.mixed)
library(lmtest)
library(estimatr)
library(rcompanion)
library(clubSandwich) 
library(fixest)
library(MCMCglmm)
library(Matrix)
library(ordinal)
library(tableone)
library(cobalt)
library(stargazer)
library(knitr)
library(RColorBrewer)
library(ggdist)
library(ggbeeswarm)
library(ggeffects)

normalize_truth_code <- function(df) {
  df %>%
    mutate(NormalizedTruthCode = (TruthCode + 1) / 2)
}

# Calculate adjusted evaluations and errors
calculate_adjusted_evaluations <- function(df) {
  df %>%
    # Pre evaluation adjustment
    mutate(
      AdjustedPreEva = ifelse(PreEvaCode == -1, 
                              (1 - PreConfCode) * 0.5,
                              ifelse(PreEvaCode == 1, 
                                     0.5 + PreConfCode * 0.5, 
                                     0.5)),
      # Calculate Pre Error
      HumanPreError = abs(NormalizedTruthCode - AdjustedPreEva),
      
      # Post evaluation adjustment
      AdjustedPostEva = ifelse(PostEvaCode == -1, 
                               (1 - PostConfCode) * 0.5,
                               ifelse(PostEvaCode == 1, 
                                      0.5 + PostConfCode * 0.5, 
                                      0.5)),
      # Calculate Post Error
      HumanPostError = abs(NormalizedTruthCode - AdjustedPostEva),
      
      # Performance metrics
      PrePerformance = 1 - HumanPreError,  
      PostPerformance = 1 - HumanPostError,
      PerformanceChange = PostPerformance - PrePerformance
    )
}

# Function to parse political stances
parse_stance <- function(df) {
  df %>%
    mutate(
      # Convert to simplified stance codes
      AIStanceCode = case_when(
        AIStanceLabel_S == "Republican" ~ 1,
        AIStanceLabel_S == "Neutral" ~ 0,
        AIStanceLabel_S == "Default" ~ 0,
        AIStanceLabel_S == "Democrat" ~ -1,
        TRUE ~ NA_real_
      ),
      
      # Convert user stance to code using UStanceLabel_S
      UStanceCode = case_when(
        UStanceLabel_S == "Republican" ~ 1,
        UStanceLabel_S == "Independent" ~ 0,
        UStanceLabel_S == "Democrat" ~ -1,
        TRUE ~ NA_real_
      )
    )
}

# Function to parse dual AI stance labels
parse_dual_stance <- function(df) {
  df %>%
    mutate(
      # Extract AI1 and AI2 stance labels using regex from AIStanceLabel_S
      AI1StanceLabel_S = str_extract(AIStanceLabel_S, "(?<=\\(').*?(?=', ')"),
      AI2StanceLabel_S = str_extract(AIStanceLabel_S, "(?<=', ').*?(?=')"),
      
      # Convert to simplified stance codes
      AI1StanceCode = case_when(
        AI1StanceLabel_S == "Republican" ~ 1,
        AI1StanceLabel_S == "Neutral" ~ 0,
        AI1StanceLabel_S == "Default" ~ 0,
        AI1StanceLabel_S == "Democrat" ~ -1,
        TRUE ~ NA_real_
      ),
      AI2StanceCode = case_when(
        AI2StanceLabel_S == "Republican" ~ 1,
        AI2StanceLabel_S == "Neutral" ~ 0,
        AI2StanceLabel_S == "Default" ~ 0,
        AI2StanceLabel_S == "Democrat" ~ -1,
        TRUE ~ NA_real_
      ),
      # Convert user stance to code using UStanceLabel_S
      UStanceCode = case_when(
        UStanceLabel_S == "Republican" ~ 1,
        UStanceLabel_S == "Independent" ~ 0,
        UStanceLabel_S == "Democrat" ~ -1,
        TRUE ~ NA_real_
      )
    )
}

# Process the single AI data
process_single_ai_data <- function(df) {
  # First apply the normalization and error calculations
  df_processed <- df %>%
    normalize_truth_code() %>%
    calculate_adjusted_evaluations() %>%
    parse_stance()
  
  # Create the baseline for Default AI cases
  default_cases <- df_processed %>%
    dplyr::filter(AIStanceLabel_S %in% c("Default")) %>%
    mutate(StanceRelationship = "Baseline")
  
  # Categorize non-Default cases based on stance relationships
  non_default_cases <- df_processed %>%
    dplyr::filter(AIStanceLabel_S != "Default") %>%
    mutate(
      # Calculate stance relationships
      StanceDistance = abs(UStanceCode - AIStanceCode),
      StanceRelationship = case_when(
        StanceDistance == 0 ~ "Echo Chamber",
        StanceDistance == 2 ~ "Strong Opposition", 
        StanceDistance == 1 ~ "Moderate Opposition",
        TRUE ~ "Other"
      )
    )
  
  # Combine into a single dataset
  combined_data <- bind_rows(
    default_cases,
    non_default_cases
  )
  
  # Create abbreviated relationship names
  relationship_abbr <- c(
    "Baseline" = "BL",
    "Echo Chamber" = "EC",
    "Moderate Opposition" = "MO",
    "Strong Opposition" = "SO"
  )
  
  # Add abbreviated names and return
  combined_data %>%
    mutate(StanceRelationship_Abbr = relationship_abbr[StanceRelationship])
}

# Process the dual AI data
process_dual_ai_data <- function(df) {
  # First apply the normalization and error calculations
  df_processed <- df %>%
    parse_dual_stance() %>%
    normalize_truth_code() %>%
    calculate_adjusted_evaluations()
  
  # Create the baseline for Default-Default AI cases
  default_default_cases <- df_processed %>%
    dplyr::filter(AI1StanceLabel_S == "Default" & AI2StanceLabel_S == "Default") %>%
    mutate(StanceRelationship = "Baseline")
  
  # Categorize non-Default-Default cases based on stance relationships
  non_default_cases <- df_processed %>%
    dplyr::filter(!(AI1StanceLabel_S == "Default" & AI2StanceLabel_S == "Default")) %>%
    mutate(
      # Distance between user and each AI
      StanceDistance_AI1 = abs(UStanceCode - AI1StanceCode),
      StanceDistance_AI2 = abs(UStanceCode - AI2StanceCode),
      
      # Calculate AI-AI distance
      AI_AI_Distance = abs(AI1StanceCode - AI2StanceCode),
      
      # Categorize the dual AI relationship
      StanceRelationship = case_when(
        # Echo Chamber: both AIs close to user and to each other
        (StanceDistance_AI1 + StanceDistance_AI2) == 0 ~ "Echo Chamber",
        
        # Balanced Perspective: AIs on opposite sides of user (user in middle)
        (AI1StanceCode < UStanceCode & UStanceCode < AI2StanceCode) | 
          (AI2StanceCode < UStanceCode & UStanceCode < AI1StanceCode) ~ "Balanced Perspective",
        
        # Cross-Pressuring: AIs on different sides relative to each other but not user between
        ((AI1StanceCode < AI2StanceCode & !(AI1StanceCode < UStanceCode & UStanceCode < AI2StanceCode)) |
           (AI2StanceCode < AI1StanceCode & !(AI2StanceCode < UStanceCode & UStanceCode < AI1StanceCode))) &
          AI_AI_Distance >= 1 ~ "Cross-Pressuring",
        
        # Strong Skewed Opposition: both AIs on same side, strongly opposing user
        ((AI1StanceCode > UStanceCode & AI2StanceCode > UStanceCode) | 
           (AI1StanceCode < UStanceCode & AI2StanceCode < UStanceCode)) &
          (StanceDistance_AI1 >= 2 & StanceDistance_AI2 >= 2) ~ "Strong Opposition",
        
        # Moderate Skewed Opposition: both AIs on same side, moderately opposing user
        ((AI1StanceCode > UStanceCode & AI2StanceCode > UStanceCode) | 
           (AI1StanceCode < UStanceCode & AI2StanceCode < UStanceCode)) ~ "Moderate Opposition",
        
        # Fallback - should capture any remaining cases
        TRUE ~ "Mixed Alignment"
      )
    )
  
  # Combine all cases
  combined_data <- bind_rows(
    default_default_cases,
    non_default_cases
  )
  
  # Create abbreviated relationship names
  relationship_abbr <- c(
    "Baseline" = "BL",
    "Echo Chamber" = "EC",
    "Balanced Perspective" = "BP",
    "Cross-Pressuring" = "CP",
    "Moderate Opposition" = "MO",
    "Strong Opposition" = "SO"
  )
  
  # Add abbreviated names and return
  combined_data %>%
    mutate(StanceRelationship_Abbr = relationship_abbr[StanceRelationship])
}


# Read the single AI assistant data
df1 <- read.csv("../data/encrypted_ai1.csv")

# Blank strings ("") in character covariates are NOT read as NA by read.csv;
# left as-is, "" sorts first and becomes a phantom factor reference level in
# models (e.g. UIdeo: 6 such rows), silently inflating every emmean SE and
# shifting the marginal means. Convert to NA so complete-case filters drop them.
df1$UIdeo[df1$UIdeo == ""] <- NA
df1$URace[df1$URace == ""] <- NA

# Process the data for single AI
single_ai_processed <- process_single_ai_data(df1)

# Read the dual AI assistants data
df2 <- read.csv("../data/encrypted_ai2.csv")

# Same blank-string fix for the dual file (36 blank UIdeo cells)
df2$UIdeo[df2$UIdeo == ""] <- NA
df2$URace[df2$URace == ""] <- NA

