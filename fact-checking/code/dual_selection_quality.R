# ==============================================================================
# dual_selection_quality.R
# SI analysis: "Can users select wisely when encountering conflicting advice?"
#
# The six-cell decomposition (dual_ai_decomposition_v2.R) infers selection from
# the SIGN OF THE PERFORMANCE CHANGE: a split pair plus an upward shift is coded
# "selected better" (SB), a downward shift "selected worse" (SW). That is an
# outcome-defined proxy. Here selection is measured DIRECTLY from behaviour:
# each assistant states a position on the same 0-1 scale as the participant's
# own evaluation (AIkEva; 0 = "false", 1 = "true"), so when the two assistants
# disagree we can ask which one the participant's post-interaction evaluation
# ended up closer to, independently of whether that helped.
#
#   conflicting advice : |AI1Eva - AI2Eva| >= GAP  (main GAP = 0.3; 0.2/0.4/0.5
#                        reported as sensitivity)
#   better advisor     : higher AIkCorrectness on that item (ties dropped)
#   chose_better       : |post - Eva_better| < |post - Eva_worse|   (endpoint)
#   moved_better       : moved >= tau toward the better advisor     (movement,
#                        the SB/SW analogue)
#
# Outputs: console tables + Images/dual_selection_quality.png
# Run:  Rscript Code/dual_selection_quality.R
# ==============================================================================
suppressMessages({
  library(dplyr); library(stringr); library(ggplot2); library(lme4)
  library(lmerTest); library(emmeans)
})

PROJ <- ".."
GAP <- 0.3      # minimum disagreement between the two assistants
TAU <- 0.1      # minimum substantive shift, as in the decomposition script

# ---- 1. Data, arms, and the conflicting-advice subsample ---------------------
d <- read.csv(file.path(PROJ, "data", "../data/encrypted_ai2.csv"), stringsAsFactors = FALSE)
adj <- function(code, conf) ifelse(code == -1, (1 - conf) * 0.5,
                            ifelse(code ==  1, 0.5 + conf * 0.5, 0.5))
code_of <- function(s) case_when(s == "Republican" ~ 1,
                                 s %in% c("Neutral", "Default") ~ 0,
                                 s == "Democrat" ~ -1, TRUE ~ NA_real_)

d <- d %>% mutate(
  nt   = (TruthCode + 1) / 2,
  pre  = adj(PreEvaCode,  PreConfCode),
  post = adj(PostEvaCode, PostConfCode),
  PrePerformance  = 1 - abs(nt - pre),
  PostPerformance = 1 - abs(nt - post),
  delta = PostPerformance - PrePerformance,
  AI1S = str_extract(AIStanceLabel_S, "(?<=\\(').*?(?=', ')"),
  AI2S = str_extract(AIStanceLabel_S, "(?<=', ').*?(?=')"),
  A1 = code_of(AI1S), A2 = code_of(AI2S),
  UC = case_when(UStanceLabel_S == "Republican" ~ 1,
                 UStanceLabel_S == "Independent" ~ 0,
                 UStanceLabel_S == "Democrat" ~ -1, TRUE ~ NA_real_),
  Arm = case_when(
    AI1S == "Default" & AI2S == "Default" ~ "Dual Default",
    A1 != A2 & UC > pmin(A1, A2) & UC < pmax(A1, A2) ~ "Dual Balanced",
    UC != 0 & A1 != 0 & A2 != 0 &
      sign(A1) == -sign(UC) & sign(A2) == -sign(UC) ~ "Dual Opposition",
    TRUE ~ "Other dual"))

conf <- d %>%
  dplyr::filter(!is.na(post), !is.na(pre), !is.na(AI1Eva), !is.na(AI2Eva),
                !is.na(AI1Correctness), !is.na(AI2Correctness),
                abs(AI1Eva - AI2Eva) >= GAP,
                abs(AI1Correctness - AI2Correctness) > 1e-9) %>%
  mutate(
    eva_better  = ifelse(AI1Correctness > AI2Correctness, AI1Eva, AI2Eva),
    eva_worse   = ifelse(AI1Correctness > AI2Correctness, AI2Eva, AI1Eva),
    corr_better = pmax(AI1Correctness, AI2Correctness),
    corr_worse  = pmin(AI1Correctness, AI2Correctness),
    qual_gap    = corr_better - corr_worse,
    stance_better = ifelse(AI1Correctness > AI2Correctness, A1, A2),
    stance_worse  = ifelse(AI1Correctness > AI2Correctness, A2, A1),
    # (a) endpoint measure: which advisor did the final evaluation land nearer?
    chose_better = case_when(
      abs(post - eva_better) < abs(post - eva_worse) ~ 1L,
      abs(post - eva_better) > abs(post - eva_worse) ~ 0L,
      TRUE ~ NA_integer_),
    # (b) movement measure: the SB/SW analogue, direction of the shift
    moved_better = case_when(
      abs(post - pre) < TAU ~ NA_integer_,
      sign(post - pre) == sign(eva_better - pre) ~ 1L,
      sign(post - pre) == sign(eva_worse  - pre) ~ 0L,
      TRUE ~ NA_integer_),
    # prior proximity: which advisor already agreed with the participant?
    d_better = abs(pre - eva_better), d_worse = abs(pre - eva_worse),
    prior_fav_better = case_when(d_better < d_worse ~ 1L,
                                 d_better > d_worse ~ 0L, TRUE ~ NA_integer_),
    # decisiveness: is being right here a matter of staying uncertain?
    better_is_moderate = abs(eva_better - 0.5) < abs(eva_worse - 0.5),
    truth = factor(TruthCode, levels = c(-1, 0, 1),
                   labels = c("false", "unsure", "true")),
    # congeniality: does the better advisor lean the participant's own way?
    ideo = ifelse(UIdeo %in% c("", "Prefer not to say"), NA, UIdeo),
    ideo_code = case_when(
      ideo %in% c("Extremely conservative", "Conservative", "Slightly conservative") ~ 1,
      ideo %in% c("Extremely liberal", "Liberal", "Slightly liberal") ~ -1,
      ideo == "Moderate" ~ 0, TRUE ~ NA_real_),
    congenial_is_better = case_when(
      is.na(ideo_code) | ideo_code == 0 | stance_better == stance_worse ~ NA,
      sign(stance_better) == sign(ideo_code) & stance_better != 0 ~ TRUE,
      sign(stance_worse)  == sign(ideo_code) & stance_worse  != 0 ~ FALSE,
      TRUE ~ NA))

cat("=== conflicting-advice subsample (|Eva1 - Eva2| >=", GAP, ") ===\n")
cat("rows:", nrow(conf), " participants:", n_distinct(conf$UID), "\n")
print(table(conf$Arm))
cat("\nmean quality gap (Corr_better - Corr_worse):",
    sprintf("%.3f\n", mean(conf$qual_gap)))

# ---- 2. Is selection better than chance? ------------------------------------
report_rate <- function(x, label) {
  x <- x[!is.na(x)]
  k <- sum(x); n <- length(x)
  bt <- binom.test(k, n, 0.5)
  cat(sprintf("%-34s %4d/%4d = %5.1f%%  95%% CI [%.1f, %.1f]  P = %.4f\n",
              label, k, n, 100 * k / n, 100 * bt$conf.int[1],
              100 * bt$conf.int[2], bt$p.value))
}
cat("\n=== selection accuracy vs chance (naive binomial, rows) ===\n")
report_rate(conf$chose_better, "endpoint: chose better")
report_rate(conf$moved_better, "movement: moved to better")

cat("\n--- participant-clustered test (mixed logistic, intercept only) ---\n")
for (v in c("chose_better", "moved_better")) {
  sub <- conf[!is.na(conf[[v]]), ]
  m <- glmer(as.formula(paste(v, "~ 1 + (1|UID)")), data = sub, family = binomial)
  b <- summary(m)$coefficients
  cat(sprintf("%-14s p_hat = %5.1f%%  b = %+.3f (SE %.3f)  z = %+.2f  P = %.4f  [n=%d, %d ppts]\n",
              v, 100 * plogis(b[1, 1]), b[1, 1], b[1, 2], b[1, 3], b[1, 4],
              nrow(sub), n_distinct(sub$UID)))
}

cat("\n--- by arm (endpoint measure) ---\n")
for (a in c("Dual Balanced", "Dual Opposition", "Dual Default", "Other dual")) {
  x <- conf$chose_better[conf$Arm == a]
  if (sum(!is.na(x)) >= 10) report_rate(x, a)
}

# ---- 3. Do users detect quality? gap sensitivity -----------------------------
cat("\n=== does selection track the size of the quality gap? ===\n")
sub <- conf[!is.na(conf$chose_better), ]
m_gap <- glmer(chose_better ~ scale(qual_gap) + (1|UID), data = sub, family = binomial)
print(round(summary(m_gap)$coefficients, 4))
cat("\nselection accuracy by quality-gap tercile:\n")
sub$gap_bin <- cut(sub$qual_gap, breaks = quantile(sub$qual_gap, c(0, 1/3, 2/3, 1)),
                   include.lowest = TRUE, labels = c("small", "medium", "large"))
print(sub %>% group_by(gap_bin) %>%
        summarise(n = n(), gap = mean(qual_gap), pct_better = 100 * mean(chose_better),
                  .groups = "drop") %>% as.data.frame(), row.names = FALSE, digits = 3)

# ---- 4. Congeniality: accuracy or agreement? --------------------------------
cat("\n=== congeniality vs accuracy (stance-differentiated pairs only) ===\n")
cg <- conf %>% dplyr::filter(!is.na(congenial_is_better), !is.na(chose_better))
cat("rows:", nrow(cg), " participants:", n_distinct(cg$UID), "\n")
if (nrow(cg) >= 30) {
  cat("\nselection accuracy when the congenial advisor IS / IS NOT the better one:\n")
  print(cg %>% group_by(congenial_is_better) %>%
          summarise(n = n(), pct_chose_better = 100 * mean(chose_better),
                    .groups = "drop") %>% as.data.frame(), row.names = FALSE, digits = 3)
  m_cg <- glmer(chose_better ~ congenial_is_better + (1|UID), data = cg, family = binomial)
  print(round(summary(m_cg)$coefficients, 4))

  cg$followed_congenial <- ifelse(cg$congenial_is_better, cg$chose_better, 1 - cg$chose_better)
  cat("\n")
  report_rate(cg$followed_congenial, "followed the congenial advisor")
}

# ---- 5a. Prior agreement, not accuracy --------------------------------------
cat("\n=== mechanism 1: does the participant follow whoever already agreed? ===\n")
pf <- conf %>% dplyr::filter(!is.na(chose_better), !is.na(prior_fav_better))
cat(sprintf("mean |pre - better| = %.3f   mean |pre - worse| = %.3f\n",
            mean(pf$d_better), mean(pf$d_worse)))
cat("\nselection accuracy split by whether the PRIOR already favoured the better advisor:\n")
print(pf %>% group_by(prior_fav_better) %>%
        summarise(n = n(), pct_chose_better = 100 * mean(chose_better), .groups = "drop") %>%
        as.data.frame(), row.names = FALSE, digits = 3)
cat("\n")
report_rate(ifelse(pf$prior_fav_better == 1, pf$chose_better, 1 - pf$chose_better),
            "followed the prior-congruent advisor")

# ---- 5b. Decisiveness: being right can mean staying uncertain ---------------
cat("\n=== mechanism 2: the pull toward a decisive verdict ===\n")
sub <- conf[!is.na(conf$chose_better), ]
cat(sprintf("evaluations that are decisive (|eva - 0.5| > 0.35): pre %.1f%% -> post %.1f%%\n",
            100 * mean(abs(sub$pre - 0.5) > 0.35), 100 * mean(abs(sub$post - 0.5) > 0.35)))
cat("\nselection accuracy by whether the better advisor is the MORE MODERATE one:\n")
print(sub %>% group_by(better_is_moderate) %>%
        summarise(n = n(), pct_chose_better = 100 * mean(chose_better), .groups = "drop") %>%
        as.data.frame(), row.names = FALSE, digits = 3)
cat("\nselection accuracy by ground truth:\n")
print(sub %>% group_by(truth) %>%
        summarise(n = n(), qual_gap = mean(qual_gap),
                  pct_chose_better = 100 * mean(chose_better),
                  pct_better_is_moderate = 100 * mean(better_is_moderate),
                  .groups = "drop") %>% as.data.frame(), row.names = FALSE, digits = 3)
cat("\njoint model (gap effect net of moderation pull and prior agreement):\n")
sj <- conf %>% dplyr::filter(!is.na(chose_better), !is.na(prior_fav_better))
print(round(summary(glmer(chose_better ~ scale(qual_gap) + better_is_moderate +
                            prior_fav_better + (1|UID),
                          data = sj, family = binomial))$coefficients, 4))

# ---- 5c. How much accuracy was left on the table ----------------------------
# NOTE: chose_better is defined from the final evaluation's position, so its
# association with delta is mechanical and is NOT evidence of anything. The
# counterfactual below is the interpretable quantity: the accuracy that perfect
# selection would have delivered relative to what participants actually got.
cat("\n=== how much accuracy did imperfect selection cost? ===\n")
cat(sprintf("  actual mean PostPerformance      : %.3f\n", mean(sub$PostPerformance)))
cat(sprintf("  follow-better-always             : %.3f\n",
            mean(1 - abs(sub$nt - sub$eva_better))))
cat(sprintf("  follow-worse-always              : %.3f\n",
            mean(1 - abs(sub$nt - sub$eva_worse))))
cat(sprintf("  stay with pre-interaction view   : %.3f\n", mean(sub$PrePerformance)))
cat(sprintf("  share of the available gain captured: %.1f%%\n",
            100 * (mean(sub$PostPerformance) - mean(sub$PrePerformance)) /
                  (mean(1 - abs(sub$nt - sub$eva_better)) - mean(sub$PrePerformance))))

# ---- 6. Sensitivity to the disagreement threshold ---------------------------
cat("\n=== sensitivity: disagreement threshold ===\n")
for (g in c(0.2, 0.3, 0.4, 0.5)) {
  s <- d %>% dplyr::filter(abs(AI1Eva - AI2Eva) >= g,
                           abs(AI1Correctness - AI2Correctness) > 1e-9,
                           !is.na(post)) %>%
    mutate(eb = ifelse(AI1Correctness > AI2Correctness, AI1Eva, AI2Eva),
           ew = ifelse(AI1Correctness > AI2Correctness, AI2Eva, AI1Eva),
           cb = ifelse(abs(post - eb) < abs(post - ew), 1L,
                ifelse(abs(post - eb) > abs(post - ew), 0L, NA_integer_)))
  report_rate(s$cb, sprintf("GAP >= %.1f", g))
}

# ---- 7. Figure ---------------------------------------------------------------
plot_df <- bind_rows(
  conf %>% dplyr::filter(!is.na(chose_better)) %>%
    group_by(Arm) %>%
    summarise(n = n(), p = mean(chose_better),
              lo = binom.test(sum(chose_better), n())$conf.int[1],
              hi = binom.test(sum(chose_better), n())$conf.int[2], .groups = "drop"),
  conf %>% dplyr::filter(!is.na(chose_better)) %>%
    summarise(Arm = "All dual trials", n = n(), p = mean(chose_better),
              lo = binom.test(sum(chose_better), n())$conf.int[1],
              hi = binom.test(sum(chose_better), n())$conf.int[2])) %>%
  dplyr::filter(n >= 10) %>%
  mutate(Arm = factor(Arm, levels = c("All dual trials", "Dual Default",
                                      "Dual Balanced", "Dual Opposition", "Other dual")))

base_theme <- theme_classic() +
  theme(
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.line = element_blank(),
    text = element_text(family = "Avenir", color = "black"),
    axis.title.x = element_text(family = "Avenir", size = 13.5, margin = margin(t = 12)),
    axis.title.y = element_text(family = "Avenir", size = 13.5, margin = margin(r = 10)),
    axis.text.x = element_text(family = "Avenir", size = 11.7, color = "black",
                               margin = margin(t = 4)),
    axis.text.y = element_text(family = "Avenir", size = 11.7, color = "black"),
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    axis.ticks.length = unit(3.5, "pt"),
    panel.grid = element_blank(),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10))

p <- ggplot(plot_df, aes(x = Arm, y = 100 * p)) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "grey40", linewidth = 0.6) +
  geom_col(fill = "#4E79A7", alpha = 0.85, width = 0.6,
           color = "black", linewidth = 0.3) +
  geom_errorbar(aes(ymin = 100 * lo, ymax = 100 * hi), width = 0.15, linewidth = 0.5) +
  geom_text(aes(y = 100 * hi + 2.5, label = sprintf("%.1f%%\n(n=%d)", 100 * p, n)),
            size = 3.2, family = "Avenir") +
  scale_y_continuous(name = "Followed the more accurate advisor (%)",
                     limits = c(0, 100), expand = expansion(mult = c(0, 0.05))) +
  labs(x = NULL) + base_theme +
  theme(axis.text.x = element_text(angle = 20, hjust = 1, size = 10))

out <- file.path(PROJ, "figures", "dual_selection_quality.png")
ragg::agg_png(out, width = 6.0, height = 4.3, units = "in", res = 500)
print(p); dev.off()
cat("\nSaved:", out, "\n")
