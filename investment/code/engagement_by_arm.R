# ==============================================================================
# engagement_by_arm.R
# Engagement with the AI advisor by treatment group: Averse / Default / Seeking.
# Investment-experiment analog of first_figure_single_a3.R (Diversified AI Bias),
# with the design differences that follow from our experiment:
#   - 3 groups (Som+Ext aggregated per side), Risk-Neutral EXCLUDED
#   - one observation per participant -> NO participant RE/clustering; HC3 SEs
#   - news-item FE (NID)      -> wave FE
#   - PrePerformance          -> pre_active_m2_ann (baseline Active M²)
#   - UIdeo/UStance/AICorrectness have no analog -> dropped
#
# Specifications (pre-reg specifies the Active M² model; engagement outcome
# spec was not registered -> report the ladder):
#   spec1:  outcome ~ grp + wave
#   spec2:  outcome ~ grp + wave + pre_active_m2_ann
#   spec3:  spec2 + 3-group imbalanced covariates (wave1_exam §11.10a,
#           max pairwise |ASMD| > 0.10: Sex, trade_freq_12m, news_follow_freq,
#           time_preference_switch, fin_lit_score, fin_lit_selfassess,
#           market_outlook_2wk; median-imputed — precision controls, randomized)
#   spec4:  spec3 + risk_pref_score (balanced, ASMD .017 -> precision only)
#
# Outcomes:
#   ConvLength      = followup_words  (participant words, canned opener excluded)
#   ConvRound       = n_followup_turns
#   LengthPerRound  = length_per_round (NA when 0 rounds)
#   + LLM rubric dims: behavioral, cognitive, emotional, autonomy, social_presence
#     (floor-scored 0 for zero-follow-up, EXCEPT autonomy = NA -> autonomy models
#      run on engagers only; flagged in output)
#
# Inputs:  engagement_annotations.csv, active_m2_treatment_data.csv
#   setwd("investment/code"); source("_setup.R"); source("engagement_by_arm.R")
# ==============================================================================
suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(tidyr); library(scales)
  library(sandwich); library(lmtest); library(emmeans)
})

.args <- commandArgs(FALSE); .f <- sub("^--file=", "", .args[grep("^--file=", .args)])
SCRIPT_DIR <- if (length(.f)) dirname(normalizePath(.f)) else getwd()
rd <- function(f) read.csv(file.path(DATA_DIR, f), check.names = FALSE,
                           stringsAsFactors = FALSE)

eng <- rd("engagement_annotations.csv")
m2  <- rd("active_m2_treatment_data.csv")
cov <- rd("participant_covariates.csv")
d <- merge(eng, m2[, c("participantId", "pre_active_m2_ann")], by = "participantId")
d <- merge(d, cov, by = "participantId", all.x = TRUE)

# ── 3 groups, Neutral excluded; Default = reference ──────────────────────────
d$grp <- with(d, ifelse(ai_group %in% c("Extremely Risk-Averse","Somewhat Risk-Averse"), "Averse",
              ifelse(ai_group %in% c("Extremely Risk-Seeking","Somewhat Risk-Seeking"), "Seeking",
              ifelse(ai_group == "Default", "Default", NA_character_))))
d <- d[!is.na(d$grp), ]
d$grp  <- relevel(factor(d$grp, levels = c("Averse","Default","Seeking")), ref = "Default")
d$wave <- factor(d$wave)
d$conv_length <- d$followup_words
d$conv_round  <- d$n_followup_turns
d$any_follow  <- as.integer(d$n_followup_turns > 0)
cat(sprintf("N = %d (Neutral excluded): %s\n\n", nrow(d),
            paste(names(table(d$grp)), table(d$grp), sep = "=", collapse = ", ")))

hc3 <- function(m) coeftest(m, vcov = vcovHC(m, type = "HC3"))

# ── covariate sets for spec3/spec4 (see header) ──────────────────────────────
COVS3 <- c("Sex_enc", "trade_freq_12m_enc", "news_follow_freq_enc",
           "time_preference_switch", "fin_lit_score", "fin_lit_selfassess_1_enc",
           "market_outlook_2wk_enc")
RISKPREF <- "risk_pref_score"
stopifnot(all(c(COVS3, RISKPREF) %in% names(d)))
for (cv in c(COVS3, RISKPREF)) {   # median-impute (notebook §6.9/§11.10b convention)
  d[[cv]] <- as.numeric(d[[cv]])
  d[[cv]][is.na(d[[cv]])] <- median(d[[cv]], na.rm = TRUE)
}
SPECS <- list(
  spec1 = "grp + wave",
  spec2 = "grp + wave + pre_active_m2_ann",
  spec3 = paste(c("grp + wave + pre_active_m2_ann", COVS3), collapse = " + "),
  spec4 = paste(c("grp + wave + pre_active_m2_ann", COVS3, RISKPREF), collapse = " + ")
)

# ══ 1. Conversation length ════════════════════════════════════════════════════
cat("=== ConvLength (participant follow-up words) — raw descriptives ===\n")
print(d %>% group_by(grp) %>%
        summarise(n = n(), mean = mean(conv_length), sd = sd(conv_length),
                  se = sd / sqrt(n), median = median(conv_length),
                  pct_zero = mean(conv_length == 0), .groups = "drop") %>%
        as.data.frame(), digits = 3, row.names = FALSE)

m1 <- lm(conv_length ~ grp + wave,                     data = d)   # spec1
m2l <- lm(conv_length ~ grp + wave + pre_active_m2_ann, data = d)  # spec2
cat("\n=== spec1: conv_length ~ grp + wave  (HC3) ===\n");                 print(hc3(m1))
cat("\n=== spec2: conv_length ~ grp + wave + pre_active_m2_ann (HC3) ===\n"); print(hc3(m2l))

# robustness: 39% zeros, skew 4.5 -> log1p + hurdle
cat("\n=== robustness: log(1+words), both specs (HC3, grp rows only) ===\n")
for (f in list(log1p(conv_length) ~ grp + wave,
               log1p(conv_length) ~ grp + wave + pre_active_m2_ann)) {
  ct <- hc3(lm(f, data = d)); print(ct[grep("^grp", rownames(ct)), , drop = FALSE])
}
cat("\n=== hurdle part 1: any_follow ~ grp + wave (logit) ===\n")
print(coeftest(glm(any_follow ~ grp + wave, family = binomial, data = d)))
cat("\n=== hurdle part 2: words among engagers (grp rows, HC3) ===\n")
ct <- hc3(lm(conv_length ~ grp + wave, data = d[d$any_follow == 1, ]))
print(ct[grep("^grp", rownames(ct)), , drop = FALSE])

# adjusted means (both specs; figure uses spec2) + pairwise + Hedges' g
emm1 <- emmeans(m1,  ~ grp, vcov. = vcovHC(m1,  type = "HC3"))
emm2 <- emmeans(m2l, ~ grp, vcov. = vcovHC(m2l, type = "HC3"))
cat("\n=== adjusted mean ConvLength — spec1 ===\n"); print(summary(emm1))
cat("\n=== adjusted mean ConvLength — spec2 ===\n"); print(summary(emm2))
cat("\n=== pairwise contrasts (spec2) with Hedges' g ===\n")
pw <- summary(pairs(emm2), infer = TRUE)
J  <- 1 - 3 / (4 * df.residual(m2l) - 1)
pw$hedges_g <- (pw$estimate / sigma(m2l)) * J
print(pw, digits = 3)

# per-wave (regime-specific) group coefficients, spec2 within wave
cat("\n=== per-wave: conv_length ~ grp + pre_active_m2_ann (HC3, grp rows) ===\n")
for (w in levels(d$wave)) {
  ct <- hc3(lm(conv_length ~ grp + pre_active_m2_ann, data = d[d$wave == w, ]))
  cat(sprintf("-- %s --\n", w)); print(ct[grep("^grp", rownames(ct)), , drop = FALSE])
}

# ══ 1b. Reply turns (ConvRound — registered measure; the FIGURE outcome) ══════
cat("\n=== ConvRound (participant reply turns) — raw descriptives ===\n")
print(d %>% group_by(grp) %>%
        summarise(n = n(), mean = mean(conv_round), sd = sd(conv_round),
                  se = sd / sqrt(n), pct_zero = mean(conv_round == 0), .groups = "drop") %>%
        as.data.frame(), digits = 3, row.names = FALSE)

r1 <- lm(conv_round ~ grp + wave,                     data = d)   # spec1
r2 <- lm(conv_round ~ grp + wave + pre_active_m2_ann, data = d)   # spec2
cat("\n=== spec1: conv_round ~ grp + wave  (HC3) ===\n");                  print(hc3(r1))
cat("\n=== spec2: conv_round ~ grp + wave + pre_active_m2_ann (HC3) ===\n"); print(hc3(r2))

emm1r <- emmeans(r1, ~ grp, vcov. = vcovHC(r1, type = "HC3"))
emm2r <- emmeans(r2, ~ grp, vcov. = vcovHC(r2, type = "HC3"))
cat("\n=== adjusted mean ConvRound — spec1 ===\n"); print(summary(emm1r))
cat("\n=== adjusted mean ConvRound — spec2 ===\n"); print(summary(emm2r))
cat("\n=== pairwise contrasts (spec2) with Hedges' g ===\n")
pwr <- summary(pairs(emm2r), infer = TRUE)
Jr  <- 1 - 3 / (4 * df.residual(r2) - 1)
pwr$hedges_g <- (pwr$estimate / sigma(r2)) * Jr
print(pwr, digits = 3)

cat("\n=== per-wave: conv_round ~ grp + pre_active_m2_ann (HC3, grp rows) ===\n")
for (w in levels(d$wave)) {
  ct <- hc3(lm(conv_round ~ grp + pre_active_m2_ann, data = d[d$wave == w, ]))
  cat(sprintf("-- %s --\n", w)); print(ct[grep("^grp", rownames(ct)), , drop = FALSE])
}

# ══ 2. LLM rubric dimensions — spec ladder, FDR/Bonferroni across dims ════════
DIMS <- c("behavioral", "cognitive", "emotional", "autonomy", "social_presence")
res <- do.call(rbind, lapply(DIMS, function(dim) {
  do.call(rbind, lapply(names(SPECS), function(s) {
    ct <- hc3(lm(as.formula(paste(dim, "~", SPECS[[s]])), data = d))
    rows <- grep("^grp", rownames(ct))
    data.frame(dimension = dim, spec = s,
               contrast = sub("^grp", "", rownames(ct)[rows]),
               estimate = ct[rows, 1], se = ct[rows, 2], p = ct[rows, 4])
  }))
}))
res <- res %>% group_by(spec, contrast) %>%
  mutate(p_fdr = p.adjust(p, "fdr"), p_bonf = p.adjust(p, "bonferroni")) %>% ungroup()
cat("\n=== rubric dimensions ~ grp (vs Default), HC3; FDR/Bonf across 5 dims ===\n")
cat("    spec3 = +3-group imbalanced covs; spec4 = spec3 + risk_pref_score\n")
cat("    NOTE: autonomy is NA for zero-follow-up participants -> engagers only (selection!)\n")
print(as.data.frame(res), digits = 3, row.names = FALSE)

# headline count models under the fullest controls (spec4 RHS)
cat("\n=== headline participation models under spec4 controls (grp rows) ===\n")
cat("-- any_follow (logit) --\n")
ct <- coeftest(glm(as.formula(paste("any_follow ~", SPECS$spec4)), family = binomial, data = d))
print(ct[grep("^grp", rownames(ct)), , drop = FALSE])
cat("-- conv_round (HC3) + Tukey pairwise --\n")
rc <- lm(as.formula(paste("conv_round ~", SPECS$spec4)), data = d)
ct <- hc3(rc); print(ct[grep("^grp", rownames(ct)), , drop = FALSE])
print(summary(pairs(emmeans(rc, ~ grp, vcov. = vcovHC(rc, type = "HC3")))), digits = 3)

# ══ 3. Figure 1 — adjusted reply-turn (ConvRound) bar, spec2 (house style) ════
gcol <- c("Averse" = "#59A14F", "Default" = "#79706E", "Seeking" = "#B07AA1")
 


pd <- as.data.frame(summary(emm2r)) %>%
  mutate(lo = emmean - 1.96 * SE, hi = emmean + 1.96 * SE,
         grp = factor(as.character(grp), levels = c("Averse", "Default", "Seeking")))

p1 <- ggplot(pd, aes(x = emmean, y = grp, fill = grp)) +
  geom_col(alpha = 0.8, width = 0.85) +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0.4, size = 0.5) +
  geom_text(aes(x = hi + max(emmean) * 0.06, label = sprintf("%.2f", emmean)),
            size = 3.3, family = "Avenir", hjust = 0) +
  # geom_point(color = "black", size = 2.7, shape = 20) +
  scale_fill_manual(values = gcol) +
  scale_x_continuous(labels = label_number(accuracy = 0.1),
                     expand = expansion(mult = c(0, 0.15))) +
  labs(x = "Number of Reply Turns", y = NULL) +
  nature_theme + 
  theme(
    legend.position = "none",
    panel.border = element_blank()
  )

print(p1)
ggsave(file.path(FIG_DIR, "engagement_reply_turns.png"), p1, width = 4., height = 1.8, dpi = 500)

# ══ 4. Figure 2 — 5-dim rubric radar (political-study construction) ═══════════
# FULL SAMPLE (floors included; autonomy is NA for non-engagers -> its mean is
# engagers-only). z of each group mean vs the pooled individual-level mean/sd,
# radius = pmax(0.5, 4 + z*8) — same scaling as first_figure_single_a3.R, but
# ring labels CORRECTED: at this scaling ring 2 = -0.25 SD, 4 = Mean, 6 = +0.25 SD
# (the political figure's -1SD/Mean/+1SD labels did not match its own scaling).
prof <- d %>% pivot_longer(all_of(DIMS), names_to = "dim", values_to = "score") %>%
  group_by(dim) %>% mutate(z = (score - mean(score, na.rm = TRUE)) / sd(score, na.rm = TRUE)) %>%
  group_by(grp, dim) %>% summarise(zbar = mean(z, na.rm = TRUE), .groups = "drop") %>%
  mutate(radius = pmax(0.5, 4 + zbar * 8),
         dim = factor(dim, levels = DIMS,
                      labels = c("Behavioral", "Cognitive", "Emotional",
                                 "Autonomy", "Social Presence"))) %>%
  arrange(grp, dim)   # polygon vertices must follow angular (factor) order,
                      # not summarise()'s alphabetical row order
n_dim <- nlevels(prof$dim)
ang <- setNames(seq(0, 2 * pi, length.out = n_dim + 1)[1:n_dim], levels(prof$dim))
prof$x <- prof$radius * cos(ang[prof$dim]); prof$y <- prof$radius * sin(ang[prof$dim])

ring <- function(r) annotate("path", x = r * cos(seq(0, 2 * pi, length.out = 120)),
                             y = r * sin(seq(0, 2 * pi, length.out = 120)),
                             color = "gray80", linewidth = 0.3)
p2 <- ggplot(prof, aes(x, y, color = grp, fill = grp, group = grp)) +
  ring(2) + ring(4) + ring(6) +
  annotate("segment", x = 0, y = 0, xend = 9 * cos(ang), yend = 9 * sin(ang),
           color = "gray60", linewidth = 0.3) +
  geom_polygon(alpha = 0.2, linewidth = 1.2) + geom_point(size = 3, alpha = 0.8) +
  annotate("text", x = 9.5 * cos(ang), y = 9.5 * sin(ang), label = names(ang),
           hjust = 0.5, vjust = 0.5, size = 3.2, family = "Avenir") +
  annotate("text", x = -0.3, y = c(2, 4, 6), label = c("-0.25 SD", "Mean", "+0.25 SD"),
           size = 2.8, family = "Avenir", color = "gray60", hjust = 1) +
  scale_color_manual(values = gcol, name = NULL) +
  scale_fill_manual(values = gcol, guide = "none") +
  coord_fixed() + xlim(-12, 12) + ylim(-12, 12) +
  theme_void() +
  theme(
    legend.position = "none",
    legend.text = element_text(family = "Avenir", size = 8),
    plot.margin = margin(0, 0, 0, 0),
    panel.background = element_rect(fill = "white", color = NA)
  )

print(p2)

# ggsave(file.path(FIG_DIR, "engagement_radar.pdf"), p2, width = 4.8, height = 4.6, device = cairo_pdf)
ggsave(file.path(FIG_DIR, "engagement_radar.png"), p2, width = 3, height = 3, dpi = 500)
cat(sprintf("\nSaved engagement_reply_turns.{pdf,png} + engagement_radar.{pdf,png} in %s\n", SCRIPT_DIR))
cat("Radar note: full sample; autonomy mean is engagers-only (NA for zero-follow-up).\n")

# ══ 5. Binary comparison — Biased (Averse+Seeking) vs Non-Biased (Default) ════
# Mirrors the political study's BiasedType factor (ref = Non-Biased).
# Same spec ladder; outcomes: conv_length, conv_round, any_follow, 5 rubric dims.
d$biased <- factor(ifelse(d$grp == "Default", "Non-Biased", "Biased"),
                   levels = c("Non-Biased", "Biased"))
cat(sprintf("\n=== BINARY: Biased (n=%d) vs Non-Biased (n=%d) ===\n",
            sum(d$biased == "Biased"), sum(d$biased == "Non-Biased")))
SPECS_B <- lapply(SPECS, function(s) sub("^grp", "biased", s))

cat("\n--- Biased coefficient (vs Non-Biased): quantitative engagement ---\n")
cnt <- do.call(rbind, lapply(c("conv_length", "conv_round"), function(y) {
  do.call(rbind, lapply(names(SPECS_B), function(s) {
    ct <- hc3(lm(as.formula(paste(y, "~", SPECS_B[[s]])), data = d))
    r <- grep("^biased", rownames(ct))
    data.frame(outcome = y, spec = s, estimate = ct[r, 1], se = ct[r, 2], p = ct[r, 4])
  }))
}))
lg <- do.call(rbind, lapply(names(SPECS_B), function(s) {
  ct <- coeftest(glm(as.formula(paste("any_follow ~", SPECS_B[[s]])),
                     family = binomial, data = d))
  r <- grep("^biased", rownames(ct))
  data.frame(outcome = "any_follow (logit)", spec = s,
             estimate = ct[r, 1], se = ct[r, 2], p = ct[r, 4])
}))
print(rbind(cnt, lg), digits = 3, row.names = FALSE)

cat("\n--- Biased coefficient (vs Non-Biased): rubric dimensions (FDR across 5) ---\n")
cat("    NOTE: Averse & Seeking pull in opposite directions on some dims -> pooling dilutes.\n")
resb <- do.call(rbind, lapply(DIMS, function(dim) {
  do.call(rbind, lapply(names(SPECS_B), function(s) {
    ct <- hc3(lm(as.formula(paste(dim, "~", SPECS_B[[s]])), data = d))
    r <- grep("^biased", rownames(ct))
    data.frame(dimension = dim, spec = s, estimate = ct[r, 1], se = ct[r, 2], p = ct[r, 4])
  }))
}))
resb <- resb %>% group_by(spec) %>%
  mutate(p_fdr = p.adjust(p, "fdr"), p_bonf = p.adjust(p, "bonferroni")) %>% ungroup()
print(as.data.frame(resb), digits = 3, row.names = FALSE)
