# ==============================================================================
# extended_neutral_comparison.R — EXTENDED DATA: does an explicitly NEUTRALIZED
# assistant behave like the Default one, or like the biased ones?
#
# Arms (single-AI experiment, full sample incl. the Neutral arm):
#   Default / Neutralized (politically-neutral instruction) / Republican / Democrat
# Model: the a1 primary spec —
#   lmer(PostPerformance ~ PrePerformance + Arm + as.factor(NID) + (1|UID))
# Contrasts: (A) each arm vs Default; (B) each arm vs Neutralized — both FDR
# within family — plus the full pairwise table. Hedges' g on the total SD
# (sqrt(UID var + residual var)), as in first_figure_single_a1_separated.R.
#
# Figure: Fig-2a-style ladder (nested 90/95/99 CIs), rows Republican / Default /
# Neutralized / Democrat. Saves Images/extended_neutral_comparison.png.
# Self-contained: sources preprcessing.R if needed.
# ==============================================================================
if (!exists("single_ai_processed")) {
  source("preprcessing.R")
}
suppressMessages({library(dplyr); library(ggplot2); library(lmerTest); library(emmeans)})

d <- single_ai_processed %>%
  mutate(Arm = case_when(
    AIStanceLabel_S == "Default"    ~ "Default",
    AIStanceLabel_S == "Neutral"    ~ "Neutralized",
    AIStanceLabel_S == "Republican" ~ "Republican",
    AIStanceLabel_S == "Democrat"   ~ "Democrat")) %>%
  dplyr::filter(!is.na(Arm), !is.na(PostPerformance), !is.na(PrePerformance))
d$Arm <- factor(d$Arm, levels = c("Default", "Neutralized", "Republican", "Democrat"))
cat("obs by arm:\n"); print(table(d$Arm))
cat("participants by arm:\n"); print(tapply(d$UID, d$Arm, function(x) length(unique(x))))

emm_options(lmer.df = "satterthwaite", lmerTest.limit = 20000, pbkrtest.limit = 20000)
m <- lmerTest::lmer(PostPerformance ~ PrePerformance + Arm + as.factor(NID) + (1|UID), data = d)
emm <- emmeans(m, ~ Arm, data = d)
vc <- as.data.frame(VarCorr(m))
pooled_sd <- sqrt(sum(vc$vcov[vc$grp %in% c("UID", "Residual")]))
cat("\n=== adjusted means (total SD =", round(pooled_sd, 4), ") ===\n")
print(as.data.frame(emm)[, c("Arm", "emmean", "SE")], digits = 4)

fam <- function(ref) {
  r <- as.data.frame(summary(contrast(emm, "trt.vs.ctrl", ref = ref, adjust = "fdr"),
                             infer = TRUE))
  r$g <- r$estimate / pooled_sd * (1 - 3 / (4 * r$df - 1))
  cat(sprintf("\n=== each arm vs %s (FDR within family) ===\n", ref))
  print(r[, c("contrast", "estimate", "lower.CL", "upper.CL", "p.value", "g")],
        row.names = FALSE, digits = 3)
}
fam("Default")
fam("Neutralized")

pw <- as.data.frame(summary(pairs(emm, adjust = "fdr"), infer = TRUE))
pw$g <- pw$estimate / pooled_sd * (1 - 3 / (4 * pw$df - 1))
cat("\n=== all pairwise (FDR across 6) ===\n")
print(pw[, c("contrast", "estimate", "lower.CL", "upper.CL", "p.value", "g")],
      row.names = FALSE, digits = 3)

# ---- ladder figure (Fig-2a style: nested 90/95/99 CI bars) -------------------
pd <- as.data.frame(emm) %>%
  mutate(
    y = case_when(Arm == "Republican" ~ 4, Arm == "Default" ~ 3,
                  Arm == "Neutralized" ~ 2, Arm == "Democrat" ~ 1),
    lo90 = emmean - SE * qnorm(.95),  hi90 = emmean + SE * qnorm(.95),
    lo95 = emmean - SE * qnorm(.975), hi95 = emmean + SE * qnorm(.975),
    lo99 = emmean - SE * qnorm(.995), hi99 = emmean + SE * qnorm(.995))
cols <- c(Republican = "#B2182B", Default = "#ABABAB",
          Neutralized = "#4D4D4D", Democrat = "#2166AC")   # Neutralized = darker gray than Default
p <- ggplot(pd, aes(y = y, color = Arm)) +
  geom_errorbarh(aes(xmin = lo99, xmax = hi99), height = .25, linewidth = 1.2, alpha = .3) +
  geom_errorbarh(aes(xmin = lo95, xmax = hi95), height = .20, linewidth = .9,  alpha = .5) +
  geom_errorbarh(aes(xmin = lo90, xmax = hi90), height = .15, linewidth = .7,  alpha = .8) +
  geom_point(aes(x = emmean), size = 3, alpha = .9) +
  scale_color_manual(values = cols, guide = "none") +
  scale_y_continuous(breaks = 4:1,
                     labels = c("Republican", "Default", "Neutralized", "Democrat"),
                     expand = expansion(add = .35)) +
  scale_x_continuous(labels = scales::number_format(accuracy = .01)) +
  labs(x = "Post-Interaction Performance", y = NULL) +
  xlim(0.55, 0.85) +
  theme_classic() +
  theme(text = element_text(family = "Avenir", color = "black"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = .5),
        axis.line = element_blank(),
        axis.text = element_text(family = "Avenir", size = 9, color = "black"),
        axis.title.x = element_text(family = "Avenir", size = 11, margin = margin(t = 8)),
        axis.ticks = element_line(color = "black", linewidth = .4),
        panel.grid = element_blank(),
        plot.margin = margin(12, 15, 10, 10))

# (render via ragg only: Rscript's default pdf device lacks the Avenir font;
#  do NOT auto-print `p` at top level for the same reason)
out <- path.expand("../figures/extended_neutral_comparison.png")
ragg::agg_png(out, width = 2.6, height = 2.8, units = "in", res = 500)
print(p); dev.off()
cat("\nSaved:", out, "\n")

# ==============================================================================
# Cross-news political bias of the four arms (a2/b1 "BiasScore" convention)
# BiasScore = mean |pairwise gap| in adjusted performance across the three news
# slants (Rep/Neu/Dem), per arm. Two specs:
#   SPEC A (a2 convention, main-text consistent): Arm*PoliBias + UStance +
#          factor(NID) fixed + (1|UID). NOTE: PoliBias is nested in NID, so the
#          fixed-NID matrix is rank-deficient and these SEs are OPTIMISTIC.
#   SPEC B (honest): (1|NID) random. |gap|-comparisons lose significance, but
#          the Arm x PoliBias INTERACTION (which cancels shared item effects)
#          is the defensible test: F(6,2908) = 2.60, p = .017; focused contrast
#          Neutralized-vs-Default Rep-Dem gap: D = 0.115, FDR p = .051.
# ==============================================================================
d$PoliBias <- factor(d$PoliBias, levels = c("Republican", "Neutral", "Democrat"))
biasscore_tab <- function(emm_df) {
  emm_df %>% group_by(Arm) %>% summarise(
    BiasScore = mean(c(abs(emmean[PoliBias=="Republican"] - emmean[PoliBias=="Neutral"]),
                       abs(emmean[PoliBias=="Republican"] - emmean[PoliBias=="Democrat"]),
                       abs(emmean[PoliBias=="Neutral"]    - emmean[PoliBias=="Democrat"]))),
    SE = sqrt(sum(SE^2))/3, .groups = "drop")
}
pair_z <- function(bs) {
  arms <- as.character(bs$Arm); out <- NULL
  for (i in 1:(nrow(bs)-1)) for (j in (i+1):nrow(bs)) {
    dd <- bs$BiasScore[i] - bs$BiasScore[j]; se <- sqrt(bs$SE[i]^2 + bs$SE[j]^2)
    out <- rbind(out, data.frame(contrast = paste(arms[i], "-", arms[j]),
                                 diff = dd, se = se, z = dd/se, p = 2*pnorm(-abs(dd/se))))
  }
  out$p_fdr <- p.adjust(out$p, "fdr"); out
}
mA <- lmerTest::lmer(PostPerformance ~ Arm*PoliBias + PrePerformance + UStanceLabel +
                       as.factor(NID) + (1|UID), data = d)
bsA <- biasscore_tab(as.data.frame(emmeans(mA, ~ Arm | PoliBias, data = d)))
cat("\n=== BiasScore (SPEC A, a2 convention) ===\n"); print(as.data.frame(bsA), digits = 3)
cat("--- pairwise (SPEC A; SEs optimistic, see header note) ---\n")
print(pair_z(bsA), row.names = FALSE, digits = 3)
mB <- lmerTest::lmer(PostPerformance ~ Arm*PoliBias + PrePerformance + UStanceLabel +
                       (1|NID) + (1|UID), data = d)
bsB <- biasscore_tab(as.data.frame(emmeans(mB, ~ Arm | PoliBias, data = d)))
cat("\n=== BiasScore (SPEC B, honest SEs) ===\n"); print(as.data.frame(bsB), digits = 3)
cat("--- pairwise (SPEC B) ---\n"); print(pair_z(bsB), row.names = FALSE, digits = 3)
cat("--- omnibus Arm x PoliBias (SPEC B) ---\n"); print(anova(mB)["Arm:PoliBias", ], digits = 4)

# ---- bar figure (a2/b1 style; SPEC A values for main-text consistency) -------
bp <- bsA %>% mutate(Arm = factor(Arm, levels = c("Republican", "Default", "Neutralized", "Democrat")),
                     lo = BiasScore - 1.96*SE, hi = BiasScore + 1.96*SE)
pb <- ggplot(bp, aes(x = Arm, y = BiasScore, fill = Arm)) +
  geom_col(alpha = .85, width = .62) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = .15, linewidth = .6, color = "black") +
  geom_text(aes(y = hi + max(hi)*.07, label = sprintf("%.3f", BiasScore)),
            family = "Avenir", size = 3.4) +
  scale_fill_manual(values = cols, guide = "none") +
  scale_y_continuous(labels = scales::number_format(accuracy = .01),
                     expand = expansion(mult = c(0, .5))) +
  labs(x = NULL, y = "Post-Interaction Performance Gap\nAcross Rep/Neu/Dem News") +
  theme_classic() +
  theme(text = element_text(family = "Avenir", color = "black"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = .5),
        axis.line = element_blank(),
        axis.text = element_text(family = "Avenir", size = 9.5, color = "black"),
        axis.title.y = element_text(family = "Avenir", size = 10.5, margin = margin(r = 8)),
        axis.ticks = element_line(color = "black", linewidth = .4),
        panel.grid = element_blank(),
        plot.margin = margin(12, 15, 8, 10))
out2 <- path.expand("../figures/extended_neutral_biasgap.png")
# (pb auto-print removed: default pdf device lacks Avenir)
ragg::agg_png(out2, width = 3.9, height = 2.8, units = "in", res = 500)
print(pb); dev.off()
cat("\nSaved:", out2, "\n")

# ==============================================================================
# Conversation engagement (participant words) and perceived improvement,
# 4 arms. Specs mirror the main-text counterparts:
#   ConvLength: lmer(ConvLength ~ Arm + PrePerformance + as.factor(NID) + (1|UID))
#               [= second_figure_b1 / a3 mixed spec]
#   Perceived : MCMCglmm(PerceivedImproveCode ~ Arm + as.factor(NID) +
#               PrePerformance + UIdeo + UStanceLabel + AICorrectness, ~UID)
#               [= second_figure_b1 ordinal spec; latent means built as
#                intercept + arm + PrePerf@mean, the b1 convention]
# Ladders drawn in the same style/colors as the performance panel.
# ==============================================================================
ladder <- function(pd, xlab, fname) {
  p <- ggplot(pd, aes(y = y, color = Arm)) +
    geom_errorbarh(aes(xmin = lo99, xmax = hi99), height = .25, linewidth = 1.2, alpha = .3) +
    geom_errorbarh(aes(xmin = lo95, xmax = hi95), height = .20, linewidth = .9,  alpha = .5) +
    geom_errorbarh(aes(xmin = lo90, xmax = hi90), height = .15, linewidth = .7,  alpha = .8) +
    geom_point(aes(x = est), size = 3, alpha = .9) +
    scale_color_manual(values = cols, guide = "none") +
    scale_y_continuous(breaks = 4:1,
                       labels = c("Republican", "Default", "Neutralized", "Democrat"),
                       expand = expansion(add = .35)) +
    scale_x_continuous(labels = scales::number_format(accuracy = .01),
                       expand = expansion(mult = c(.25, 1))) +
    labs(x = xlab, y = NULL) +
    theme_classic() +
    theme(text = element_text(family = "Avenir", color = "black"),
          panel.border = element_rect(color = "black", fill = NA, linewidth = .5),
          axis.line = element_blank(),
          axis.text = element_text(family = "Avenir", size = 9, color = "black"),
          axis.title.x = element_text(family = "Avenir", size = 11, margin = margin(t = 8)),
          axis.ticks = element_line(color = "black", linewidth = .4),
          panel.grid = element_blank(),
          plot.margin = margin(12, 15, 10, 10))
  fp <- path.expand(file.path("../figures", fname))
  ragg::agg_png(fp, width = 2.6, height = 2.8, units = "in", res = 500)
  print(p); dev.off(); cat("Saved:", fp, "\n")
}
ypos <- c(Republican = 4, Default = 3, Neutralized = 2, Democrat = 1)

## ---- 1. CONVERSATION ENGAGEMENT ---------------------------------------------
dc <- d[!is.na(d$ConvLength), ]
mc <- lmerTest::lmer(ConvLength ~ Arm + PrePerformance + as.factor(NID) + (1|UID), data = dc)
emc <- emmeans(mc, ~ Arm, data = dc)
vcc <- as.data.frame(VarCorr(mc)); psd_c <- sqrt(sum(vcc$vcov[vcc$grp %in% c("UID","Residual")]))
cat("\n=== CONV LENGTH: adjusted means (total SD =", round(psd_c, 3), ") ===\n")
print(as.data.frame(emc)[, c("Arm", "emmean", "SE")], digits = 4)
pwc <- as.data.frame(summary(pairs(emc, adjust = "fdr"), infer = TRUE))
pwc$g <- pwc$estimate / psd_c * (1 - 3 / (4 * pwc$df - 1))
cat("--- all pairwise (FDR) ---\n")
print(pwc[, c("contrast", "estimate", "lower.CL", "upper.CL", "p.value", "g")],
      row.names = FALSE, digits = 3)
pdc <- as.data.frame(emc) %>% mutate(est = emmean, y = ypos[as.character(Arm)],
  lo90 = est - SE*qnorm(.95),  hi90 = est + SE*qnorm(.95),
  lo95 = est - SE*qnorm(.975), hi95 = est + SE*qnorm(.975),
  lo99 = est - SE*qnorm(.995), hi99 = est + SE*qnorm(.995))
ladder(pdc, "Conversation Length", "extended_neutral_convlength.png")

## ---- 2. INTERACTION MEANINGFULNESS (b1 ordinal MCMC spec) --------------------
suppressMessages(library(MCMCglmm)); set.seed(123)
d$Meaning <- as.numeric(factor(AIInterMean <- d$AIInterMean,
               levels = c("Not meaningful at all", "Slightly meaningful",
                          "Moderately meaningful", "Very meaningful",
                          "Extremely meaningful")))
dp <- d[complete.cases(d[, c("Meaning","PrePerformance","NID","UIdeo",
                             "UStanceLabel","AICorrectness","UID")]), ]
dp$Mo <- factor(dp$Meaning, ordered = TRUE)
mp <- MCMCglmm(Mo ~ Arm + as.factor(NID) + PrePerformance + UIdeo + UStanceLabel +
                 AICorrectness, random = ~UID, family = "ordinal",
               nitt = 25000, thin = 10, burnin = 5000, data = dp, verbose = FALSE)
S <- as.matrix(mp$Sol)
pre_eff <- S[, "PrePerformance"] * mean(dp$PrePerformance, na.rm = TRUE)
draws <- sapply(levels(d$Arm), function(a) {
  lp <- S[, "(Intercept)"] + pre_eff
  cn <- paste0("Arm", a); if (cn %in% colnames(S)) lp <- lp + S[, cn]; lp })
cat("\n=== INTERACTION MEANINGFULNESS: latent means (b1 construction) ===\n")
mi_tab <- data.frame(Arm = colnames(draws), Mean = colMeans(draws), SD = apply(draws, 2, sd))
print(mi_tab, row.names = FALSE, digits = 3)
sd_obs <- sd(dp$Meaning, na.rm = TRUE)
nb <- table(dp$Arm); arms <- colnames(draws); rows <- NULL
for (i in 1:3) for (j in (i+1):4) {
  dd <- draws[, j] - draws[, i]
  Jc <- 1 - 3 / (4 * (nb[[arms[i]]] + nb[[arms[j]]] - 2) - 1)
  rows <- rbind(rows, data.frame(contrast = paste(arms[j], "-", arms[i]),
    est = mean(dd), lo = quantile(dd, .025), hi = quantile(dd, .975),
    p = min(1, 2 * min(mean(dd > 0), mean(dd < 0))),
    g = mean(dd / sd_obs) * Jc))
}
rows$p_fdr <- p.adjust(rows$p, "fdr")
cat("--- all pairwise (posterior diff, FDR across 6; pooled SD =", round(sd_obs, 3), ") ---\n")
print(rows[, c("contrast", "est", "lo", "hi", "g", "p", "p_fdr")], row.names = FALSE, digits = 3)
pdp <- mi_tab %>% mutate(est = Mean, y = ypos[Arm], Arm = factor(Arm, levels = names(ypos)),
  lo90 = est - SD*qnorm(.95),  hi90 = est + SD*qnorm(.95),
  lo95 = est - SD*qnorm(.975), hi95 = est + SD*qnorm(.975),
  lo99 = est - SD*qnorm(.995), hi99 = est + SD*qnorm(.995))
ladder(pdp, "Interaction Meaningfulness", "extended_neutral_meaningfulness.png")
