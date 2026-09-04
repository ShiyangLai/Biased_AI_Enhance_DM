# ==============================================================================
# positioning_map_cross_domain.R
# ONE positioning map, TWO experiments: each of the 5 arms (Single Default /
# Single Biased / Dual Default / Dual Opposition / Dual Balanced) is placed on
#   x = OBJECTIVE performance   (investment: post Active M2; misinfo: post
#                                detection performance)
#   y = PERCEIVED IMPROVEMENT   (investment: CHANGE IN SELF-RATED CONFIDENCE,
#                                post - pre, MCMC ordinal latent [SI §X defends
#                                this operationalization]; misinfo:
#                                PerceivedImproveCode, MCMC ordinal latent)
#
# Cross-domain scaling: outcomes live on incompatible scales, so each arm is
# plotted as its DEVIATION FROM THE DOMAIN GRAND MEAN (unweighted mean of the
# 5 condition means) in pooled-SD units, using each project's own Hedges-g
# convention for the SD (model residual SD for the performance models; observed
# outcome SD for the ordinal latent scales). PATTERNS are comparable across
# domains, LEVELS are not.
#
# Uncertainty matches the plotted quantity: per-arm CIs of marginal means embed
# GLOBAL uncertainty (intercept/thresholds/covariates) that is shared by all 5
# arms and irrelevant to their relative positions (the five_arm script itself
# notes shared terms cancel in pairwise differences). So:
#   frequentist axes: emmeans "eff" contrasts (level - grand mean) with HC3/
#                     Satterthwaite CIs at 90/95/99
#   MCMC axes:        each posterior draw centered at its own 5-arm mean
#                     (shared terms cancel within draw), then quantiles
#
# Estimates mirror the registered analyses verbatim:
#   Investment x: post_active_m2_ann ~ ExperimentType + wave + pre   (HC3 emmeans)
#   Investment y: MCMCglmm(conf_change ordinal ~ ExperimentType + wave; R fixed),
#                 latent marginal means, wave at mode  (= perception_outcomes.R d2)
#   Misinfo    x: lmer(Post ~ ExperimentType + Pre + NID + UStance + (1|UID)),
#                 emmeans                        (= five_arm_analysis PART A)
#   Misinfo    y: MCMCglmm(PerceivedImproveCode ~ ExperimentType + NID + Pre +
#                 UIdeo + AICorrectness + UStance, random=~UID), latent
#                 marginal means (Pre at mean, NID at mode)      (= PART B)
#
# Figure: frameless (no panel border / axis lines / ticks / tick text), dashed
# crosshairs at (0,0) = domain grand mean; uncertainty as GRADIENT-FADE
# whiskers (alpha follows the implied density through the 90/95/99 bounds —
# same ladder information, drawn as a continuous comet fade). Color = DOMAIN
# (investment PRGn green #1B7837 / misinfo RdBu blue #2166AC); marker shape =
# CONTENT (circle = default, triangle = biased), white-rimmed, no black frame;
# SESSION is marked with a thin OPEN RING: ringed = dual AI, plain = single AI.
# No in-figure labels (added in slides); axes zoomed to the point cloud, faded
# tails run past the edge.
#
# CAVEATS: single-vs-dual is cross-experiment in the investment domain; misinfo
# Dual Default has n=54 participants (long whiskers); the map is a positioning
# summary — inference lives in the per-outcome scripts.
#
# Inputs: active_m2_treatment_data.csv , dual_active_m2.csv ,
#         perceived_improvement.csv (§16), and the misinfo export
#         five_arm_single_dual.csv (path below).
# Run:  Rscript positioning_map_cross_domain.R   (2 MCMC fits, ~2-5 min)
# ==============================================================================
suppressPackageStartupMessages({
  library(ggplot2); library(emmeans); library(dplyr); library(sandwich)
  library(lmtest); library(scales); library(lme4); library(lmerTest)
  library(MCMCglmm)
})

.args <- commandArgs(FALSE); .f <- sub("^--file=", "", .args[grep("^--file=", .args)])
SCRIPT_DIR <- if (length(.f)) dirname(normalizePath(.f)) else getwd()
rd <- function(f) { p <- file.path(DATA_DIR, f); if (!file.exists(p)) p <- file.path(DATA_DIR, f)
                    read.csv(p, check.names = FALSE, stringsAsFactors = FALSE) }
MISINFO_CSV <- "../../fact-checking/data/five_arm_single_dual.csv"

LEVELS <- c("Single AI Default", "Single AI Biased",
            "Dual AI Default", "Dual AI Opposition", "Dual AI Balanced")
# investment-only breakdown: Single AI Biased split into Averse vs Seeking.
# 6-level factor for the models; the map shows the pooled point (dark) PLUS the
# two sub-conditions (lighter green), all as deviations from the SAME 5-condition
# grand mean, so the pooled point and the crosshairs are unchanged.
LEV6 <- c("Single AI Default", "Single AI Averse", "Single AI Seeking",
          "Dual AI Default", "Dual AI Opposition", "Dual AI Balanced")
COND7 <- c("Single AI Default", "Single AI Biased", "Single AI Averse",
           "Single AI Seeking", "Dual AI Default", "Dual AI Opposition",
           "Dual AI Balanced")
CI_LVLS <- c(0.90, 0.95, 0.99)

# ══ INVESTMENT ════════════════════════════════════════════════════════════════
single <- rd("active_m2_treatment_data.csv")
dual   <- rd("dual_active_m2.csv")
pi_inv <- rd("perceived_improvement.csv")

s1 <- single %>%
  filter(ai_group %in% c("Default", "Extremely Risk-Averse", "Somewhat Risk-Averse",
                         "Extremely Risk-Seeking", "Somewhat Risk-Seeking")) %>%
  transmute(participantId, wave, experiment = "single",
            pre_active_m2_ann, post_active_m2_ann,
            ExperimentType = ifelse(ai_group == "Default",
                                    "Single AI Default", "Single AI Biased"),
            ExperimentType6 = ifelse(ai_group == "Default", "Single AI Default",
                              ifelse(grepl("Averse", ai_group),
                                     "Single AI Averse", "Single AI Seeking")))
d1 <- dual %>%
  filter(dual_condition %in% c("dual_nonbiased", "dual_balanced", "dual_opposition")) %>%
  transmute(participantId, wave, experiment = "dual",
            pre_active_m2_ann, post_active_m2_ann,
            ExperimentType = recode(dual_condition,
                                    "dual_nonbiased"  = "Dual AI Default",
                                    "dual_opposition" = "Dual AI Opposition",
                                    "dual_balanced"   = "Dual AI Balanced"),
            ExperimentType6 = ExperimentType)
inv <- bind_rows(s1, d1) %>%
  left_join(pi_inv %>% select(participantId, experiment, pre_conf, post_conf) %>%
              mutate(conf_change = post_conf - pre_conf),
            by = c("participantId", "experiment")) %>%
  mutate(ExperimentType  = factor(ExperimentType,  levels = LEVELS),
         ExperimentType6 = factor(ExperimentType6, levels = LEV6),
         wave = factor(wave))

# ── x: Active M2 — deviation from grand mean ("eff" contrasts, HC3) ──────────
# eff contrasts = each level minus the (unweighted) grand mean, with the CI of
# THAT deviation — the comparison-relevant uncertainty for this map.
eff_tab <- function(emm_obj, clean = identity) {
  eff <- contrast(emm_obj, method = "eff")
  out <- as.data.frame(summary(eff)) %>%
    transmute(cond = clean(sub(" effect$", "", contrast)), est = estimate)
  for (lv in CI_LVLS) {
    ci <- as.data.frame(confint(eff, level = lv))
    out[[paste0("lo", lv*100)]] <- ci$lower.CL; out[[paste0("hi", lv*100)]] <- ci$upper.CL
  }
  out
}
# investment: 6-level model (biased split), custom contrasts vs the 5-condition
# grand mean; "Single AI Biased" = n-weighted mix of the Averse/Seeking cells.
dm <- inv %>% filter(!is.na(pre_active_m2_ann), !is.na(post_active_m2_ann))
mm_inv <- lm(post_active_m2_ann ~ ExperimentType6 + wave + pre_active_m2_ann, data = dm)
Vm <- vcovHC(mm_inv, type = "HC3")
wA <- sum(dm$ExperimentType6 == "Single AI Averse") /
      sum(dm$ExperimentType6 %in% c("Single AI Averse", "Single AI Seeking"))
unit6  <- function(i) { v <- rep(0, 6); v[i] <- 1; v }
pooled <- c(0, wA, 1 - wA, 0, 0, 0)                     # LEV6 order
gmv    <- (unit6(1) + pooled + unit6(4) + unit6(5) + unit6(6)) / 5
CONTR7 <- setNames(list(unit6(1) - gmv, pooled - gmv, unit6(2) - gmv,
                        unit6(3) - gmv, unit6(4) - gmv, unit6(5) - gmv,
                        unit6(6) - gmv), COND7)
emm_inv6 <- emmeans(mm_inv, ~ ExperimentType6, vcov. = Vm)
eff_inv  <- contrast(emm_inv6, method = CONTR7)
inv_x <- data.frame(cond = COND7, est = summary(eff_inv)$estimate)
for (lv in CI_LVLS) {
  ci <- as.data.frame(confint(eff_inv, level = lv))
  inv_x[[paste0("lo", lv*100)]] <- ci$lower.CL; inv_x[[paste0("hi", lv*100)]] <- ci$upper.CL
}
S_inv_x <- sigma(mm_inv)                    # Hedges convention of the M2 script
cat("=== investment x: Active M2 deviation from grand mean (custom eff, HC3) ===\n")
print(inv_x, digits = 4)

# ── y: PERCEIVED IMPROVEMENT = change in self-rated confidence (post - pre).
# The investment instrument has no direct analog of the misinfo perceived-
# improvement item, so the confidence delta operationalizes the same construct
# (defended in SI §X). The `perceived_improve` item is retained in the export as
# a secondary measure; it shows no condition differences (all p >= 0.10).
# PRIOR: family="ordinal" does NOT identify the residual variance (only
# effect/scale ratios are), so without fixing it the chain drifts along the scale
# direction and latent means / CI widths / pairwise p-values are unstable across
# runs. R fixed at 1 + longer chain, as in bias_magnitude_outcomes.R.
dpi <- inv %>% filter(!is.na(conf_change)) %>%
  mutate(pi_ord = factor(conf_change, ordered = TRUE))
cat(sprintf("\ninvestment confidence change n = %d — fitting MCMCglmm...\n", nrow(dpi)))
set.seed(123)
mc_inv <- MCMCglmm(pi_ord ~ ExperimentType6 + wave,
                   family = "ordinal", nitt = 55000, thin = 25, burnin = 5000,
                   prior = list(R = list(V = 1, fix = 1)),
                   data = as.data.frame(dpi), verbose = FALSE)
post <- as.matrix(mc_inv$Sol); cn <- colnames(post)
mode_wave <- names(sort(table(dpi$wave), decreasing = TRUE))[1]
mm_draw6 <- sapply(LEV6, function(arm) {
  lp <- if ("(Intercept)" %in% cn) post[, "(Intercept)"] else rep(0, nrow(post))
  an <- paste0("ExperimentType6", arm); if (an %in% cn) lp <- lp + post[, an]
  wn <- paste0("wave", mode_wave);      if (wn %in% cn) lp <- lp + post[, wn]
  lp
})
# pooled Single AI Biased = n-weighted mix of the two sub-cells (this sample)
wA2 <- sum(dpi$ExperimentType6 == "Single AI Averse") /
       sum(dpi$ExperimentType6 %in% c("Single AI Averse", "Single AI Seeking"))
mm_draw_inv <- cbind(mm_draw6[, "Single AI Default"],
                     wA2 * mm_draw6[, "Single AI Averse"] +
                       (1 - wA2) * mm_draw6[, "Single AI Seeking"],
                     mm_draw6[, "Single AI Averse"],
                     mm_draw6[, "Single AI Seeking"],
                     mm_draw6[, "Dual AI Default"],
                     mm_draw6[, "Dual AI Opposition"],
                     mm_draw6[, "Dual AI Balanced"])
colnames(mm_draw_inv) <- COND7
# deviation from grand mean WITHIN each posterior draw (shared terms cancel);
# center_cols = the conditions defining the grand mean (default: all columns)
qs <- function(M, pr) apply(M, 2, quantile, pr)
dev_tab <- function(draws, conds, center_cols = seq_len(ncol(draws))) {
  dv <- draws - rowMeans(draws[, center_cols, drop = FALSE])
  out <- data.frame(cond = conds, est = colMeans(dv))
  for (lv in CI_LVLS) { a <- (1 - lv) / 2
    out[[paste0("lo", lv*100)]] <- qs(dv, a)
    out[[paste0("hi", lv*100)]] <- qs(dv, 1 - a) }
  out
}
inv_y <- dev_tab(mm_draw_inv, COND7, center_cols = match(LEVELS, COND7))
# Standardize the LATENT estimates by the model-implied LATENT SD, sqrt(sum of
# variance components) -- NOT by the observed response SD. The two live on
# different scales, and dividing latent means by a response-scale SD is only
# accidentally sensible. With the latent SD the coordinates are invariant to the
# (arbitrary) ordinal scale identification: across no-prior / R-fixed / parameter-
# expanded priors the misinfo coordinates agree to ~0.01 SD, whereas the raw
# latent estimates differ by an order of magnitude.
S_inv_y <- sqrt(mean(as.matrix(mc_inv$VCV)[, "units"]))   # = 1 (R fixed, no random effect)
cat("\n=== investment y: confidence change — latent deviation from grand mean ===\n")
print(inv_y, digits = 3)

# ── triad characterization: Single Averse / Single Seeking / Dual Balanced ────
# Dual Balanced presents BOTH single biases at once, so the key composite tests
# whether it equals the AVERAGE of the two one-sided biases ("mixture linearity";
# a triangulation/complementarity bonus would push it above the midpoint).
# CAVEAT: Balanced is the dual experiment (cross-experiment vs the single arms);
# the Averse-Seeking direction is regime-embedded, the midpoint test is not.
TRIAD <- list("Averse - Seeking"    = unit6(2) - unit6(3),
              "Balanced - Averse"   = unit6(6) - unit6(2),
              "Balanced - Seeking"  = unit6(6) - unit6(3),
              "Balanced - mid(A,S)" = unit6(6) - 0.5 * unit6(2) - 0.5 * unit6(3))
ctx <- as.data.frame(summary(contrast(emm_inv6, TRIAD), infer = TRUE))
ctx$SD_units <- ctx$estimate / S_inv_x
cat("\n=== triad x: post Active M2 (HC3) ===\n")
print(ctx[, c("contrast","estimate","SE","lower.CL","upper.CL","p.value","SD_units")],
      row.names = FALSE, digits = 3)
cty <- do.call(rbind, lapply(names(TRIAD), function(nm) {
  w <- TRIAD[[nm]]; d <- as.vector(mm_draw6 %*% w)
  data.frame(contrast = nm, estimate = mean(d),
             lo95 = quantile(d, .025), hi95 = quantile(d, .975),
             p_two_tail = min(1, 2 * min(mean(d < 0), mean(d > 0))),
             SD_units = mean(d) / S_inv_y, row.names = NULL) }))
cat("\n=== triad y: confidence change (latent scale, posterior) ===\n")
print(cty, row.names = FALSE, digits = 3)

# ══ MISINFORMATION (five_arm_analysis_standalone.R, PARTs A & B verbatim) ═════
mis <- read.csv(MISINFO_CSV, check.names = FALSE, stringsAsFactors = FALSE)
# Blank UIdeo (12 rows) is non-response, kept as its own category rather than
# dropped. Give it an explicit label and make sure it is NOT the reference level
# ("" would sort first), so the intercept is anchored on a substantive category.
mis$UIdeo[mis$UIdeo == ""] <- "Not reported"
mis$UIdeo <- relevel(factor(mis$UIdeo), ref = "Moderate")
arm_map <- c(Single_AI_Non_Biased = "Single AI Default",
             Single_AI_Biased     = "Single AI Biased",
             Dual_AI_Non_Biased   = "Dual AI Default",
             Dual_AI_Opposition   = "Dual AI Opposition",
             Dual_AI_Balanced     = "Dual AI Balanced")
arm_levels_mis <- names(arm_map)
mis$ExperimentType <- factor(mis$ExperimentType, levels = arm_levels_mis)
cat(sprintf("\nmisinfo rows = %d, participants = %d\n", nrow(mis), length(unique(mis$UID))))

# ── x: post detection performance (their PART A lmer + emmeans) ───────────────
emm_options(lmer.df = "satterthwaite", lmerTest.limit = 20000, pbkrtest.limit = 20000)
perf_lmer <- lmer(PostPerformance ~ ExperimentType + PrePerformance +
                    as.factor(NID) + as.factor(UStanceLabel) + (1 | UID),
                  data = mis, na.action = na.omit)
mis_x <- eff_tab(emmeans(perf_lmer, ~ ExperimentType),
                 clean = function(x) unname(arm_map[x]))
S_mis_x <- sigma(perf_lmer)                 # their Cohen's d convention
cat("\n=== misinfo x: detection performance deviation from grand mean (eff) ===\n")
print(mis_x, digits = 4)

# ── y: PerceivedImproveCode — their PART B MCMCglmm, latent marginal means ────
# MCMCglmm errors on an NA in ANY fixed predictor, so filter on the full set
# (as fact-checking/code/forth_figure_d2.R does); this drops rows with a
# missing political-ideology response.
mis_pi <- mis[complete.cases(mis[, c("PerceivedImproveCode", "AICorrectness",
                                     "ExperimentType", "NID", "PrePerformance",
                                     "UIdeo", "UStanceLabel", "UID")]), ]
mis_pi$PerceivedImproveCode <- factor(mis_pi$PerceivedImproveCode, ordered = TRUE)
cat(sprintf("\nmisinfo perceived n = %d — fitting MCMCglmm (random ~UID)...\n", nrow(mis_pi)))
set.seed(123)
mc_mis <- MCMCglmm(PerceivedImproveCode ~ ExperimentType + as.factor(NID) +
                     PrePerformance + UIdeo + AICorrectness + as.factor(UStanceLabel),
                   random = ~ UID, family = "ordinal",
                   nitt = 55000, thin = 25, burnin = 5000,
                   # residual variance fixed (unidentified under family="ordinal");
                   # the UID random effect keeps a proper vague prior
                   prior = list(R = list(V = 1, fix = 1),
                                G = list(G1 = list(V = 1, nu = 0.002))),
                   data = mis_pi, verbose = FALSE)
postm <- as.matrix(mc_mis$Sol); cnm <- colnames(postm)
mean_pre <- mean(mis_pi$PrePerformance, na.rm = TRUE)
mode_nid <- names(sort(table(mis_pi$NID), decreasing = TRUE))[1]
mm_draw_mis <- sapply(arm_levels_mis, function(arm) {
  lp <- if ("(Intercept)" %in% cnm) postm[, "(Intercept)"] else rep(0, nrow(postm))
  if (arm != arm_levels_mis[1]) { an <- paste0("ExperimentType", arm)
    if (an %in% cnm) lp <- lp + postm[, an] }
  if ("PrePerformance" %in% cnm) lp <- lp + postm[, "PrePerformance"] * mean_pre
  nc <- paste0("as.factor(NID)", mode_nid); if (nc %in% cnm) lp <- lp + postm[, nc]
  lp
})
mis_y <- dev_tab(mm_draw_mis, unname(arm_map[arm_levels_mis]))
S_mis_y <- sqrt(mean(rowSums(as.matrix(mc_mis$VCV))))     # latent SD = sqrt(V_UID + V_units)
cat("\n=== misinfo y: perceived improvement — latent deviation from grand mean ===\n")
print(mis_y, digits = 3)

# ══ STANDARDIZE WITHIN DOMAIN-AXIS, ASSEMBLE MAP DATA ═════════════════════════
# estimates are already deviations from the domain grand mean; scale to SD units
std_axis <- function(tab, S) {
  out <- tab
  for (col in setdiff(names(tab), "cond")) out[[col]] <- tab[[col]] / S
  out
}
zx_inv <- std_axis(inv_x, S_inv_x); zy_inv <- std_axis(inv_y, S_inv_y)
zx_mis <- std_axis(mis_x, S_mis_x); zy_mis <- std_axis(mis_y, S_mis_y)
merge_xy <- function(zx, zy, domain) {
  names(zx)[-1] <- paste0("x_", names(zx)[-1]); names(zy)[-1] <- paste0("y_", names(zy)[-1])
  left_join(zx, zy, by = "cond") %>% mutate(domain = domain)
}
pos <- bind_rows(merge_xy(zx_inv, zy_inv, "Investment"),
                 merge_xy(zx_mis, zy_mis, "Fact-checking")) %>%
  mutate(cond    = factor(cond, levels = COND7),
         is_sub  = cond %in% c("Single AI Averse", "Single AI Seeking"),
         content = ifelse(cond %in% c("Single AI Default", "Dual AI Default"),
                          "Default", "Biased"),
         session = ifelse(grepl("^Dual", cond), "Dual AI", "Single AI"),
         short   = recode(as.character(cond),
                          "Single AI Default"  = "Single Default",
                          "Single AI Biased"   = "Single Biased",
                          "Single AI Averse"   = "Single Averse",
                          "Single AI Seeking"  = "Single Seeking",
                          "Dual AI Default"    = "Dual Default",
                          "Dual AI Opposition" = "Dual Opposition",
                          "Dual AI Balanced"   = "Dual Balanced"))
cat("\n=== standardized coordinates (SD from domain mean) ===\n")
print(pos %>% select(domain, cond, x_est, y_est) %>% arrange(domain, cond),
      row.names = FALSE, digits = 3)
write.csv(pos, file.path(DATA_DIR, "positioning_map_cross_domain_data.csv"),
          row.names = FALSE)   # cached estimates (also handy for slides/tables)

# ══ FIGURE ════════════════════════════════════════════════════════════════════
# domain color ramps (ladder gradient: 99% lightest -> 90% darkest)
# one color per domain: investment = PRGn dark green, misinfo = RdBu dark blue;
# marker SHAPE = content (circle = default, triangle = biased), white rim only.
# Session (single vs dual) is NOT encoded — identified by position.
DOM_COL <- c(Investment = "#1B7837", `Fact-checking` = "#2166AC")
SUB_COL <- "#7FBF7B"   # lighter PRGn green: Single-Biased breakdown (Averse/Seeking)

pos <- pos %>% arrange(domain, cond)
pos$col <- unname(DOM_COL[pos$domain])
pos$col[pos$is_sub] <- SUB_COL

# GRADIENT-FADE whiskers (ggdist-style gradient interval): each half-whisker is
# subdivided into small segments whose alpha follows the implied normal density,
# alpha = A0 * exp(-z^2/2), with z mapped piecewise through the 90/95/99 bounds
# (handles the asymmetric MCMC intervals). Same information as a 90/95/99
# ladder, drawn as a continuous fade — tails melt out instead of truncating.
ZL <- c(`90` = 1.645, `95` = 1.960, `99` = 2.576)
A0 <- 0.62; NSEG <- 44
fade_half <- function(est, b90, b95, b99, axis, fixed, col, cond) {
  dists <- abs(c(0, b90 - est, b95 - est, b99 - est))
  ends  <- seq(est, b99, length.out = NSEG + 1)
  mid   <- (head(ends, -1) + tail(ends, -1)) / 2
  z     <- approx(dists, c(0, ZL), xout = abs(mid - est), rule = 2)$y
  a     <- A0 * exp(-z^2 / 2)
  if (axis == "x") data.frame(x = head(ends, -1), xend = tail(ends, -1),
                              y = fixed, yend = fixed, a = a, col = col, cond = cond)
  else             data.frame(x = fixed, xend = fixed,
                              y = head(ends, -1), yend = tail(ends, -1),
                              a = a, col = col, cond = cond)
}
fade <- do.call(rbind, lapply(seq_len(nrow(pos)), function(i) { r <- pos[i, ]; out <- rbind(
  fade_half(r$x_est, r$x_lo90, r$x_lo95, r$x_lo99, "x", r$y_est, r$col, as.character(r$cond)),
  fade_half(r$x_est, r$x_hi90, r$x_hi95, r$x_hi99, "x", r$y_est, r$col, as.character(r$cond)),
  fade_half(r$y_est, r$y_lo90, r$y_lo95, r$y_lo99, "y", r$x_est, r$col, as.character(r$cond)),
  fade_half(r$y_est, r$y_hi90, r$y_hi95, r$y_hi99, "y", r$x_est, r$col, as.character(r$cond)) )
  out$domain <- r$domain; out }))
fade$is_sub <- fade$cond %in% c("Single AI Averse", "Single AI Seeking")

# MAIN map = the 10 primary conditions (sub-conditions live in the triad panel)
pos_main  <- pos  %>% filter(!is_sub)
fade_main <- fade %>% filter(!is_sub)

# NOTE: axes are ZOOMED to the point cloud to amplify condition separation;
# whiskers reaching the panel edge are TRUNCATED (full CIs are in the console
# output and positioning_map_cross_domain_data.csv). No in-figure labels —
# conditions are identified by color/shape/fill (labelled downstream in slides).
p <- ggplot(pos_main, aes(x = x_est, y = y_est)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey62", linewidth = 0.35) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey62", linewidth = 0.35) +
  geom_segment(data = fade_main, aes(x = x, xend = xend, y = y, yend = yend,
                                     color = I(col), alpha = I(a)),
               linewidth = 1.7, inherit.aes = FALSE) +
  geom_point(data = subset(pos_main, session == "Dual AI"),    # open outline marks the
             aes(x = x_est, y = y_est, color = I(col),         # DUAL-AI sessions; the
                 shape = I(ifelse(content == "Biased", 2, 1)), # outline matches the
                 size  = I(ifelse(content == "Biased", 7.6, 7.6))),  # marker shape
             stroke = 0.8, inherit.aes = FALSE) +
  geom_point(aes(shape = content, fill = I(col)),
             color = "white", stroke = 0.7, size = 4.4) +   # white rim, no black frame
  geom_point(aes(color = domain), alpha = 0, size = 4.4) +   # legend carrier only
  scale_shape_manual(values = c("Default" = 21, "Biased" = 24), name = NULL,
                     labels = c("Default" = "Default AI(s)",
                                "Biased"  = "Biased AI(s)")) +
  scale_color_manual(values = DOM_COL, name = NULL) +
  coord_cartesian(xlim = c(-0.125, 0.105), ylim = c(-0.70, 0.52)) +
  labs(x = "Objective Performance (SD from domain mean)",
       y = "Perceived Improvement (SD from domain mean)") +
  theme_classic() +
  theme(
    text = element_text(family = "Avenir", color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_text(family = "Avenir", size = 12, margin = margin(t = 8)),
    axis.title.y = element_text(family = "Avenir", size = 12, margin = margin(r = 8)),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(family = "Avenir", size = 9),
    legend.box.margin = margin(t = -6),
    plot.margin = margin(t = 12, r = 15, b = 6, l = 12)
  ) +
  guides(shape = guide_legend(override.aes = list(fill = "grey45", color = "white")),
         color = guide_legend(override.aes = list(alpha = 1, shape = 15, size = 4)))
print(p)

ggsave(file.path(FIG_DIR, "positioning_map_cross_domain.png"), p,
       width = 5.5, height = 5.6, dpi = 500)
cat(sprintf("\nSaved positioning_map_cross_domain.png in %s\n", SCRIPT_DIR))
cat("Encoding: color = domain (investment green #1B7837 / misinfo blue #2166AC);\n")
cat("          circle = default AI content, triangle = biased AI content;\n")
cat("          open ring = dual-AI session, plain = single-AI session.\n")
cat("Whiskers are gradient-fade intervals (alpha ~ implied density through the\n")
cat("90/95/99 bounds); faded tails may run past the panel edge (full CIs in CSV).\n")

# ══ TRIAD zoom panel: Single Averse / Single Seeking / Dual Balanced ══════════
# Companion to the main map (same encoding, same axes/units). A dashed segment
# connects the two one-sided single biases; the grey cross marks their midpoint.
# Dual Balanced (ringed dark-green triangle) sitting ON the midpoint is the
# mixture-linearity result (Balanced - mid(A,S): x p = .71; see triad contrasts).
TRI_CONDS <- c("Single AI Averse", "Single AI Seeking", "Dual AI Balanced")
pos_tri  <- pos  %>% filter(cond %in% TRI_CONDS & domain == "Investment")
fade_tri <- fade %>% filter(cond %in% TRI_CONDS & domain == "Investment")
sa <- pos_tri[pos_tri$cond == "Single AI Averse", ]
ss <- pos_tri[pos_tri$cond == "Single AI Seeking", ]
xr_t <- range(c(pos_tri$x_lo99, pos_tri$x_hi99)); xp <- 0.04 * diff(xr_t)
yr_t <- range(c(pos_tri$y_lo99, pos_tri$y_hi99)); yp <- 0.04 * diff(yr_t)

p_tri <- ggplot(pos_tri, aes(x = x_est, y = y_est)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey62", linewidth = 0.35) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey62", linewidth = 0.35) +
  geom_segment(data = fade_tri, aes(x = x, xend = xend, y = y, yend = yend,
                                    color = I(col), alpha = I(a)),
               linewidth = 1.7, inherit.aes = FALSE) +
  annotate("segment", x = sa$x_est, y = sa$y_est, xend = ss$x_est, yend = ss$y_est,
           linetype = "22", color = "grey45", linewidth = 0.45) +
  annotate("point", x = (sa$x_est + ss$x_est) / 2, y = (sa$y_est + ss$y_est) / 2,
           shape = 3, size = 2.6, stroke = 0.8, color = "grey35") +
  geom_point(data = subset(pos_tri, session == "Dual AI"),
             aes(x = x_est, y = y_est, color = I(col)),
             shape = 2, size = 7.6, stroke = 0.8, inherit.aes = FALSE) +
  geom_point(aes(shape = content, fill = I(col)),
             color = "white", stroke = 0.7, size = 4.4) +
  scale_shape_manual(values = c("Default" = 21, "Biased" = 24), guide = "none") +
  coord_cartesian(xlim = c(xr_t[1] - xp, xr_t[2] + xp),
                  ylim = c(yr_t[1] - yp, yr_t[2] + yp)) +
  labs(x = "Objective Performance",
       y = "Perceived Improvement") +
  theme_classic() +
  theme(
    text = element_text(family = "Avenir", color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_text(family = "Avenir", size = 11, margin = margin(t = 8)),
    axis.title.y = element_text(family = "Avenir", size = 11, margin = margin(r = 8)),
    panel.grid = element_blank(),
    legend.position = "none",
    plot.margin = margin(t = 12, r = 12, b = 8, l = 12)
  )
print(p_tri)

ggsave(file.path(FIG_DIR, "positioning_map_triad.png"), p_tri,
       width = 2.5, height = 2.5, dpi = 500)
cat(sprintf("\nSaved positioning_map_triad.png in %s\n", SCRIPT_DIR))
cat("Triad panel: light green = Single Averse / Single Seeking; ringed dark green\n")
cat("triangle = Dual Balanced; grey cross = Averse-Seeking midpoint.\n")
