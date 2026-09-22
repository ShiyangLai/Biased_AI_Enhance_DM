# ==============================================================================
# bias_magnitude_outcomes.R — educational-AI replication of
# paper/reference_code/bias_magnitude_outcomes.R (the investment-study sibling:
# bias magnitude ladder + magnitude x direction SPLIT panels with nested
# 90/95/99 CI boxes fanning out from the shared Default point).
#
# Mapping (reference -> this study):
#   Bias magnitude No/Moderate/Strong -> bias PRESENCE: No Bias (default) vs
#       Biased (novelty + reliability pooled). This design has one bias dose,
#       so the magnitude ladder collapses to a single step; the signature
#       direction split is preserved at that step.
#   Direction Risk-Averse / Risk-Seeking -> Reliability / Novelty
#       (averse ~ cautious/standard ~ reliability; seeking ~ exploratory ~ novelty)
#   wave FE                        -> week FE (weeks 1-9)
#   participant (1 obs each; HC3)  -> student-week panel with a STUDENT FIXED
#       EFFECT (within-student estimator -- treatment is sprinkled within
#       students, so each is their own control) + student-clustered SEs; the FE
#       absorbs prior_prog_exp/prior_ml_exp and every time-invariant student trait
#       (deviation: the reference is cross-sectional with one obs per participant)
#   pre_active_m2_ann (baseline)   -> prior programming + prior ML/AI experience
#   post Active M^2 (performance)  -> coding_hg (coding: Gemini+
#       human 2-grader mean, from data/scores_all_graders_student_week.csv)
#   n_reply_turns (engagement)     -> conv_round, modeled as log(1+turns)
#       (deviation: raw in the reference; ours is heavily right-skewed) and
#       use-conditioned (turns exist only for weeks with conversations)
#   post confidence (ordinal)      -> perceived HELPFULNESS: post-assignment survey
#       "usefulness" item (1-7), modeled linearly within student (ordinal can't carry
#       39 student dummies at n=91). Replaces the earlier perceived-improvement panel;
#       from data/post_survey_student_week.csv.
#   perceived improvement (console only) -> memo performance: memo_regrade (mean of
#       the available graders, from data/memo_regrade_student_week.csv). Memo shares
#       the 0-10 assignment grading scale with coding, so panel 1 is ONE frame split
#       by a vertical rule -- coding (left) | memo (right) -- on one shared y axis
#       ("Assignment Performance") rather than two differently-scaled side axes.
#   16 investment covariates omnibus check -> a balance check on the two prior-
#       experience covariates is printed for reference, but the student fixed
#       effect already absorbs them (and all other time-invariant student traits),
#       so no covariates enter the panel models.
#
# Inputs : data/bm_student_week.csv, data/scores_all_graders_student_week.csv,
#          data/post_survey_student_week.csv, data/memo_regrade_student_week.csv
# Outputs: figures/bm_marginal_means.csv, figures/bm_contrasts_hedges.csv,
#          figures/bias_magnitude_outcomes.png
#   setwd("education/code"); source("_setup.R"); source("bias_magnitude_outcomes.R")
# ==============================================================================
suppressPackageStartupMessages({
  library(ggplot2); library(emmeans); library(dplyr); library(sandwich)
  library(lmtest); library(scales); library(patchwork); library(readr)
})
set.seed(123)

# Resolve this script's own folder so data/ and figures/ resolve whether it is run
# via Rscript, sourced in RStudio, or pasted line-by-line from any working directory.
IN <- DATA_DIR; OUT <- FIG_DIR
# Route any implicit graphics (e.g. grid text measurement) to a throwaway PNG rather than
# the default pdf device (some R builds can't render custom fonts to pdf). Figures are PNG.

# Reliability-matched grader sets (same choice as plot_performance_treatment.R and
# bias_side_performance.R): coding = the two human graders + Gemini (equal per-grader
# weight, Gemini mean-aligned to the human scale; alpha ~ .76); memo = the two human
# graders only (LLM memo grades track humans at r ~ .3). Joined before the factor()
# cast so the integer week keys still match.
grades <- read_csv(file.path(IN, "coding_regrade_student_week.csv"), show_col_types = FALSE) %>%
  left_join(read_csv(file.path(IN, "scores_all_graders_student_week.csv"), show_col_types = FALSE) %>%
              dplyr::select(student_id, week, gem_asgn = gemini_asgn_score),
            by = c("student_id", "week"))
GEM_SHIFT <- mean(grades$coding_regrade[!is.na(grades$gem_asgn)], na.rm = TRUE) -
             mean(grades$gem_asgn[!is.na(grades$coding_regrade)], na.rm = TRUE)
grades <- grades %>%
  mutate(coding_hg = ifelse(is.na(gem_asgn), coding_regrade,
                            (2 * coding_regrade + gem_asgn + GEM_SHIFT) / 3),
         coding_hg = ifelse(is.na(coding_regrade), NA, coding_hg)) %>%
  dplyr::select(student_id, week, coding_hg)
cat(sprintf("coding composite: Gemini mean-alignment shift %+.3f\n", GEM_SHIFT))
# Panel 3 uses the post-assignment "usefulness" item (1-7) as a perceived-helpfulness proxy.
post <- read_csv(file.path(IN, "post_survey_student_week.csv"), show_col_types = FALSE) %>%
  dplyr::select(student_id, week, usefulness)
# Memo uses the re-graded sheet (mean of available graders). It shares the 0-10
# assignment scale with coding, so both live in panel 1 on one common y axis.
memo <- read_csv(file.path(IN, "memo_regrade_student_week.csv"), show_col_types = FALSE) %>%
  dplyr::select(student_id, week, memo_regrade)
d <- read_csv(file.path(IN, "bm_student_week.csv"), show_col_types = FALSE) %>%
  left_join(grades, by = c("student_id", "week")) %>%
  left_join(post, by = c("student_id", "week")) %>%
  left_join(memo, by = c("student_id", "week")) %>%
  mutate(BiasedCat = factor(bias_pooled, levels = c("No Bias", "Biased")),
         arm = factor(dplyr::recode(direction, None = "Default",
                                    Novelty = "Novelty", Reliability = "Reliability"),
                      levels = c("Default", "Novelty", "Reliability")),
         week = factor(week), student_id = as.factor(student_id),
         conv_round_log = log1p(conv_round))
cat("N by bias presence:\n"); print(table(d$BiasedCat))
cat("N by arm:\n"); print(table(d$arm))
LV <- c("No Bias", "Biased")
ARMS3 <- c("Default", "Novelty", "Reliability")
hedges_J <- function(n1, n2) { d0 <- n1 + n2 - 2; ifelse(d0 > 0, 1 - 3 / (4 * d0 - 1), 1) }

# ── covariates + omnibus imbalance check (reference lines 74-93) ──────────────
COVARS <- c("prior_prog_exp", "prior_ml_exp")
for (cv in COVARS) { d[[cv]] <- as.numeric(d[[cv]]); d[[cv]][is.na(d[[cv]])] <- median(d[[cv]], na.rm = TRUE) }
bal <- do.call(rbind, lapply(COVARS, function(cv) {
  av <- anova(lm(d[[cv]] ~ d$arm))
  data.frame(covariate = cv, eta2 = av$`Sum Sq`[1] / sum(av$`Sum Sq`), anova_p = av$`Pr(>F)`[1])
}))
bal$flag <- ifelse(bal$anova_p < .05 | bal$eta2 >= .01, "IMBALANCED", "")
cat("\n=== 3-arm imbalance check (flag if ANOVA p<.05 or eta^2>=.01) ===\n")
print(bal, row.names = FALSE, digits = 3)
cat("(diagnostic only; the panel models carry a STUDENT FIXED EFFECT, which absorbs\n",
    "these prior-experience covariates and every other time-invariant student trait)\n")

add_cis_norm <- function(mean, se) data.frame(
  lo90 = mean - se * qnorm(.95),  hi90 = mean + se * qnorm(.95),
  lo95 = mean - se * qnorm(.975), hi95 = mean + se * qnorm(.975),
  lo99 = mean - se * qnorm(.995), hi99 = mean + se * qnorm(.995))

means_out <- list(); contrasts_out <- list()

# student-FE lm + student-clustered emmeans for one factor; returns viz frame + CSV rows.
# The student fixed effect = the WITHIN-student estimator (each student their own
# control) and absorbs the time-invariant prior-experience covariates.
lm_block <- function(dat, ycol, fac, lvls, label, key, analysis) {
  m <- lm(as.formula(paste(ycol, "~", fac, "+ week + factor(student_id)")), data = dat)
  vc <- vcovCL(m, cluster = dat$student_id[as.integer(rownames(model.frame(m)))])
  em <- emmeans(m, as.formula(paste("~", fac)), vcov. = vc)
  s <- as.data.frame(summary(em))
  cat(sprintf("\n=== %s — marginal means (student FE, student-clustered) ===\n", label))
  print(s, digits = 4)
  pw <- as.data.frame(summary(pairs(em, adjust = "none"), infer = TRUE))
  pw$p_raw <- pw$p.value
  # FDR by family: biased-vs-reference (vs Default / No Bias) is the focal family
  # (2 contrasts for the 3-arm split, 1 for pooled); any biased-vs-biased contrast
  # is a separate family. Keeps these p_fdr identical to bm_default_contrasts.csv.
  is_ref <- grepl(lvls[1], as.character(pw$contrast), fixed = TRUE)
  pw$p_fdr <- NA_real_
  pw$p_fdr[is_ref] <- p.adjust(pw$p_raw[is_ref], "fdr")
  if (any(!is_ref)) pw$p_fdr[!is_ref] <- p.adjust(pw$p_raw[!is_ref], "fdr")
  nb <- table(dat[[fac]])
  ysd <- sd(dat[[ycol]], na.rm = TRUE)   # standardize on raw outcome SD (FE residual understates it)
  pw$hedges_g <- mapply(function(est, ct) {
    g <- strsplit(gsub("[()]", "", as.character(ct)), " - ")[[1]]
    (est / ysd) * hedges_J(nb[[g[1]]], nb[[g[2]]]) }, pw$estimate, pw$contrast)
  cat("--- pairwise + Hedges' g (FDR within family: biased-vs-Default focal) ---\n")
  print(pw[, c("contrast", "estimate", "SE", "p_fdr", "hedges_g")], row.names = FALSE, digits = 3)
  means_out[[paste(key, analysis)]] <<- data.frame(
    outcome = key, analysis = analysis, level = as.character(s[[fac]]),
    emmean = s$emmean, se = s$SE, n_obs = nrow(model.frame(m)))
  contrasts_out[[paste(key, analysis)]] <<- data.frame(
    outcome = key, analysis = analysis, model = "student FE + clustered SE",
    contrast = as.character(pw$contrast), estimate = pw$estimate, se = pw$SE,
    p_raw = pw$p_raw, p_fdr = pw$p_fdr, hedges_g = pw$hedges_g)
  data.frame(level = as.character(s[[fac]]), emmean = s$emmean, add_cis_norm(s$emmean, s$SE))
}

# ══ 1. Coding performance (ANCOVA analog; score>0) — Gemini+human combined grade ══
dp <- d %>% filter(!is.na(coding_hg), coding_hg > 0)
viz_perf  <- lm_block(dp, "coding_hg", "BiasedCat", LV,
                      "1. Coding performance (Gemini+human; pooled bias presence)", "coding", "pooled")
viz_perf3 <- lm_block(dp, "coding_hg", "arm", ARMS3,
                      "1b. Coding performance (Gemini+human; direction split)", "coding", "arms3")

# ══ 2. Conversation turns, log(1+x), use-conditioned ══════════════════════════
dr <- d %>% filter(conv_round > 0)
viz_rt  <- lm_block(dr, "conv_round_log", "BiasedCat", LV,
                    "2. Conversation turns, log(1+x) (pooled)", "turns", "pooled")
viz_rt3 <- lm_block(dr, "conv_round_log", "arm", ARMS3,
                    "2b. Conversation turns, log(1+x) (direction split)", "turns", "arms3")

# ══ 3. Perceived helpfulness (usefulness item, 1-7): pooled + direction split ══
# Alternative proxy for perceived improvement (post-assignment "usefulness"), same
# linear student-FE model (ordinal FE can't carry 39 student dummies at n=91).
# USE-CONDITIONED, as the reply-turn panel necessarily is: a student who never opened
# the assistant in a given week cannot have formed a view of it, so weeks with zero
# conversation are dropped. (Conditioning on use is post-treatment; the unconditioned
# ITT estimate is reported in the console block below and in the SI.)
dph <- d %>% filter(!is.na(usefulness), conv_round > 0)
viz_perc  <- lm_block(dph, "usefulness", "BiasedCat", LV,
                      "3. Perceived helpfulness (pooled)", "helpfulness", "pooled")
viz_perc3 <- lm_block(dph, "usefulness", "arm", ARMS3,
                      "3b. Perceived helpfulness (direction split)", "helpfulness", "arms3")
# unconditioned (ITT) companion for the SI: same models on every surveyed week
dpi <- d %>% filter(!is.na(usefulness))
cat("\n--- ITT companion: perceived helpfulness on ALL surveyed weeks (not plotted) ---\n")
invisible(lm_block(dpi, "usefulness", "BiasedCat", LV,
                   "3-ITT. Perceived helpfulness (pooled, unconditioned)", "helpfulness_itt", "pooled"))
invisible(lm_block(dpi, "usefulness", "arm", ARMS3,
                   "3b-ITT. Perceived helpfulness (direction split, unconditioned)", "helpfulness_itt", "arms3"))

# ══ 4. Memo performance (re-graded sheet) — shares panel 1 with coding ═══════
dm <- d %>% filter(!is.na(memo_regrade), memo_regrade > 0)
viz_memo  <- lm_block(dm, "memo_regrade", "BiasedCat", LV,
                      "4. Memo performance (re-grade; pooled bias presence)", "memo", "pooled")
viz_memo3 <- lm_block(dm, "memo_regrade", "arm", ARMS3,
                      "4b. Memo performance (re-grade; direction split)", "memo", "arms3")

write_csv(bind_rows(means_out), file.path(OUT, "bm_marginal_means.csv"))
write_csv(bind_rows(contrasts_out), file.path(OUT, "bm_contrasts_hedges.csv"))

# ── Default vs each biased arm: focal pairwise from the three-arm model, with
#    FDR applied over the two Default-vs-biased contrasts WITHIN each analysis.
#    Signs are harmonised to "biased - Default" (positive = biased arm higher).
OUTLAB <- c(coding = "Coding performance", turns = "Number of reply turns (log 1+x)",
            helpfulness = "Perceived Helpfulness", memo = "Memo performance")
focal <- bind_rows(contrasts_out) %>%
  filter(analysis == "arms3", grepl("Default", contrast)) %>%
  mutate(flip = grepl("^\\s*Default", contrast),
         biased = trimws(gsub("Default|-", "", contrast)),
         estimate = ifelse(flip, -estimate, estimate),
         hedges_g = ifelse(flip, -hedges_g, hedges_g)) %>%
  group_by(outcome) %>%
  mutate(p_fdr_default = p.adjust(p_raw, "fdr")) %>%
  ungroup() %>%
  transmute(outcome = OUTLAB[outcome], model,
            comparison = paste0(dplyr::recode(biased, Novelty = "Novelty-biased",
                                Reliability = "Reliability-biased"), " vs Default"),
            estimate, se, hedges_g, p_raw, p_fdr = p_fdr_default,
            sig = as.character(cut(p_fdr_default, c(-Inf, .001, .01, .05, .1, Inf),
                                   labels = c("***", "**", "*", ".", "ns"))))
cat("\n=== DEFAULT vs BIASED ARMS: pairwise from the three-arm model",
    "(Hedges' g; FDR over the 2 focal contrasts per analysis) ===\n")
print(as.data.frame(focal), row.names = FALSE, digits = 3)
write_csv(focal, file.path(OUT, "bm_default_contrasts.csv"))

# ══ figure: stacked SPLIT panels (reference lines 283-394) ════════════════════
# x = the collapsed magnitude ladder {Default, Biased}; at "Biased" the two
# directions fan out from the shared Default point. Reliability takes the
# reference's averse role, novelty the seeking role; both are drawn in the BrBG
# diverging family -- reliability BROWN, novelty TEAL (markers + dashed lines);
# ladders are one muted family per panel with tinted biased columns.
DIRCOL <- c(Reliability = "#8C510A", Novelty = "#01665E", None = "#666666")
tintc <- function(cc, f) colorRampPalette(c("white", cc))(101)[round(f * 100) + 1]
mk_star <- function(cx, cy, rx, ry, id) {
  ao <- pi / 2 + 2 * pi * (0:4) / 5; ai <- pi / 2 + pi / 5 + 2 * pi * (0:4) / 5
  a <- as.vector(rbind(ao, ai)); r <- rep(c(1, 0.382), 5)
  data.frame(x = cx + cos(a) * rx * r, y = cy + sin(a) * ry * r, g = id)
}
prep_split <- function(v, off = 0.16) {
  mp <- data.frame(level = ARMS3, xm = c(0, 1, 1),
                   dir = c("None", "Novelty", "Reliability"))
  v <- merge(v, mp, by = "level")
  v$x <- v$xm + ifelse(v$dir == "Reliability", -off, ifelse(v$dir == "Novelty", off, 0))
  v[order(v$xm, v$dir), ]
}
# box fills / marker colors / box widths for ONE ladder
style_split <- function(v, c90, c95, c99, mc0, w0, B2) {
  v$f90 <- c90; v$f95 <- c95; v$f99 <- c99
  bia <- v$dir != "None"
  v$f90[bia] <- sapply(v$f90[bia], tintc, f = 0.85)
  v$f95[bia] <- sapply(v$f95[bia], tintc, f = 0.85)
  v$f99[bia] <- sapply(v$f99[bia], tintc, f = 0.85)
  v$mcol <- unname(DIRCOL[v$dir])
  if (!is.null(mc0)) v$mcol[v$dir == "None"] <- mc0
  v$ocol <- ifelse(v$dir == "None", v$mcol, "white")
  v$msz <- ifelse(v$dir == "None", 0.8, 1)
  wmul <- ifelse(v$dir == "None", w0, 1)
  v$bw90 <- B2 * 1.4 * wmul; v$bw95 <- B2 * wmul; v$bw99 <- B2 * 0.7 * wmul
  v
}
# one ladder's layers (nested boxes + the two dashed direction lines + markers) at opacity a
ladder_layers <- function(v, shp, rx, ry, a = 1) {
  # a layer's `alpha` tints polygon/point FILL only, so fade the marker outline
  # explicitly -- otherwise the overlay's Default star (whose outline color equals
  # its fill, unlike the white-edged biased stars) keeps a solid edge
  if (a < 1) v$ocol <- alpha(v$ocol, a)
  L <- list(
    geom_rect(data = v, aes(xmin = x - bw99, xmax = x + bw99, ymin = lo99, ymax = hi99,
                            fill = I(f99)), alpha = a),
    geom_rect(data = v, aes(xmin = x - bw95, xmax = x + bw95, ymin = lo95, ymax = hi95,
                            fill = I(f95)), alpha = a),
    geom_rect(data = v, aes(xmin = x - bw90, xmax = x + bw90, ymin = lo90, ymax = hi90,
                            fill = I(f90)), alpha = a),
    geom_line(data = v[v$dir != "Novelty", ], aes(x = x, y = emmean),
              color = DIRCOL[["Reliability"]], linewidth = 0.9, linetype = "22", alpha = a),
    geom_line(data = v[v$dir != "Reliability", ], aes(x = x, y = emmean),
              color = DIRCOL[["Novelty"]], linewidth = 0.9, linetype = "22", alpha = a))
  if (identical(shp, "star")) {
    st <- do.call(rbind, lapply(seq_len(nrow(v)), function(i)
      mk_star(v$x[i], v$emmean[i], rx * v$msz[i], ry * v$msz[i], i)))
    st$mcol <- rep(v$mcol, each = 10); st$ocol <- rep(v$ocol, each = 10)
    c(L, list(geom_polygon(data = st, aes(x = x, y = y, group = g, fill = I(mcol),
                                          color = I(ocol)), linewidth = 0.3, alpha = a)))
  } else {
    c(L, list(geom_point(data = v, aes(x = x, y = emmean, fill = I(mcol), color = I(ocol),
                                       size = I(2.4 * msz)), shape = shp, stroke = 0.6, alpha = a)))
  }
}
# One panel. Supplying v2 overlays a SECOND outcome in the same frame as a
# TRANSPARENT ladder -- legitimate here because coding and memo share the 0-10
# assignment grading scale, so one y axis serves both. The overlay is paired just
# right of the primary at each x slot; the direction fan-out widens and the boxes
# narrow so both ladders fit inside the unchanged x limits, which keeps this panel
# aligned with the panels stacked below it.
panel_split <- function(v, ylab, shp, c90, c95, c99, acc = 0.01, show_x = TRUE,
                        ytit_r = 8, w0 = 1.3, mc0 = NULL, dlabels = FALSE,
                        v2 = NULL, key = NULL, a2 = 0.40,
                        # y headroom as a multiple of the data span; raise top_mult
                        # for more room above the ladders (significance brackets)
                        top_mult = 0.525, bot_mult = 0.225, ybreaks = waiver()) {
  duo <- !is.null(v2)
  off <- if (duo) 0.26  else 0.16    # direction fan-out at "Biased"
  B2  <- if (duo) 0.045 else 0.06    # box half-width unit
  nud <- if (duo) 0.085 else 0       # primary left of the slot, overlay right
  rx  <- if (duo) 0.038 else 0.05    # star radius
  v <- style_split(prep_split(v, off), c90, c95, c99, mc0, w0, B2); v$x <- v$x - nud
  if (duo) {
    v2 <- style_split(prep_split(v2, off), c90, c95, c99, mc0, w0, B2); v2$x <- v2$x + nud
  }
  yr <- range(c(v$lo99, v$hi99, if (duo) c(v2$lo99, v2$hi99))); ysp <- diff(yr)
  # star aspect: the panel is aspect.ratio = 1 over an x span of 1.95, so ry must be
  # scaled by the ACTUAL plotted y span (data + head/foot room). Using a fixed factor
  # here flattens the markers whenever top_mult/bot_mult change.
  ry <- rx * (ysp * (1 + top_mult + bot_mult)) / 1.95
  p <- ggplot() + c(if (duo) ladder_layers(v2, shp, rx, ry, a2) else list(),
                    ladder_layers(v, shp, rx, ry, 1))
  if (duo && !is.null(key)) {        # inline opaque/transparent key, upper right
    # anchor the rows to the PANEL TOP (yr[2] + top_mult * ysp) rather than to the
    # data, so the key stays in the corner when top_mult changes
    kx <- 1.10; kw <- 0.10; ky <- yr[2] + (top_mult - c(0.10, 0.21)) * ysp
    p <- p +
      annotate("rect", xmin = kx, xmax = kx + kw, ymin = ky - 0.030 * ysp,
               ymax = ky + 0.030 * ysp, fill = c95, alpha = c(1, a2)) +
      annotate("text", x = kx + kw + 0.05, y = ky, label = key, hjust = 0,
               size = 2.5, family = FONT, color = "black")
  }
  if (dlabels) {                     # direct series labels next to the split markers
    lab <- v[v$dir != "None", ]
    p <- p + annotate("text", x = lab$x + 0.10, y = lab$emmean, label = lab$dir,
                      color = unname(DIRCOL[lab$dir]), size = 2.6, family = FONT, hjust = 0)
  }
  p <- p +
    scale_x_continuous(breaks = 0:1, labels = c("Default", "Biased"),
                       limits = c(-0.35, 1.6), name = "AI Bias Magnitude") +
    # limits set explicitly (= the old 0.225/0.525 expansion) so the key sits INSIDE
    # the 1.5x headroom for later annotations rather than enlarging it
    scale_y_continuous(name = ylab, labels = function(y) number_format(accuracy = acc)(y),
                       breaks = ybreaks,
                       limits = c(yr[1] - bot_mult * ysp, yr[2] + top_mult * ysp),
                       expand = c(0, 0)) +
    theme_classic() +
    theme(aspect.ratio = 1,
          text = element_text(family = FONT, color = "black"),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
          axis.line = element_blank(),
          axis.title.x = element_text(family = FONT, size = 9, margin = margin(t = 8)),
          axis.title.y = element_text(family = FONT, size = 9, margin = margin(r = ytit_r)),
          axis.text = element_text(family = FONT, size = 8, color = "black"),
          axis.ticks = element_line(color = "black", linewidth = 0.4),
          axis.ticks.length = unit(2.5, "pt"), panel.grid = element_blank(),
          plot.margin = margin(t = 10, r = 15, b = 6, l = 10))
  if (!show_x) p <- p + theme(axis.title.x = element_blank(),
                              axis.text.x = element_blank(), axis.ticks.x = element_blank())
  p
}

p1 <- panel_split(viz_perf3, "Assignment Performance", "star",
                  "#228B22", "#66C266", "#BBE5BB",
                  acc = 0.01, show_x = FALSE, ytit_r = 8, mc0 = "#006400",
                  v2 = viz_memo3, key = c("Coding", "Memo"),
                  # wider range + headroom for the significance brackets; top_mult
                  # 0.799 puts the panel ceiling at ~11.25 (data hi99 = 10.03, span 1.53)
                  top_mult = 0.799, bot_mult = 0.35, ybreaks = seq(6, 12, 0.5))
p2 <- panel_split(viz_rt3, "log(1+Number of Reply Turns)", 21,
                  "#8B4513", "#A0522D", "#D2B48C",   # brown box family: saddle / sienna / tan
                  acc = 0.01, show_x = FALSE, ytit_r = 12, mc0 = "#654321")
p3 <- panel_split(viz_perc3, "Perceived Helpfulness", 22,
                  "#FF8C00", "#FFA500", "#FFB347",   # orange box family: dark orange / orange / peach
                  acc = 0.01, ytit_r = 12, mc0 = "#D2691E")
pc <- p1 / p2 / p3
pc
save_png(file.path(OUT, "bias_magnitude_outcomes.png"), pc, width = 4, height = 8, dpi = 500)
cat(sprintf("\nSaved bias_magnitude_outcomes.png, bm_marginal_means.csv, bm_contrasts_hedges.csv in %s\n", OUT))
cat("Done: bias_magnitude_outcomes.R\n")
