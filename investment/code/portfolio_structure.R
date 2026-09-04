# ==============================================================================
# portfolio_structure.R
# CROSS-PARTICIPANT portfolio diversity (homogenization), by treatment group.
#
# Question: does biased AI make a group's portfolios MORE diverse across
# participants than they were BEFORE treatment (vs the unbiased Default AI)?
#
# Measure: cross-participant dispersion = mean pairwise L1 distance among a group's
#   row-normalized 25-asset portfolios.  Treatment effect = change POST - PRE.
#   (>0 = group diversified;  <0 = group converged/homogenized.)
#
# Groups: Averse (Som+Ext Risk-Averse), Default, Seeking (Som+Ext Risk-Seeking).
#   Risk-Neutral is EXCLUDED.
# Inference: bootstrap CI on the change; pairwise between-group permutation tests
#   (Averse-Default, Seeking-Default, Seeking-Averse) and paired vs-zero tests.
#   Permutation p = (count+1)/(B+1) (Phipson & Smyth 2010 -> no p=0, FDR-safe);
#   BH-FDR within each 3-test family (pairwise; vs-zero).
#
# Input (notebook §13 export):
#   participant_portfolios.csv  (participantId, wave, ai_group, pre_w_* x25, post_w_* x25)
#   setwd("investment/code"); source("_setup.R"); source("portfolio_structure.R")
# ==============================================================================
suppressPackageStartupMessages({ library(ggplot2); library(dplyr); library(scales) })

.args <- commandArgs(FALSE); .f <- sub("^--file=", "", .args[grep("^--file=", .args)])
SCRIPT_DIR <- if (length(.f)) dirname(normalizePath(.f)) else getwd()
rd <- function(f) { p <- file.path(DATA_DIR, f); if (!file.exists(p)) p <- file.path(DATA_DIR, f)
                    read.csv(p, check.names = FALSE, stringsAsFactors = FALSE) }

ASSETS <- c("SHY","IEF","LQD","GLD","VNQ","SPY","XLF","XLE","XLI","XLP","XLU",
            "AAPL","MSFT","AMZN","GOOGL","NVDA","TSLA","SHOP","SNOW","PLTR","DKNG",
            "RIVN","CRSP","BTC","ETH")
PREW <- paste0("pre_w_", ASSETS); POSTW <- paste0("post_w_", ASSETS)

d <- rd("participant_portfolios.csv"); stopifnot(all(c(PREW, POSTW) %in% names(d)))

# 3 groups (Neutral excluded)
d$grp <- with(d, ifelse(ai_group %in% c("Extremely Risk-Averse","Somewhat Risk-Averse"), "Averse",
                 ifelse(ai_group %in% c("Extremely Risk-Seeking","Somewhat Risk-Seeking"), "Seeking",
                 ifelse(ai_group == "Default", "Default", NA_character_))))
d <- d[!is.na(d$grp), ]

norm_rows <- function(M) { M[is.na(M)] <- 0; M[M < 0] <- 0; rs <- rowSums(M); M[rs <= 0, ] <- NA_real_; M / rs }
Wpre  <- norm_rows(as.matrix(d[, PREW]))
Wpost <- norm_rows(as.matrix(d[, POSTW]))
ok <- is.finite(rowSums(Wpre)) & is.finite(rowSums(Wpost))
d <- d[ok, ]; Wpre <- Wpre[ok, ]; Wpost <- Wpost[ok, ]

disp  <- function(M, idx) if (length(idx) < 2) NA_real_ else mean(dist(M[idx, , drop = FALSE], method = "manhattan"))
delta <- function(idx) disp(Wpost, idx) - disp(Wpre, idx)

GROUPS <- c("Averse", "Default", "Seeking")
gidx <- lapply(GROUPS, function(g) which(d$grp == g)); names(gidx) <- GROUPS

# ── point estimates ──────────────────────────────────────────────────────────
res <- data.frame(grp = GROUPS,
  n        = sapply(gidx, length),
  pre_disp = sapply(gidx, function(ix) disp(Wpre, ix)),
  post_disp= sapply(gidx, function(ix) disp(Wpost, ix)),
  d_change = sapply(gidx, delta))

# ── bootstrap CI on the change (resample participants within group) ──────────
set.seed(1); B <- 1000
boot_ci <- function(ix) {
  bs <- replicate(B, { s <- sample(ix, length(ix), replace = TRUE)
    mean(dist(Wpost[s, , drop = FALSE], "manhattan")) - mean(dist(Wpre[s, , drop = FALSE], "manhattan")) })
  quantile(bs, c(.025, .975), na.rm = TRUE)
}
ci <- sapply(gidx, boot_ci)
res$lo <- ci[1, ]; res$hi <- ci[2, ]

# ── pairwise between-group permutation tests (valid group-level) ─────────────
# Shuffle labels within the two compared groups; two-sided on the difference in
# d_change. p = (count+1)/(B+1)  (Phipson & Smyth 2010 -> never exactly 0).
perm_pair <- function(g1, g2) {
  obs <- delta(gidx[[g1]]) - delta(gidx[[g2]])
  ab <- c(gidx[[g1]], gidx[[g2]]); labs <- d$grp[ab]; cnt <- 0
  for (b in seq_len(B)) {
    pl <- sample(labs); i1 <- ab[pl == g1]; i2 <- ab[pl == g2]
    dl <- delta(i1) - delta(i2)
    if (!is.na(dl) && abs(dl) >= abs(obs)) cnt <- cnt + 1
  }
  c(diff = obs, p = (cnt + 1) / (B + 1))
}
PAIRS <- list(c("Averse", "Default"), c("Seeking", "Default"), c("Seeking", "Averse"))
pw <- do.call(rbind, lapply(PAIRS, function(pr) {
  v <- perm_pair(pr[1], pr[2])
  data.frame(comparison = paste(pr[1], "vs", pr[2]),
             diff_d_change = unname(v["diff"]), perm_p = unname(v["p"]))
}))
pw$p_fdr <- p.adjust(pw$perm_p, "fdr")          # BH across the 3 pairwise tests
res$perm_p_vs_Default <- c(pw$perm_p[1], NA_real_, pw$perm_p[2])  # Averse, Default, Seeking

# ── test each group's change vs ZERO (paired pre/post permutation) ───────────
# H0: the group's dispersion is unchanged pre->post. pre & post are PAIRED within
# a participant, so under H0 each participant's (pre, post) labels are exchangeable.
# Randomly swap pre<->post per participant, recompute d_change -> null centered on 0.
# Two-sided p = P(|d_change*| >= |observed|). Valid for this non-independent statistic.
perm_p0 <- function(g) {
  ix  <- gidx[[g]]; n <- length(ix); if (n < 2) return(NA_real_)
  obs <- delta(ix); cnt <- 0
  for (b in seq_len(B)) {
    sw <- runif(n) < 0.5
    Pp <- Wpost[ix, , drop = FALSE]; Pr <- Wpre[ix, , drop = FALSE]
    t2 <- Pp[sw, , drop = FALSE]; Pp[sw, ] <- Pr[sw, ]; Pr[sw, ] <- t2
    dl <- mean(dist(Pp, method = "manhattan")) - mean(dist(Pr, method = "manhattan"))
    if (!is.na(dl) && abs(dl) >= abs(obs)) cnt <- cnt + 1
  }
  (cnt + 1) / (B + 1)
}
res$perm_p_vs_zero <- sapply(as.character(res$grp), perm_p0)
res$p_vs_zero_fdr  <- p.adjust(res$perm_p_vs_zero, "fdr")   # BH across the 3 vs-zero tests

cat("=== Cross-participant dispersion (mean pairwise L1), by group ===\n")
cat("   d_change = POST - PRE  (>0 diversified, <0 homogenized)\n")
cat("   perm_p_vs_zero (+ BH-FDR across 3): does the group's change differ from 0?\n")
print(res, row.names = FALSE, digits = 3)

cat("\n=== pairwise between-group comparisons of d_change (permutation; BH-FDR across 3) ===\n")
print(pw, row.names = FALSE, digits = 3)

cat(sprintf("\nSeeking change-vs-zero: d_change = %+.4f, 95%% boot CI [%+.4f, %+.4f], perm p(vs 0) = %.3f\n",
            res$d_change[res$grp == "Seeking"], res$lo[res$grp == "Seeking"],
            res$hi[res$grp == "Seeking"], res$perm_p_vs_zero[res$grp == "Seeking"]))

sig <- function(p) ifelse(is.na(p), "", ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", "ns"))))

# ================= bar plot (Nature style) ==================================
res$grp <- factor(res$grp, levels = GROUPS)
gcol <- c("Averse" = "#59A14F", "Default" = "#79706E", "Seeking" = "#B07AA1")
off  <- 0.06 * diff(range(c(res$lo, res$hi, 0), na.rm = TRUE))
res$lab_y  <- ifelse(res$d_change >= 0, res$hi + off, res$lo - off)
res$star   <- sig(res$perm_p_vs_Default)

nature_theme <- theme_classic() +
  theme(
    text = element_text(family = "Avenir", size = 8),
    plot.title = element_text(family = "Avenir", size = 10, face = "bold", hjust = 0),
    axis.title = element_text(family = "Avenir", size = 9, face = "plain"),
    axis.text = element_text(family = "Avenir", size = 8, color = "black"),
    axis.text.y = element_text(family = "Avenir", size = 9, color = "black", face = "plain"),
    axis.line = element_line(color = "black", linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.ticks.length.x = unit(0.15, "cm"),
    axis.ticks.length = unit(0.15, "cm"),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 10)
  )

p <- ggplot(res, aes(x = grp, y = d_change, fill = grp)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  geom_col(alpha = 0.85, width = 0.6) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.15, linewidth = 0.7, alpha = 0.9) +
  geom_point(color = "black", size = 3, shape = 18) +
  geom_text(aes(y = lab_y, label = sprintf("%+.3f", d_change)), size = 4, family = "Avenir") +
  scale_fill_manual(values = gcol) +
  scale_y_continuous(labels = label_number(accuracy = 0.01)) +
  labs(x = NULL, y = "Change in Cross-participant Portfolio Diversity") +
  nature_theme +
  theme(
    legend.position = "none",
    text = element_text(family = "Avenir", size = 12),
    axis.text = element_text(family = "Avenir", size = 12, color = "black"),
    axis.text.y = element_text(family = "Avenir", size = 10, color = "black", face = "plain"),
    axis.title.y = element_text(family = "Avenir", size = 12, color = "black", margin = margin(r=6)),
    # panel.border = element_blank()
  ) +
  ylim(-0.3, 0.01)

print(p)

# Avenir needs a font-aware device; cairo_pdf handles it. (Drop family="Avenir" if it errors.)
# ggsave(file.path(FIG_DIR, "cross_participant_diversity.pdf"), p, width = 5.2, height = 4.2, device = cairo_pdf)
ggsave(file.path(FIG_DIR, "cross_participant_diversity.png"), p, width = 3.8, height = 4., dpi = 500)
cat(sprintf("\nSaved cross_participant_diversity.{pdf,png} in %s\n", SCRIPT_DIR))
