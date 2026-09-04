# ==============================================================================
# advisor_selection_dual.R
# "Can users select wisely when advisors conflict?" -- investment analog of the
# fact-checking selection analysis.
#
# When the two assistants recommend DIFFERENT portfolios, the participant's task
# is not whether to take advice but WHICH advice to take. Selection is measured
# behaviourally: which assistant's recommended portfolio is the participant's
# POST-interaction allocation closer to (L1 distance over the 25 normalized
# weights)? "Correct" selection = moving toward the assistant whose own
# recommended portfolio realized the higher Active M2 on that participant's
# evaluation window.
#
# Cues tested against accuracy (each a binary "which advisor does this favour"):
#   prior agreement : the advisor nearer the participant's PRE portfolio
#   congeniality    : the advisor whose persona sits on the participant's own
#                     side of the risk scale (calibrated AI_POSITIONS vs R_i)
#   decisiveness    : the advisor with the more concentrated recommendation (HHI)
#
# Oracle bound: realized post M2 vs (a) keeping the pre portfolio, (b) adopting
# the better advisor's portfolio on every conflicting trial.
#
# Inputs: dual_portfolios.csv, dual_active_m2.csv, dual_covariates.csv,
#         daily_returns.csv (), ../ai_portfolios_dual_extracted.csv
#   setwd("investment/code"); source("_setup.R"); source("advisor_selection_dual.R")
# ==============================================================================
suppressPackageStartupMessages({ library(dplyr) })

.args <- commandArgs(FALSE); .f <- sub("^--file=", "", .args[grep("^--file=", .args)])
SCRIPT_DIR <- if (length(.f)) dirname(normalizePath(.f)) else getwd()
NB_DIR <- DATA_DIR
rd  <- function(f) read.csv(file.path(DATA_DIR, f), stringsAsFactors = FALSE)
rdn <- function(f) read.csv(file.path(DATA_DIR, f),     stringsAsFactors = FALSE)

ASSETS <- c("SHY","IEF","LQD","GLD","VNQ","SPY","XLF","XLE","XLI","XLP","XLU",
            "AAPL","MSFT","AMZN","GOOGL","NVDA","TSLA","SHOP","SNOW","PLTR","DKNG",
            "RIVN","CRSP","BTC","ETH")
AI_POS <- setNames(c(0.0000, 0.2080, 0.5600, 0.8350, 1.0000, 0.3240),
                   c("extremely risk-averse", "somewhat risk-averse", "risk-neutral",
                     "somewhat risk-seeking", "extremely risk-seeking", ""))

nrm <- function(M) { s <- rowSums(M, na.rm = TRUE); M[is.na(M)] <- 0
                     M / ifelse(s > 0, s, NA) }
L1  <- function(A, B) rowSums(abs(A - B))
HHI <- function(M) rowSums(M^2)
active_m2 <- function(w, R) {
  s <- sum(w, na.rm = TRUE); if (!is.finite(s) || s <= 0) return(NA_real_)
  w <- ifelse(is.na(w), 0, w) / s
  a <- as.numeric(R %*% w) - R[, "SPY"]; sa <- sd(a); sm <- sd(R[, "SPY"])
  if (is.na(sa) || sa == 0 || sm == 0) return(NA_real_)
  (mean(a) / sa) * sm * sqrt(252) }
score <- function(W, starts, ret) vapply(seq_len(nrow(W)), function(i) {
  Ri <- as.matrix(ret[ret$Date >= starts[i] & ret$Date <= (starts[i] + 14), ASSETS, drop = FALSE])
  if (nrow(Ri) < 3) NA_real_ else active_m2(as.numeric(W[i, ]), Ri) }, numeric(1))

ret <- rd("daily_returns.csv"); ret$Date <- as.Date(ret$Date)
meta <- rd("dual_active_m2.csv"); meta$eval_start <- as.Date(meta$eval_start)
aid <- rdn("ai_portfolios_dual_extracted.csv"); names(aid)[names(aid) == "pid"] <- "participantId"
d <- rd("dual_portfolios.csv") %>%
  inner_join(meta %>% select(participantId, eval_start,
                             persona_ai1, persona_ai2), by = "participantId") %>%
  inner_join(rd("dual_covariates.csv") %>% select(participantId, risk_pref_score),
             by = "participantId") %>%
  inner_join(aid %>% select(participantId, ai1_bias, ai2_bias,
                            all_of(c(paste0("ai1_w_", ASSETS), paste0("ai2_w_", ASSETS)))),
             by = "participantId")

PRE  <- nrm(as.matrix(d[, paste0("pre_w_",  ASSETS)]))
POST <- nrm(as.matrix(d[, paste0("post_w_", ASSETS)]))
A1   <- nrm(as.matrix(d[, paste0("ai1_w_",  ASSETS)]))
A2   <- nrm(as.matrix(d[, paste0("ai2_w_",  ASSETS)]))
ok <- complete.cases(PRE, POST, A1, A2)
d <- d[ok, ]; PRE <- PRE[ok, ]; POST <- POST[ok, ]; A1 <- A1[ok, ]; A2 <- A2[ok, ]

d$m2_pre  <- score(PRE,  d$eval_start, ret); d$m2_post <- score(POST, d$eval_start, ret)
d$m2_ai1  <- score(A1,   d$eval_start, ret); d$m2_ai2  <- score(A2,   d$eval_start, ret)
d$gap_adv <- L1(A1, A2)                              # how far apart the advice is
d$gap_q   <- abs(d$m2_ai1 - d$m2_ai2)                # how far apart the quality is
d$better  <- ifelse(d$m2_ai1 > d$m2_ai2, 1, 2)       # which advisor was right
d$chosen  <- ifelse(L1(POST, A1) < L1(POST, A2), 1, 2)
d$prior   <- ifelse(L1(PRE,  A1) < L1(PRE,  A2), 1, 2)
p1 <- unname(AI_POS[ifelse(is.na(d$ai1_bias), "", d$ai1_bias)])
p2 <- unname(AI_POS[ifelse(is.na(d$ai2_bias), "", d$ai2_bias)])
d$congenial <- ifelse(abs(p1 - d$risk_pref_score) < abs(p2 - d$risk_pref_score), 1, 2)
d$decisive  <- ifelse(HHI(A1) > HHI(A2), 1, 2)
d$followed_better <- as.integer(d$chosen == d$better)

BIASED <- c("dual_opposition", "dual_balanced")
GAP_MIN <- 0.30                                     # advisors must actually differ
cat(sprintf("dual sample with both recommendations and both outcomes: %d\n",
            sum(complete.cases(d[, c("m2_pre","m2_post","m2_ai1","m2_ai2")]))))
cat(sprintf("advisor-distance quartiles: %s\n",
            paste(round(quantile(d$gap_adv, c(.25,.5,.75)), 3), collapse = ", ")))

cf <- d %>% filter(is.finite(m2_ai1), is.finite(m2_ai2), is.finite(m2_post),
                   gap_adv >= GAP_MIN, gap_q > 0)
cat(sprintf("\nCONFLICTING-ADVICE TRIALS (L1 gap >= %.2f, quality gap > 0): n = %d\n",
            GAP_MIN, nrow(cf)))
print(table(cf$dual_condition))

ci <- function(x) { x <- x[!is.na(x)]; b <- binom.test(sum(x), length(x)); sprintf(
  "%.1f%% [%.1f, %.1f], P = %s (n = %d)", 100*mean(x), 100*b$conf.int[1],
  100*b$conf.int[2], format.pval(b$p.value, digits = 3), length(x)) }

cat("\n=== followed the more accurate advisor ===\n")
cat(sprintf("  all conflicting trials      : %s\n", ci(cf$followed_better)))
for (g in BIASED) { s <- cf %>% filter(dual_condition == g)
  cat(sprintf("  %-27s : %s\n", g, ci(s$followed_better))) }
s <- cf %>% filter(dual_condition == "dual_nonbiased")
cat(sprintf("  %-27s : %s\n", "dual_nonbiased", ci(s$followed_better)))

cat("\n=== sensitivity to how far apart the advice is ===\n")
for (g in c(0.30, 0.50, 0.75, 1.00)) { s <- d %>% filter(gap_adv >= g, gap_q > 0, is.finite(m2_post))
  cat(sprintf("  L1 gap >= %.2f : %s\n", g, ci(s$followed_better))) }

cat("\n=== does a larger QUALITY gap help selection? ===\n")
m <- glm(followed_better ~ scale(gap_q), data = cf, family = binomial)
cs <- summary(m)$coefficients
cat(sprintf("  logit b = %.3f (SE %.3f), P = %.3g  [per SD of quality gap]\n",
            cs[2,1], cs[2,2], cs[2,4]))
cf$q_tercile <- cut(cf$gap_q, quantile(cf$gap_q, 0:3/3), include.lowest = TRUE,
                    labels = c("small","medium","large"))
print(cf %>% group_by(q_tercile) %>%
      summarise(n = n(), followed_better = sprintf("%.1f%%", 100*mean(followed_better)),
                .groups = "drop") %>% as.data.frame(), row.names = FALSE)

# congeniality is only defined where the two personas sit at different positions
cf$congenial[cf$dual_condition == "dual_nonbiased"] <- NA
cat("\n=== what DID participants follow? (cue-following rates) ===\n")
for (cue in c("prior","congenial","decisive")) {
  x <- as.integer(cf$chosen == cf[[cue]])
  cat(sprintf("  followed the %-10s advisor : %s\n", cue, ci(x))) }
cf$cue_prior <- as.integer(cf$prior == cf$better)
cf$cue_cong  <- as.integer(cf$congenial == cf$better)
cf$cue_dec   <- as.integer(cf$decisive == cf$better)
cat("\n  accuracy of selection when each cue happens to point the right way:\n")
for (cue in c("prior","cong","dec")) {
  a <- cf[[paste0("cue_", cue)]]; fb <- cf$followed_better
  cat(sprintf("  %-5s -> BETTER advisor: %.1f%% correct (n=%d) | -> WORSE advisor: %.1f%% correct (n=%d)\n",
              cue, 100*mean(fb[which(a == 1)]), sum(a == 1, na.rm = TRUE),
              100*mean(fb[which(a == 0)]), sum(a == 0, na.rm = TRUE))) }
mj <- glm(followed_better ~ cue_prior + cue_cong + cue_dec, data = cf, family = binomial)
cat("\n  joint logistic model (biased arms only, congeniality defined):\n")
print(round(summary(mj)$coefficients, 4))

cat("\n=== oracle bound on conflicting trials (Active M2, %) ===\n")
cat(sprintf("  ignore both advisors (keep PRE portfolio) : %+.3f\n", 100*mean(cf$m2_pre)))
cat(sprintf("  what participants achieved (POST)         : %+.3f\n", 100*mean(cf$m2_post)))
cat(sprintf("  always adopt the BETTER advisor           : %+.3f\n",
            100*mean(pmax(cf$m2_ai1, cf$m2_ai2))))
cat(sprintf("  always adopt the WORSE advisor            : %+.3f\n",
            100*mean(pmin(cf$m2_ai1, cf$m2_ai2))))
capt <- (mean(cf$m2_post) - mean(cf$m2_pre)) /
        (mean(pmax(cf$m2_ai1, cf$m2_ai2)) - mean(cf$m2_pre))
cat(sprintf("  share of the available margin captured    : %.1f%%\n", 100*capt))
