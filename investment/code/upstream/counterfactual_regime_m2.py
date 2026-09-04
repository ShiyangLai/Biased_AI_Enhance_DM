# ==============================================================================
# counterfactual_regime_m2.py
# POST-HOC counterfactual regime analysis (single-AI investment experiment).
#
# Motivation: the realized ~2-week outcome windows (May-June 2026) were
# defensive, so realized Active M2 embeds that regime. Because portfolios are
# FIXED at interaction time and the outcome window is an unforecastable
# buy-and-hold draw (preference-vs-signal argument), we can legitimately
# re-evaluate the SAME portfolios under other market draws: performance is a
# deterministic function of (weights x return path), and the counterfactual
# path is exogenous to all decisions.
#
# Design (extends the notebook §19 machinery):
#   - Pinned pre-study history: BOOT_START (2017-11-01) -> STUDY_START
#     (2026-05-05); the common asset panel effectively starts 2021-11 (RIVN
#     IPO) after dropna. Strictly pre-study => no look-ahead.
#   - Enumerate ALL overlapping 15-row (calendar-day) windows, matching the
#     realized 14-day evaluation window. Deterministic - no Monte-Carlo noise.
#   - Active M2 per window per portfolio (verbatim §19 convention):
#         M2_ann = mean(active)/sd(active) * sd(SPY) * sqrt(252),
#     active_t = r_portfolio,t - r_SPY,t within the window.
#   - REGIME BUCKETS by the window's SPY cumulative return:
#         Bear / Neutral / Bull = bottom / middle / top tercile.
#     Sensitivity buckets: Bull_q4 (top quartile), Bull_gt2 (SPY > +2%).
#   - Per participant: conditional mean M2 (pre & post portfolio) per bucket
#     + unconditional mean over all windows.
#
# Outputs (to notebooks/R/):
#   counterfactual_regime_m2.csv       participant-level conditional outcomes
#   counterfactual_windows_arm_m2.csv  window-level arm means (regime-response
#                                      curve raw material)
#
# Run:  cd notebooks && python counterfactual_regime_m2.py   (needs yfinance)
# ==============================================================================
import json as _json
import re as _re
import os
import numpy as np
import pandas as pd
from datetime import date
from scipy import stats

_BASE = "/Users/shiyang/Desktop/26 Spring/Biased AI"
os.chdir(f"{_BASE}/notebooks")

STUDY_START = pd.Timestamp("2026-05-05")   # first session day; data end the day before
BOOT_START  = pd.Timestamp("2017-11-01")   # ETH inception bounds the common window
LEN_WIN     = 15                           # panel rows = the realized 14-day evaluation
                                           # window (eval_start .. eval_start+14 inclusive).
                                           # The panel is CALENDAR-daily (crypto 7-day,
                                           # equities forward-filled), so these are
                                           # calendar days, not trading days.

# ── replicate §19 loading: exec the notebook's filter chain ───────────────────
_nb = _json.load(open(f"{_BASE}/notebooks/wave1_exam.ipynb"))
_g = {"stats": stats, "date": date}
def _run(i):
    exec(compile("".join(_nb["cells"][i]["source"]), f"cell{i}", "exec"), _g)
for _i in (0, 3, 4, 5, 7, 9, 10, 11, 12, 17):   # filter chain -> valid_obs (+ eligibility)
    _run(_i)
_run(19)                                         # ASSETS, N, pre_cols, post_cols
_src20 = "".join(_nb["cells"][20]["source"])     # download helpers only
_defs = "\n".join(_re.findall(r"(?ms)^def .*?(?=^\S|\Z)", _src20))
exec(compile(_defs, "cell20_defs", "exec"), _g)

_vo, ASSETS = _g["valid_obs"], _g["ASSETS"]
pre_cols, post_cols = _g["pre_cols"], _g["post_cols"]
for c in pre_cols + post_cols:
    _vo[c] = pd.to_numeric(_vo[c], errors="coerce")
Wpre  = _vo[pre_cols].to_numpy(float)
Wpost = _vo[post_cols].to_numpy(float)

_SHORT = {"This AI is a default Gemini model.": "Default",
          "This AI is a Gemini model with risk-neutral risk orientation.": "Risk-Neutral",
          "This AI is a Gemini model with somewhat risk-averse risk orientation.": "Somewhat Risk-Averse",
          "This AI is a Gemini model with extremely risk-averse risk orientation.": "Extremely Risk-Averse",
          "This AI is a Gemini model with somewhat risk-seeking risk orientation.": "Somewhat Risk-Seeking",
          "This AI is a Gemini model with extremely risk-seeking risk orientation.": "Extremely Risk-Seeking"}
ai_group = _vo["persona_ai"].map(_SHORT)

# ── pinned pre-study history ──────────────────────────────────────────────────
print(f"Downloading {BOOT_START.date()} -> {STUDY_START.date()} history ...")
prices  = _g["robust_download"](ASSETS, BOOT_START, STUDY_START)
prices  = prices[prices.index < STUDY_START]
returns = prices.pct_change().dropna(how="any")
print(f"history: {returns.index.min().date()} -> {returns.index.max().date()}, "
      f"T={len(returns)} trading days")

R     = returns[ASSETS].to_numpy(float)
spy_i = ASSETS.index("SPY")
T     = len(R)
starts = np.arange(0, T - LEN_WIN + 1)           # ALL overlapping windows
W_n    = len(starts)
print(f"{W_n} overlapping {LEN_WIN}-row (calendar-day) windows")

# ── per-window portfolio M2 (verbatim §19 convention), full P x W matrices ───
def norm_rows(W):
    rowsum = W.sum(axis=1, keepdims=True)
    return np.divide(W, rowsum, out=np.full_like(W, np.nan), where=(rowsum > 0))

def m2_matrix(W):
    """Active M2 for each portfolio (rows) x each window (cols)."""
    Wn   = norm_rows(W)
    Wn0  = np.where(np.isnan(Wn), 0.0, Wn)
    dead = np.isnan(Wn).all(axis=1)
    out  = np.full((W.shape[0], W_n), np.nan)
    for j, s in enumerate(starts):
        Rw  = R[s:s + LEN_WIN]                    # (LEN, A)
        rp  = Wn0 @ Rw.T                          # (P, LEN)
        rb  = Rw[:, spy_i]                        # (LEN,)
        act = rp - rb
        sa  = act.std(axis=1, ddof=1)
        sm  = rb.std(ddof=1)
        m2  = np.where((sa > 0) & (sm > 0),
                       act.mean(axis=1) / sa * sm * np.sqrt(252), np.nan)
        out[:, j] = m2
    out[dead, :] = np.nan
    return out

print("Scoring pre portfolios on all windows ...");  M2_pre  = m2_matrix(Wpre)
print("Scoring post portfolios on all windows ..."); M2_post = m2_matrix(Wpost)

# ── window-level SPY stats + regime buckets ───────────────────────────────────
spy_ret = np.array([np.prod(1.0 + R[s:s + LEN_WIN, spy_i]) - 1.0 for s in starts])
spy_vol = np.array([R[s:s + LEN_WIN, spy_i].std(ddof=1) for s in starts])
t1, t2  = np.quantile(spy_ret, [1/3, 2/3])
q4      = np.quantile(spy_ret, 0.75)
BUCKETS = {
    "bear":     spy_ret <= t1,
    "neutral":  (spy_ret > t1) & (spy_ret <= t2),
    "bull":     spy_ret > t2,
    "bull_q4":  spy_ret > q4,          # sensitivity: top quartile
    "bull_gt2": spy_ret > 0.02,        # sensitivity: SPY > +2% over the window
    "all":      np.ones(W_n, bool),
}
print("\nRegime buckets (SPY {}d window return):".format(LEN_WIN))
for k, mask in BUCKETS.items():
    print(f"  {k:9s}: {mask.sum():4d} windows | SPY ret mean {spy_ret[mask].mean():+.3f} "
          f"[{spy_ret[mask].min():+.3f}, {spy_ret[mask].max():+.3f}] | "
          f"daily vol mean {spy_vol[mask].mean():.4f}")

# ── participant-level conditional means -> CSV ────────────────────────────────
out = pd.DataFrame({"participantId": _vo["participantId"].values,
                    "wave": _vo["wave"].values,
                    "ai_group": ai_group.values})
for k, mask in BUCKETS.items():
    out[f"pre_m2_{k}"]  = np.nanmean(M2_pre[:,  mask], axis=1)
    out[f"post_m2_{k}"] = np.nanmean(M2_post[:, mask], axis=1)

out_path = f"{_BASE}/notebooks/R/counterfactual_regime_m2.csv"
out.to_csv(out_path, index=False)
print(f"\nWrote {out_path}: {out.shape}")
print("\nMean post M2 by arm x regime:")
print(out.groupby("ai_group")[["post_m2_bear", "post_m2_neutral", "post_m2_bull",
                               "post_m2_all"]].mean().round(4))

# ── window-level arm means (regime-response curve raw material) ───────────────
arm_rows = []
arms = [a for a in ai_group.dropna().unique()]
for a in arms:
    sel = (ai_group == a).values
    arm_rows.append(pd.DataFrame({
        "window_start": returns.index[starts],
        "spy_ret": spy_ret, "spy_vol": spy_vol,
        "arm": a,
        "mean_post_m2": np.nanmean(M2_post[sel][:, :], axis=0),
        "mean_pre_m2":  np.nanmean(M2_pre[sel][:, :],  axis=0),
        "n": sel.sum()}))
curve = pd.concat(arm_rows, ignore_index=True)
curve_path = f"{_BASE}/notebooks/R/counterfactual_windows_arm_m2.csv"
curve.to_csv(curve_path, index=False)
print(f"Wrote {curve_path}: {curve.shape}")
