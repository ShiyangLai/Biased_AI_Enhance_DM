# ==============================================================================
# grs_regime_extremes.py
# GRS F under the FULL tradable history + systematically selected EXTREME-year
# frontiers (replaces the arbitrary 1y/3y/5y windows of §19 for the regime
# figure):
#   - full   : the entire common return panel. NOTE: RIVN's Nov-2021 IPO bounds
#              the 25-asset common sample to ~4.5y (2021-11 -> 2026-05-04);
#              a longer (e.g. 20y) frontier is impossible without dropping
#              assets participants hold, which would invalidate the statistic.
#   - bear1y : the 1-year window inside the panel with the LOWEST SPY
#              cumulative return (frontier mu-hat embeds the deepest bear)
#   - bull1y : the 1-year window with the HIGHEST SPY cumulative return
#              (the bull-regime counterfactual frontier)
# The panel is calendar-daily (crypto 7-day calendar, equities forward-filled,
# as in §19), so 1 year = 365 rows.
# All windows end before STUDY_START -> strictly ex-ante, reproducible.
# GRS computation is verbatim §19 (grs_vector, excess over SHY, pinv tangency).
#
# Output: notebooks/R/grs_regime_extremes.csv
#   participantId, wave, ai_group, pre/post_grs_F_{full,bear1y,bull1y}
# Run:  conda activate llm && python notebooks/grs_regime_extremes.py
# ==============================================================================
import json as _json
import re as _re
import numpy as np
import pandas as pd
from datetime import date
from scipy import stats

_BASE = "/Users/shiyang/Desktop/26 Spring/Biased AI"
STUDY_START = pd.Timestamp("2026-05-05")     # first session day; data end the day before
BOOT_START  = pd.Timestamp("2017-11-01")     # download start (panel self-bounds at RIVN)
WIN         = 365                            # extreme-window length (calendar-daily rows = 1y)

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

_vo, ASSETS, N = _g["valid_obs"], _g["ASSETS"], _g["N"]
pre_cols, post_cols = _g["pre_cols"], _g["post_cols"]
for c in pre_cols + post_cols:
    _vo[c] = pd.to_numeric(_vo[c], errors="coerce")
Wpre  = _vo[pre_cols].to_numpy(float)
Wpost = _vo[post_cols].to_numpy(float)

print(f"Downloading {BOOT_START.date()} -> {STUDY_START.date()} history ...")
prices  = _g["robust_download"](ASSETS, BOOT_START, STUDY_START)
prices  = prices[prices.index < STUDY_START]
returns = prices.pct_change().dropna(how="any")
print(f"common panel: {returns.index.min().date()} -> {returns.index.max().date()}, "
      f"T={len(returns)} (bounded by the latest asset inception)")

def grs_vector(W, mu, Sigma, S2_max, T, N):     # verbatim §19 logic
    W = np.asarray(W, float)
    rowsum = W.sum(axis=1, keepdims=True)
    Wn = np.divide(W, rowsum, out=np.full_like(W, np.nan), where=(rowsum > 0))
    mu_p  = Wn @ mu
    var_p = np.einsum("ij,jk,ik->i", Wn, Sigma, Wn)
    S2_p  = np.where(var_p > 0, mu_p**2 / var_p, np.nan)
    return ((T - N) / (N - 1)) * (S2_max - S2_p) / (1 + S2_p)

# ── select the extreme 252-day windows by SPY cumulative return ───────────────
logc = np.log1p(returns["SPY"].to_numpy()).cumsum()
ends = np.arange(WIN - 1, len(returns))                      # window = [e-WIN+1, e]
cum  = np.exp(logc[ends] - np.concatenate(([0.0], logc))[ends - WIN + 1]) - 1
bull_e = ends[int(np.argmax(cum))]
bear_e = ends[int(np.argmin(cum))]
windows = {
    "full":   returns,
    "def1y":  returns.iloc[-WIN:],                        # the pre-study year (defensive)
    "bear1y": returns.iloc[bear_e - WIN + 1: bear_e + 1],
    "bull1y": returns.iloc[bull_e - WIN + 1: bull_e + 1],
}
for k in ("def1y", "bear1y", "bull1y"):
    w = windows[k]
    spy_cum = float((1 + w["SPY"]).prod() - 1)
    print(f"{k}: {w.index.min().date()} -> {w.index.max().date()}  "
          f"(SPY {spy_cum*100:+.1f}% over {len(w)} days)")

out = pd.DataFrame({"participantId": _vo["participantId"], "wave": _vo["wave"]})
_SHORT = {"This AI is a default Gemini model.": "Default",
          "This AI is a Gemini model with risk-neutral risk orientation.": "Risk-Neutral",
          "This AI is a Gemini model with somewhat risk-averse risk orientation.": "Somewhat Risk-Averse",
          "This AI is a Gemini model with extremely risk-averse risk orientation.": "Extremely Risk-Averse",
          "This AI is a Gemini model with somewhat risk-seeking risk orientation.": "Somewhat Risk-Seeking",
          "This AI is a Gemini model with extremely risk-seeking risk orientation.": "Extremely Risk-Seeking"}
out["ai_group"] = _vo["persona_ai"].map(_SHORT)

def sharpe_vector(W, mu, Sigma):
    """SIGNED portfolio Sharpe under (mu, Sigma) — diagnostic for the GRS
    sign-symmetry artifact (S2_p is sign-blind: consistent losers get large
    squared Sharpe and hence deceptively small F)."""
    W = np.asarray(W, float)
    rowsum = W.sum(axis=1, keepdims=True)
    Wn = np.divide(W, rowsum, out=np.full_like(W, np.nan), where=(rowsum > 0))
    mu_p  = Wn @ mu
    var_p = np.einsum("ij,jk,ik->i", Wn, Sigma, Wn)
    return np.where(var_p > 0, mu_p / np.sqrt(var_p), np.nan)

for key, win in windows.items():
    ex  = win.sub(win["SHY"], axis=0)
    mu  = ex.mean().to_numpy()
    Sig = np.cov(ex.to_numpy(), rowvar=False, bias=True)
    S2m = float(mu @ np.linalg.pinv(Sig) @ mu)
    T   = len(win)
    out[f"pre_grs_F_{key}"]   = grs_vector(Wpre,  mu, Sig, S2m, T, N)
    out[f"post_grs_F_{key}"]  = grs_vector(Wpost, mu, Sig, S2m, T, N)
    out[f"pre_sharpe_{key}"]  = sharpe_vector(Wpre,  mu, Sig)   # signed diagnostics
    out[f"post_sharpe_{key}"] = sharpe_vector(Wpost, mu, Sig)
    print(f"{key}: T={T}, tangency Sharpe(ann)={np.sqrt(252*S2m):.2f}")

out.to_csv(f"{_BASE}/notebooks/R/grs_regime_extremes.csv", index=False)
print(f"\nWrote R/grs_regime_extremes.csv: {out.shape}, "
      f"valid post_grs_F_full: {out.post_grs_F_full.notna().sum()}")
print(out.groupby("ai_group")[["post_grs_F_full", "post_grs_F_bear1y", "post_grs_F_bull1y"]]
      .mean().round(3))
