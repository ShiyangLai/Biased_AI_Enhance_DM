"""
Risk-Standardized Active M² Bonus Assignment
=============================================
For each valid participant's post-AI portfolio (single-AI and dual-AI groups),
compute the active M² over their personal 14-calendar-day evaluation window
(EndDate + 14 days), then rank all participants together and report bonus tiers.

Active M² = IR × σ_m  (annualized)
  IR  = mean(active daily return) / std(active daily return)
  σ_m = std of SPY daily returns over the same window
  Active return = portfolio return − SPY benchmark return

A higher active M² means the participant's portfolio generated more
risk-adjusted alpha vs. the market per unit of tracking error.
"""

import warnings
warnings.filterwarnings("ignore")

import numpy as np
import pandas as pd
import yfinance as yf
import requests
from io import StringIO
from scipy import stats


def load_valid_obs(cr_path, ql_path, label):
    cr_data = pd.read_csv(cr_path)
    ql_data = pd.read_csv(ql_path)

    if not pd.to_numeric(ql_data["Duration (in seconds)"].iloc[1],
                         errors="coerce") == ql_data["Duration (in seconds)"].iloc[1]:
        ql_data = ql_data.iloc[1:].reset_index(drop=True)

    ql_data["Duration (in seconds)"] = pd.to_numeric(
        ql_data["Duration (in seconds)"], errors="coerce"
    )

    ids = cr_data[cr_data["Status"] == "Pending"].ParticipantId.values
    obs = ql_data[
        (ql_data["participantId"].isin(ids)) &
        (ql_data["Duration (in seconds)"] > 500)
    ].copy().reset_index(drop=True)
    obs["group"] = label
    print(f"  {label}: {len(obs)} valid participants")
    return obs


# ---------------------------------------------------------------
# 0. Load both groups
# ---------------------------------------------------------------
print("Loading participants …")
single_obs = load_valid_obs(
    cr_path="/Users/shiyang/Desktop/26 Spring/Biased AI/pilot-investment/single ai/"
            "assignments_019df9d2-ea77-77d3-a808-231b58b70c58.csv",
    ql_path="/Users/shiyang/Desktop/26 Spring/Biased AI/pilot-investment/single ai/"
            "Single AI investment - cloudresearch_May 6, 2026_12.00.csv",
    label="single_ai",
)
dual_obs = load_valid_obs(
    cr_path="/Users/shiyang/Desktop/26 Spring/Biased AI/pilot-investment/dual ai/"
            "assignments_019df9e0-491b-7f9a-b76b-5dedffde4723.csv",
    ql_path="/Users/shiyang/Desktop/26 Spring/Biased AI/pilot-investment/dual ai/"
            "Dual AI Investment - cloudresearch_May 6, 2026_12.07.csv",
    label="dual_ai",
)

valid_obs = pd.concat([single_obs, dual_obs], ignore_index=True)
print(f"Total valid participants: {len(valid_obs)}")

# ---------------------------------------------------------------
# 1. Asset universe — matches portfolio_{1..25} column order
# ---------------------------------------------------------------
ASSETS = [
    "SHY", "IEF", "LQD", "GLD", "VNQ",                  # 1-5  defensive
    "SPY", "XLF", "XLE", "XLI", "XLP", "XLU",           # 6-11 sectors
    "AAPL", "MSFT", "AMZN", "GOOGL", "NVDA", "TSLA",    # 12-17 mega-cap
    "SHOP", "SNOW", "PLTR", "DKNG", "RIVN", "CRSP",     # 18-23 high-beta
    "BTC-USD", "ETH-USD",                                # 24-25 crypto
]
N          = len(ASSETS)
BENCHMARK  = "SPY"
STOOQ_KEY  = "tYogT9BP2pFGWXmVZeSl3qzufjER1r75"

pre_cols  = [f"pre_portfolio_{i}"  for i in range(1, N + 1)]
post_cols = [f"post_portfolio_{i}" for i in range(1, N + 1)]

# Coerce portfolio weights to numeric for all rows
for c in pre_cols + post_cols:
    valid_obs[c] = pd.to_numeric(valid_obs[c], errors="coerce")

# ---------------------------------------------------------------
# 2. Parse EndDate → 14-calendar-day evaluation window
#    Format example: '5/6/26 0:58'
# ---------------------------------------------------------------
valid_obs["EndDate_parsed"] = pd.to_datetime(
    valid_obs["EndDate"], format="%m/%d/%y %H:%M"
)
valid_obs["eval_start"] = valid_obs["EndDate_parsed"].dt.normalize()
valid_obs["eval_end"]   = valid_obs["eval_start"] + pd.Timedelta(days=14)

print(f"Survey EndDate range : {valid_obs['EndDate_parsed'].min()} → "
      f"{valid_obs['EndDate_parsed'].max()}")
print(f"Evaluation window    : {valid_obs['eval_start'].min().date()} → "
      f"{valid_obs['eval_end'].max().date()}")

# ---------------------------------------------------------------
# 3. Pull price data spanning all evaluation windows
# ---------------------------------------------------------------
fetch_start = valid_obs["eval_start"].min() - pd.Timedelta(days=3)  # small buffer
fetch_end   = valid_obs["eval_end"].max()   + pd.Timedelta(days=1)

print(f"\nDownloading prices {fetch_start.date()} → {fetch_end.date()} …")
prices = (
    yf.download(ASSETS, start=fetch_start, end=fetch_end,
                auto_adjust=True, progress=False)["Close"]
    .reindex(columns=ASSETS)
)


def fetch_stooq(ticker, start, end, key):
    url = (
        f"https://stooq.com/q/d/l/?s={ticker.lower()}.us&i=d"
        f"&d1={start.strftime('%Y%m%d')}&d2={end.strftime('%Y%m%d')}"
        f"&apikey={key}"
    )
    r = requests.get(url, timeout=15)
    r.raise_for_status()
    text = r.text.strip()
    if not text.startswith("Date,"):
        raise RuntimeError(f"Stooq returned: {text[:200]}")
    return (
        pd.read_csv(StringIO(text), parse_dates=["Date"])
        .set_index("Date")["Close"]
        .sort_index()
    )


try:
    pltr = fetch_stooq("PLTR", fetch_start, fetch_end, STOOQ_KEY)
    prices["PLTR"] = pltr.reindex(prices.index)
    print("PLTR patched from Stooq.")
except Exception as exc:
    print(f"Stooq PLTR patch failed ({exc}); using yfinance data.")

prices.dropna(how="all", inplace=True)
returns = prices.pct_change().dropna(how="any")

print(f"Returns: {returns.index.min().date()} → {returns.index.max().date()}, "
      f"{len(returns)} trading days, {len(returns.columns)} assets")

# ---------------------------------------------------------------
# 4. Active M² for each participant
# ---------------------------------------------------------------

def active_m2_annualized(w, r_window):
    """
    Compute annualized active M² for weight vector w over r_window.

    Parameters
    ----------
    w        : (N,) array of portfolio weights (will be normalized to sum=1)
    r_window : (T_w, N) DataFrame of daily returns for the evaluation window

    Returns
    -------
    Annualized active M² (float, or np.nan if infeasible)
    """
    w = np.asarray(w, dtype=float)
    valid_mask = ~np.isnan(w)
    if valid_mask.sum() == 0 or w[valid_mask].sum() <= 0:
        return np.nan
    w = np.where(valid_mask, w, 0.0)
    w = w / w.sum()

    r_arr   = r_window.values                       # (T_w, N)
    r_p     = r_arr @ w                             # portfolio daily returns
    r_b     = r_window[BENCHMARK].values            # benchmark (SPY)

    active  = r_p - r_b                             # active daily return
    sig_a   = np.std(active,  ddof=1)
    sig_m   = np.std(r_b,     ddof=1)

    if sig_a == 0 or sig_m == 0 or np.isnan(sig_a) or np.isnan(sig_m):
        return np.nan

    IR = np.mean(active) / sig_a                    # Information Ratio (daily)
    return IR * sig_m * np.sqrt(252)                # annualized active M²


records = []
for _, row in valid_obs.iterrows():
    pid        = row["participantId"]
    eval_start = row["eval_start"]
    eval_end   = row["eval_end"]

    mask  = (returns.index >= eval_start) & (returns.index <= eval_end)
    r_win = returns.loc[mask]

    if len(r_win) < 3:
        print(f"  WARNING {pid}: only {len(r_win)} trading days in window — skipping.")
        records.append(dict(participantId=pid, group=row["group"],
                            trading_days=len(r_win),
                            eval_start=eval_start.date(), eval_end=eval_end.date(),
                            pre_active_m2_ann=np.nan, post_active_m2_ann=np.nan))
        continue

    w_pre  = row[pre_cols].to_numpy(dtype=float)
    w_post = row[post_cols].to_numpy(dtype=float)

    records.append(dict(
        participantId      = pid,
        group              = row["group"],
        eval_start         = eval_start.date(),
        eval_end           = eval_end.date(),
        trading_days       = len(r_win),
        pre_active_m2_ann  = active_m2_annualized(w_pre,  r_win),
        post_active_m2_ann = active_m2_annualized(w_post, r_win),
    ))

results = pd.DataFrame(records)
results["active_m2_improvement"] = results["post_active_m2_ann"] - results["pre_active_m2_ann"]

print("\n--- Active M² Results (all participants) ---")
print(results[["participantId", "group", "trading_days",
               "pre_active_m2_ann", "post_active_m2_ann",
               "active_m2_improvement"]].to_string(index=False))

print("\n--- Summary Stats ---")
print(results[["pre_active_m2_ann", "post_active_m2_ann",
               "active_m2_improvement"]].describe())

# ---------------------------------------------------------------
# 5. Bonus tiers — all 20 participants ranked together
#    Ranking by post_active_m2_ann (higher = better)
# ---------------------------------------------------------------
ranked = results.dropna(subset=["post_active_m2_ann"]).copy()
ranked = ranked.sort_values("post_active_m2_ann", ascending=False).reset_index(drop=True)
ranked["rank"]     = ranked.index + 1
ranked["rank_pct"] = ranked["post_active_m2_ann"].rank(
    ascending=False, pct=True, method="average"
)

top_5    = ranked[ranked["rank_pct"] <= 0.05]["participantId"].tolist()
top5_10  = ranked[(ranked["rank_pct"] > 0.05) & (ranked["rank_pct"] <= 0.10)]["participantId"].tolist()
top10_20 = ranked[(ranked["rank_pct"] > 0.10) & (ranked["rank_pct"] <= 0.20)]["participantId"].tolist()

print("\n" + "=" * 65)
print("FULL RANKING  (post-AI annualized active M², all 20 participants)")
print("=" * 65)
print(ranked[["rank", "participantId", "group",
              "post_active_m2_ann", "rank_pct"]].to_string(index=False))

print("\n" + "=" * 65)
print("BONUS TIERS  (ranked by annualized active M², post-AI, N=20)")
print("=" * 65)
print(f"Top 5%      ({len(top_5):>3} participant ): {top_5}")
print(f"Top 5-10%   ({len(top5_10):>3} participant ): {top5_10}")
print(f"Top 10-20%  ({len(top10_20):>3} participants): {top10_20}")

# ---------------------------------------------------------------
# 6. Save full results
# ---------------------------------------------------------------
out_path = (
    "/Users/shiyang/Desktop/26 Spring/Biased AI/pilot-investment/"
    "active_m2_bonus_results_combined.csv"
)
ranked.to_csv(out_path, index=False)
print(f"\nFull ranked results saved → {out_path}")
