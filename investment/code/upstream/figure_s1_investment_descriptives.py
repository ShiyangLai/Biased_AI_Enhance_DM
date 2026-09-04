"""
Supplementary descriptive figures for the INVESTMENT experiment: distributions of
categorical / discrete background, attitude, and post-interaction variables.

Investment analog of the fact-checking figure_s1_single_descriptives.py, with the
panel list adapted to this design. Every seven-point scale item is treated as a
discrete variable and shown as a bar panel rather than summarised in the
continuous table.

Self-contained: loads the wave Qualtrics exports and the CloudResearch assignment
files directly, then restricts to the main-text analytic samples.

    conda activate llm
    python notebooks/figure_s1_investment_descriptives.py

Writes notebooks/R/cat_descrp_invest_single.{png,pdf}
   and notebooks/R/cat_descrp_invest_dual.{png,pdf}
"""

import glob
from math import log10
from pathlib import Path

import matplotlib as mpl
import matplotlib.font_manager as fm
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parent.parent
RDIR = ROOT / "notebooks" / "R"
FONT = Path("/Users/shiyang/Library/CloudStorage/GoogleDrive-shiyanglai@uchicago.edu"
            "/My Drive/Diversified AI Bias/Font/AvenirLTStd-Roman.otf")

SINGLE_COLOR = "#B68A6E"   # matches fact-checking Fig. S1
DUAL_COLOR = "#9BBF95"     # matches fact-checking Fig. S2

# ---------------------------------------------------------------
# 0.  Figure style (verbatim from the fact-checking script)
# ---------------------------------------------------------------
if FONT.exists():
    fm.fontManager.addfont(str(FONT))
    font_family = fm.FontProperties(fname=str(FONT)).get_name()
else:
    font_family = "DejaVu Sans"

mpl.rcParams.update({
    "font.family":       font_family,
    "font.size":         8,
    "axes.titlesize":    8,
    "axes.labelsize":    8,
    "xtick.direction":   "in",
    "ytick.direction":   "in",
    "axes.spines.top":   True,
    "axes.spines.right": True,
    "figure.dpi":        300,
    "pdf.fonttype":      42,
    "ps.fonttype":       42,
})

# ---------------------------------------------------------------
# 1.  Data
# ---------------------------------------------------------------
SINGLE_FILES = [
    "invest-single-ai-wave1/Single AI investment - cloudresearch_May 21, 2026_17.35.csv",
    "invest-single-ai-wave2/Single AI investment - cloudresearch_May 25, 2026_17.48.csv",
    "invest-single-ai-wave3/Single AI investment - cloudresearch_June 24, 2026_21.17.csv",
]
DUAL_FILES = [
    "invest-dual-ai-wave1/Dual AI Investment - cloudresearch_May 21, 2026_17.38.csv",
    "invest-dual-ai-wave2/Dual AI Investment - cloudresearch_May 25, 2026_17.50.csv",
    "invest-dual-ai-wave3/Dual AI Investment - cloudresearch_June 24, 2026_21.25.csv",
]
ASSIGN_DIRS = ["invest-single-ai-wave1", "invest-single-ai-wave2", "invest-single-ai-wave3",
               "invest-dual-ai-wave1", "invest-dual-ai-wave2", "invest-dual-ai-wave3"]

cr = pd.concat(
    [pd.read_csv(f, dtype=str) for d in ASSIGN_DIRS
     for f in glob.glob(str(ROOT / d / "assignments_*.csv"))],
    ignore_index=True).drop_duplicates("ParticipantId")
cr["pid"] = cr["ParticipantId"].str.upper()
cr = cr[["pid", "Education", "Household Income", "Employment Status"]]

EDU = {
    "Bachelor's degree (for example: BA, AB, BS)": "Bachelor's",
    "Master's degree (for example: MA, MS, MEng, MEd, MSW, MBA)": "Master's",
    "Associate degree (for example: AA, AS)": "Associate",
    "Professional degree (for example: MD, DDS, DVM, LLB, JD)": "Professional",
    "Doctorate degree (for example: PhD, EdD)": "Doctorate",
    "Some college, but no degree": "Some college",
    "High school graduate - high school diploma or the equivalent (for example: GED)": "High school",
    "Less than a high school diploma": "< High school",
}


def coarse_income(x):
    if not isinstance(x, str) or x == "" or x == "Prefer not to say":
        return "Prefer not to say"
    try:
        low = int(x.split("-")[0].replace("$", "").replace(",", "").split(" ")[0])
    except ValueError:
        return "≥$200k" if "more" in x else "Prefer not to say"
    if low < 50_000:
        return "<$50k"
    if low < 100_000:
        return "$50-99k"
    if low < 150_000:
        return "$100-149k"
    if low < 200_000:
        return "$150-199k"
    return "≥$200k"


NEWS = {"Occasionally (a few times a month / about once a week)": "Occasionally",
        "Frequently (more than once a week)": "Frequently",
        "Rarely (less than once a month / about once a month)": "Rarely",
        "Never": "Never"}
FIRST = {"10 years ago or more": "10+ years", "5 years to less than 10 years ago": "5-10 years",
         "3 years to less than 5 years ago": "3-5 years", "1 year to less than 3 years ago": "1-3 years",
         "Less than a year ago": "< 1 year"}
ROLE = {"Primarily as an agent trying to influence or persuade me in making determinations": "Agent",
        "Mostly as a tool to assist me in making my own determinations": "Tool",
        "A mix of both a tool and an influencing agent": "Mixed",
        "Neither as a tool nor as an influencing agent": "Neither",
        "Unsure": "Unsure"}


def short_lottery(x):
    if not isinstance(x, str) or not x.startswith("Option"):
        return np.nan
    n = x.split(":")[0].replace("Option ", "")
    amts = [p.split("$")[1].split(" ")[0].replace(",", "")
            for p in x.split("/") if "$" in p]
    return f"{n}: {int(amts[0]) // 1000}k/{int(amts[-1]) // 1000}k"


def load(files, analytic_csv, covariate_csv):
    df = pd.concat([pd.read_csv(ROOT / f, skiprows=[1, 2], low_memory=False) for f in files],
                   ignore_index=True).drop_duplicates("ResponseId")
    keep = set(pd.read_csv(RDIR / analytic_csv)["participantId"].astype(str).str.upper())
    df["pid"] = df["participantId"].astype(str).str.upper()
    df = df[df["pid"].isin(keep)].drop_duplicates("pid").merge(cr, on="pid", how="left")

    cov = pd.read_csv(RDIR / covariate_csv)
    cov["pid"] = cov["participantId"].astype(str).str.upper()
    df = df.merge(cov[["pid", "fin_lit_score"]], on="pid", how="left")

    df["edu"] = df["Education"].replace(EDU)
    df["income"] = df["Household Income"].map(coarse_income)
    df["employ"] = df["Employment Status"].replace(
        {"Not in paid work (e.g., homemaker, disabled)": "Not in paid work"})
    df["first_inv"] = df["first_invest_time"].replace(FIRST)
    df["news"] = df["news_follow_freq"].replace(NEWS)
    df["lottery"] = df["risk_lottery_choice"].map(short_lottery)
    df["role"] = df["ai_role"].map(ROLE)
    df["fin_lit_n"] = (pd.to_numeric(df["fin_lit_score"], errors="coerce") * 7).round()
    return df


# ---------------------------------------------------------------
# 2.  Panels
# ---------------------------------------------------------------
PANELS = [
    ("edu",                    "Education"),
    ("income",                 "Household income"),
    ("employ",                 "Employment status"),
    ("first_inv",              "Years since first investment"),
    ("trade_freq_12m",         "Trading freq. (12 mo.)"),
    ("news",                   "Financial news freq."),
    ("lottery",                "Risk lottery choice"),
    ("fin_lit_n",              "Financial literacy (correct)"),
    ("fin_overconfidence_1",   "Relative investing skill"),
    ("portfolio_confidence_1", "Confidence in own investing"),
    ("market_outlook_2wk",     "Two-week market outlook"),
    ("market_volatility_2w",   "Expected volatility"),
    ("pre_portfolio_conf_1",   "Pre-interaction confidence"),
    ("post_portfolio_conf_1",  "Post-interaction confidence"),
    ("ai_influence_1",         "Perceived AI influence"),
    ("post_performance_exp_1", "Expected performance"),
    ("post_portfolio_comp_1",  "Perceived improvement"),
    ("role",                   "Perceived role of AI"),
]

# Verbal anchors for the seven-point items. The Qualtrics exports carry no
# question-text row, so these summarise each scale's intended meaning; edit here
# if the instrument's own wording differs.
INTENSITY = ["Not at all", "Slightly", "Somewhat", "Moderately", "Quite", "Very",
             "Extremely"]
SCALE_LABELS = {
    "fin_overconfidence_1":   ["Far below avg.", "Below avg.", "Slightly below",
                               "Average", "Slightly above", "Above avg.", "Far above avg."],
    "portfolio_confidence_1": INTENSITY,
    "pre_portfolio_conf_1":   INTENSITY,
    "post_portfolio_conf_1":  INTENSITY,
    "ai_influence_1":         ["Not at all", "Slightly", "Somewhat", "Moderately",
                               "Quite a bit", "A great deal", "Completely"],
    "post_performance_exp_1": ["Very poorly", "Poorly", "Slightly poorly",
                               "About average", "Slightly well", "Well", "Very well"],
    "post_portfolio_comp_1":  ["Much worse", "Worse", "Slightly worse", "About the same",
                               "Slightly better", "Better", "Much better"],
}


def nice_ceiling(value):
    """Round value up to 1-2-5 times a power of 10."""
    if value == 0:
        return 1
    order = 10 ** int(log10(value))
    for mult in (1, 2, 5, 10):
        if value <= mult * order:
            return mult * order


# Numeric rating scales are ordered by scale value rather than by frequency, so
# the panel reads as a distribution; nominal variables keep the frequency sort
# used in the fact-checking figure.
NUMERIC_SCALE = {"fin_lit_n"} | set(SCALE_LABELS)


def make_figure(df, color, out_stem, label):
    fig, axes = plt.subplots(5, 4, figsize=(10.5, 9))
    axes = axes.flatten()
    for i, (var, name) in enumerate(PANELS):
        ax = axes[i]
        if var not in df.columns:
            fig.delaxes(ax)
            continue
        s = df[var].dropna()
        if s.dtype.kind == "f":
            s = s.astype(int)
        counts = s.astype(str).value_counts()
        if var in NUMERIC_SCALE:
            # every scale point present, ordered high to low down the axis
            pts = range(0, 8) if var == "fin_lit_n" else range(1, 8)
            counts = counts.reindex([str(k) for k in pts], fill_value=0)[::-1]
            if var in SCALE_LABELS:
                counts.index = [SCALE_LABELS[var][int(k) - 1] for k in counts.index]
        else:
            counts = counts.sort_values(ascending=True)
        ax.barh(counts.index, counts.values, color=color)
        ax.set_title(name, pad=5)
        xmax = nice_ceiling(counts.max())
        ax.set_xlim(0, xmax + 20)
        ax.set_xticks(np.linspace(0, xmax, 3, dtype=int))
        ax.tick_params(axis="y", labelsize=7)
        ax.tick_params(axis="x", labelsize=7)
        ax.margins(y=0.05)
    for j in range(len(PANELS), len(axes)):
        fig.delaxes(axes[j])
    fig.tight_layout(h_pad=1, w_pad=1)
    fig.savefig(RDIR / f"{out_stem}.png", dpi=300, bbox_inches="tight")
    fig.savefig(RDIR / f"{out_stem}.pdf", dpi=300, bbox_inches="tight")
    plt.close(fig)

    print(f"\n{label} participants: {len(df)}\n")
    for var, name in PANELS:
        if var not in df.columns:
            print(f"{name}: NOT PRESENT in this survey")
            continue
        s = df[var].dropna()
        if s.dtype.kind == "f":
            s = s.astype(int)
        s = s.astype(str)
        print(f"{name}  (n = {len(s)}, missing = {len(df) - len(s)})")
        for level, n in s.value_counts().items():
            print(f"    {level:<30} {n:>5}  ({n / len(s) * 100:5.1f}%)")
        print()


single = load(SINGLE_FILES, "active_m2_treatment_data.csv", "participant_covariates.csv")
dual = load(DUAL_FILES, "dual_active_m2.csv", "dual_covariates.csv")
make_figure(single, SINGLE_COLOR, "cat_descrp_invest_single", "Single-AI")
make_figure(dual, DUAL_COLOR, "cat_descrp_invest_dual", "Dual-AI")
print(f"Saved cat_descrp_invest_{{single,dual}}.{{png,pdf}} in {RDIR}")
