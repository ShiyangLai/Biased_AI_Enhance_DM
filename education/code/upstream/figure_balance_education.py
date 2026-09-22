#!/usr/bin/env python3
"""
Covariate-balance Love plot for the CLASSROOM experiment (novelty vs reliability).

Plotting style is ported from notebooks/figure_balance_investment.py so the three
experiments' balance figures are visually identical: sage squares for categorical
covariates, coral triangles for continuous ones, a dashed 0.10 reference line, and
a frameless lower-right legend.

ONE ADDITION to that style. At n = 19 vs 20 the sampling SD of an ASMD under pure
randomization is ~0.32, so the 0.10 line sits at 0.31 SD and roughly three
quarters of covariates clear it by chance. A grey tick on each row marks that
covariate's own 95th percentile under re-randomization, so the reader can see
that every observed point falls well inside its null. Without it the figure shows
18 of 26 markers past the dashed line and reads as a randomization failure, when
re-randomization puts the median count at 20 of 26.

Input  : data/balance_education.csv  (written by balance_check_education.R)
Output : figures/balance_education.png
Run    : cd education/code; python3 upstream/figure_balance_education.py
"""
from pathlib import Path

import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[2]      # education/
DATA = ROOT / "data"
OUT = ROOT / "figures"

mpl.rcParams.update({
    "font.family":       "Avenir",
    "font.size":         8,
    "axes.spines.top":   True,
    "axes.spines.right": True,
    "xtick.direction":   "in",
    "ytick.direction":   "in",
    "figure.dpi":        300,
})

# colours & markers (verbatim from the investment figure)
style = {
    "Categorical": dict(color="#A1C181", marker="s", label="Categorical"),  # sage
    "Continuous":  dict(color="#D98C7A", marker="^", label="Continuous"),   # coral
}
NULLC = "#9A9A9A"
threshold = 0.10                       # reference line


def love_plot(csv_name, out_name):
    balance = (pd.read_csv(DATA / csv_name)
                 .rename(columns={"effect": "Metric"})
                 .assign(Type=lambda d: d["type"].str.capitalize())
                 .sort_values("Metric", ascending=False)
                 .reset_index(drop=True))

    fig, ax = plt.subplots(figsize=(5.2, 6.6))
    y = np.arange(len(balance))        # 0 at top after invert_yaxis()

    for i, row in balance.iterrows():
        s = style[row["Type"]]
        # 95th percentile of this covariate's own re-randomization null
        ax.plot([row["null_p95"]] * 2, [i - 0.34, i + 0.34],
                lw=1.1, color=NULLC, zorder=1)
        # horizontal segment + endpoint marker
        ax.plot([0, row["Metric"]], [i, i], lw=1.3, color=s["color"], zorder=2)
        ax.scatter(row["Metric"], i, s=45, marker=s["marker"],
                   color=s["color"], edgecolor="k", zorder=3)

    ax.axvline(threshold, ls="--", lw=1, color="gray")

    ax.set_yticks(y)
    ax.set_yticklabels(balance["Variable"])
    ax.set_xlabel("Balance metric  (Cramér's V  |  ASMD)", fontsize=11)
    ax.set_ylim(len(balance) - 0.5, -0.5)   # top variable at top
    ax.margins(x=0.05)
    ax.set_xlim(left=0)

    handles = [plt.Line2D([0], [0], marker=style[t]["marker"], color="w",
                          markerfacecolor=style[t]["color"], markeredgecolor="k",
                          markersize=6, lw=0, label=style[t]["label"])
               for t in ["Categorical", "Continuous"]]
    handles.append(plt.Line2D([0], [0], color=NULLC, lw=1.1,
                              label="95th pct., re-randomized"))
    # The reference puts this lower-right, but here the null ticks occupy that
    # corner (~0.6-0.69) while observed values stop at ~0.51, leaving a clean
    # corridor in between; anchor the legend there instead of over the ticks.
    ax.legend(handles=handles, frameon=False, loc="lower left",
              bbox_to_anchor=(0.30, 0.005))

    fig.tight_layout()
    fig.savefig(OUT / out_name, dpi=500, bbox_inches="tight")
    plt.close(fig)
    n_over = int((balance["Metric"] >= threshold).sum())
    n_out = int((balance["Metric"] > balance["null_p95"]).sum())
    print(f"saved {out_name}  ({len(balance)} covariates, max metric "
          f"{balance['Metric'].max():.3f})")
    print(f"  past the 0.10 line: {n_over}; past their own 95th null pct.: {n_out}")


love_plot("balance_education.csv", "balance_education.png")
