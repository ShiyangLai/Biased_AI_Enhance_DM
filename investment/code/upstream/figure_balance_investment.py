"""
Covariate-balance Love plots for the INVESTMENT experiment.

Plotting style is ported verbatim from the fact-checking notebook
(Code/main_survey_test_single.ipynb, cell 26) so the two experiments' balance
figures are visually identical: sage squares for categorical covariates,
coral triangles for continuous ones, a dashed 0.10 reference line, and a
frameless lower-right legend.

Inputs are the CSVs written by notebooks/R/balance_check_investment.R, whose
`Variable` column already carries display labels and whose `type` column is
"categorical" / "continuous".

    conda activate llm
    python notebooks/figure_balance_investment.py

Writes notebooks/R/balance_investment_{single,dual}.png
"""

from pathlib import Path

import matplotlib as mpl
import matplotlib.pyplot as plt
import pandas as pd

RDIR = Path(__file__).resolve().parent / "R"

mpl.rcParams.update({
    "font.family":     "Avenir",
    "font.size":       8,
    "axes.spines.top": True,
    "axes.spines.right": True,
    "xtick.direction": "in",
    "ytick.direction": "in",
    "figure.dpi":      300
})

# colours & markers
style = {
    "Categorical": dict(color="#A1C181", marker="s", label="Categorical"),  # sage
    "Continuous":  dict(color="#D98C7A", marker="^", label="Continuous")   # coral
}

threshold = 0.10                       # reference line


def love_plot(csv_name, out_name):
    balance = (pd.read_csv(RDIR / csv_name)
                 .rename(columns={"effect": "Metric"})
                 .assign(Type=lambda d: d["type"].str.capitalize())
                 .sort_values("Metric", ascending=False)
                 .reset_index(drop=True))

    fig, ax = plt.subplots(figsize=(5, 5))

    for _, row in balance.iterrows():
        y = row["Variable"]
        x = row["Metric"]
        s = style[row["Type"]]

        # horizontal segment
        ax.plot([0, x], [y, y], lw=1.3, color=s["color"])
        # endpoint marker
        ax.scatter(x, y, s=45, marker=s["marker"],
                   color=s["color"], edgecolor="k", zorder=3)

    # reference line
    ax.axvline(threshold, ls="--", lw=1, color="gray")

    # aesthetics
    ax.set_xlabel("Balance metric  (Cramér's V  |  ASMD)", fontsize=11)
    ax.invert_yaxis()                 # top variable at top
    ax.margins(x=0.05)                # breathing room on the left

    # custom legend (square & triangle)
    handles = [plt.Line2D([0], [0],
                          marker=style[t]["marker"],
                          color="w", markerfacecolor=style[t]["color"],
                          markeredgecolor="k", markersize=6, lw=0,
                          label=style[t]["label"])
               for t in ["Categorical", "Continuous"]]
    ax.legend(handles=handles, frameon=False, loc="lower right")

    fig.tight_layout()
    fig.savefig(RDIR / out_name, dpi=300, bbox_inches="tight")
    plt.close(fig)
    print(f"saved {out_name}  ({len(balance)} covariates, "
          f"max metric {balance['Metric'].max():.3f})")


love_plot("balance_investment_single.csv", "balance_investment_single.png")
love_plot("balance_investment_dual.csv", "balance_investment_dual.png")
