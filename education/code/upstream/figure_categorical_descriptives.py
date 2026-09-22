#!/usr/bin/env python3
"""
s2_categorical_figure.py — SI descriptive figure for the classroom experiment's
categorical pre-survey covariates, in the style of main_survey_test_single.ipynb
(cell 15): a grid of horizontal count bars, sage #9DBE9E, Avenir 8 pt, inward
ticks, all four spines. The one structural change from the reference is a shared
x axis instead of per-panel ceilings -- see shared_axis() for why.

Unit is the STUDENT (n = 39), matching Panel A of the descriptive table — these
covariates are time-invariant, so counting them over the 312 student-weeks would
repeat every student ~8 times.

Panels cover the ten pre-survey items answered on named option sets (the two
nominal covariates plus the eight ordered-option items). The 1-7 agreement
battery and the four multi-select counts stay in the table, where their means
and SDs are meaningful.

Numeric codes are mapped back to the verbatim answer options via LABELS below;
`prior_ml` is missing for 2 students and that is shown as its own level so every
panel sums to 39.

Deviations from the reference cell, both for legibility at n = 39 (remove if you
want a literal match): counts are printed at the bar ends, since bars of length
1 vs 2 are otherwise indistinguishable; and the bottom row carries an x-label.

Input  : data/supplementary_dnr_student_week.csv
Output : figures/categorical_descriptives.png
Run    : cd education/code; python3 upstream/figure_categorical_descriptives.py
"""
import os


import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))          # education/
IN, OUT = os.path.join(ROOT, "data"), os.path.join(ROOT, "figures")
os.makedirs(OUT, exist_ok=True)

# ── Global figure style (verbatim from the reference notebook) ────────────────
mpl.rcParams.update({
    "font.family":       "Avenir",
    "font.size":         8,
    "axes.titlesize":    8,
    "axes.labelsize":    8,
    "xtick.direction":   "in",
    "ytick.direction":   "in",
    "axes.spines.top":   True,
    "axes.spines.right": True,
    "figure.dpi":        300,
})
BAR = "#9DBE9E"
MISSING = "(not answered)"

# ── Answer-option maps (from the balance-check codebook) ─────────────────────
EXPERIENCE = {1: "Beginner", 2: "Intermediate", 3: "Advanced"}
FREQ30 = {0: "Never", 1: "1-2 times", 2: "Weekly", 3: "Several/week", 4: "Daily"}
VERIFY = {0: "Never", 1: "Rarely", 2: "Sometimes", 3: "Often", 4: "Always"}
CLAUDE = {0: "Not applicable", 1: "Once or twice", 2: "Monthly", 3: "Weekly", 4: "Daily"}
HOURS = {0: "0 h", 0.5: "<1 h", 3: "1-5 h", 7.5: "5-10 h", 15: "10-20 h", 25: "20+ h"}

# (column, panel title, label map or None for already-string nominal levels)
# `None` marks the two unordered covariates: their bars sort by count, while the
# ordered items keep scale order so the panel reads down the scale.
PANELS = [
    ("academic_level",  "Academic level",              None),
    ("prior_prog",      "Prior programming exp.",      EXPERIENCE),
    ("prior_ml",        "Prior ML/AI exp.",            EXPERIENCE),
    ("prior_ide_tool",  "Prior IDE-integrated AI tool", None),
    ("use_learning",    "AI use: learning",            FREQ30),
    ("use_coding",      "AI use: coding",              FREQ30),
    ("use_writing",     "AI use: writing",             FREQ30),
    ("hours_ai_7d",     "AI hours, past 7 d",          HOURS),
    ("verify_freq",     "Verifies AI outputs",         VERIFY),
    ("claude_freq",     "Prior Claude Code use",       CLAUDE),
]
NCOL, NROW = 4, 3


def shared_axis(n_students, step=10):
    """Common x ceiling and round ticks.

    The reference notebook scales each panel to its own `nice_ceiling`, which suits
    panels with differing denominators. Here every panel counts the same 39
    students, so a per-panel ceiling would make a bar of 15 look longer than a bar
    of 28 purely from the axis; and nice_ceiling(37)=50 yields 0/12/25/37/50 ticks.
    One shared axis with round ticks fixes both.
    """
    xmax = int(np.ceil(n_students / step) * step)
    return xmax, np.arange(0, xmax + 1, step)


def ordered_counts(series, mapping):
    """Return (labels, counts) top-to-bottom; scale order if ordinal, else by count."""
    if mapping is None:                       # nominal: largest bar on top
        vc = series.dropna().astype(str).value_counts()
        return list(vc.index), list(vc.values)
    counts = [int((series == code).sum()) for code in mapping]   # scale order
    labels = list(mapping.values())
    n_missing = int(series.isna().sum())
    if n_missing:
        labels.append(MISSING)
        counts.append(n_missing)
    return labels, counts


def main():
    d = pd.read_csv(os.path.join(IN, "supplementary_dnr_student_week.csv"))
    stu = d.drop_duplicates("student_id")
    n_students = len(stu)

    xmax, xticks = shared_axis(n_students)

    fig, axes = plt.subplots(NROW, NCOL, figsize=(10, 5.6))
    axes = axes.flatten()

    for i, (col, title, mapping) in enumerate(PANELS):
        ax = axes[i]
        labels, counts = ordered_counts(stu[col], mapping)
        assert sum(counts) == n_students, f"{col}: counts sum to {sum(counts)}"

        # barh draws upward from y=0, so reverse to put labels[0] at the top
        y = np.arange(len(labels))
        ax.barh(y, counts[::-1], color=BAR)
        ax.set_yticks(y)
        ax.set_yticklabels(labels[::-1])
        ax.set_title(title, pad=5)

        ax.set_xlim(0, xmax * 1.15)           # pad leaves room for the end labels
        ax.set_xticks(xticks)
        for yy, val in zip(y, counts[::-1]):  # counts at bar ends
            ax.text(val + xmax * 0.025, yy, str(val), va="center", fontsize=6.5)

        ax.tick_params(axis="y", labelsize=7)
        ax.margins(y=0.05)
        if i >= len(PANELS) - NCOL:
            ax.set_xlabel("Number of students")

    for j in range(len(PANELS), len(axes)):   # drop the 2 spare cells
        fig.delaxes(axes[j])

    fig.tight_layout(h_pad=1, w_pad=1)
    path = os.path.join(OUT, "categorical_descriptives.png")
    plt.savefig(path, dpi=500, bbox_inches="tight")
    print(f"n = {n_students} students; {len(PANELS)} panels")
    print(f"Saved {path}")


if __name__ == "__main__":
    main()
