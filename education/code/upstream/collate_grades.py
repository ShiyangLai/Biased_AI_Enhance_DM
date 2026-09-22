#!/usr/bin/env python3
"""
NOT run by run_all.R. This step needs the withheld stand-alone notebooks and the
un-blinding key, so it cannot execute from the repository; it is included so the
derivation of data/ai_standalone_grades.csv is inspectable end to end.


collate_grades.py — collate the two graders' scores for the stand-alone AI runs,
join the un-blinding key, and report the scale mismatch.

THE GRADERS DID NOT OVERLAP. Grader A scored weeks 1, 2, 7, 8, 9; Grader B scored
weeks 3, 4, 5, 6. No submission was scored by both, so the difference between their
scales is NOT identified by the data: any rescaling is an assumption, and "Grader B
is more generous" cannot be separated from "the assistants did better in weeks 3-6".

This matters less than it looks. The blinded ids were randomised independently
within each week, and each week was scored by exactly one grader, so any
WITHIN-WEEK comparison of conditions absorbs the grader effect entirely. The
arm analysis needs no rescaling at all -- only pooled descriptive means do.

Three alignment variants are written for that descriptive use, each labelled with
the assumption it makes:
  raw     as given
  z       standardised within grader (mean 0, SD 1 over that grader's scores)
  pct10   linearly mapped to 0-10 by each grader's own observed maximum

Outputs: ai_standalone_grades.csv
Run: python3 collate_grades.py
"""
import csv
import os
import statistics as st

HERE = os.path.dirname(os.path.abspath(__file__))
KEY = os.path.join(HERE, "UNBLINDING_KEY.csv")
OUT = os.path.join(HERE, "ai_standalone_grades_humans.csv")   # intermediate; merged by merge_standalone_raters.py

# Grader A — weeks 1, 2, 7, 8, 9
A = {
    (1, 1): 2.0,
    (2, 1): 4.0, (2, 2): 4.5, (2, 3): 4.5, (2, 4): 0.0, (2, 5): 4.5,
    (7, 1): 3.0, (7, 2): 0.0, (7, 3): 4.5, (7, 4): 5.5, (7, 5): 5.0,
    (8, 1): 3.0, (8, 2): 3.0, (8, 3): 5.0, (8, 4): 0.0, (8, 5): 4.0,
    (9, 1): 3.5, (9, 2): 3.0, (9, 3): 0.0, (9, 4): 3.0, (9, 5): 2.5,
}
# Grader B — weeks 3, 4, 5, 6; totals are completeness + quality as supplied
B = {
    (3, 1): 8.0, (3, 2): 0.0, (3, 3): 1.0, (3, 4): 0.0, (3, 5): 3.5,
    (4, 1): 9.0, (4, 2): 0.0, (4, 3): 9.0, (4, 4): 5.0, (4, 5): 8.5,
    (5, 1): 6.0, (5, 2): 10.0, (5, 3): 0.0, (5, 4): 9.0, (5, 5): 10.0,
    (6, 1): 0.0, (6, 2): 6.5, (6, 3): 10.0, (6, 4): 9.5, (6, 5): 8.0,
}
# Grader B reported a completeness/quality split; kept for the components table
B_PARTS = {
    (3, 1): (6.0, 2.0), (3, 3): (1.0, 0.0), (3, 5): (2.0, 1.5),
    (4, 1): (7.0, 2.0), (4, 3): (5.5, 2.0), (4, 4): (3.5, 1.5), (4, 5): (6.0, 2.5),
    (5, 1): (4.5, 1.5), (5, 2): (7.0, 3.0), (5, 4): (7.0, 2.0), (5, 5): (7.0, 3.0),
    (6, 2): (5.0, 1.5), (6, 4): (7.0, 2.5), (6, 5): (6.0, 2.0),
}
GRADES = {**{k: ("A", v) for k, v in A.items()}, **{k: ("B", v) for k, v in B.items()}}


def main():
    key = {}
    with open(KEY, encoding="utf-8") as fh:
        for r in csv.DictReader(fh):
            key[(int(r["week"]), int(r["blinded_id"]))] = r["condition"]

    missing = set(key) ^ set(GRADES)
    if missing:
        raise SystemExit(f"key/grade mismatch on {sorted(missing)}")

    by_grader = {}
    for k, (g, v) in GRADES.items():
        by_grader.setdefault(g, []).append(v)
    stats = {g: (st.mean(v), st.pstdev(v), max(v), min(v), len(v))
             for g, v in by_grader.items()}

    rows = []
    for (w, b), (g, v) in sorted(GRADES.items()):
        m, sd, mx, _, _ = stats[g]
        comp, qual = B_PARTS.get((w, b), ("", "")) if g == "B" else ("", "")
        rows.append(dict(
            week=w, blinded_id=b, condition=key[(w, b)], grader=g, raw=v,
            z=round((v - m) / sd, 3), pct10=round(10.0 * v / mx, 3),
            completeness=comp, quality=qual,
            zero=int(v == 0.0)))

    with open(OUT, "w", newline="", encoding="utf-8") as fh:
        wr = csv.DictWriter(fh, fieldnames=list(rows[0]))
        wr.writeheader(); wr.writerows(rows)

    print("=== scale mismatch (graders share no submissions) ===")
    print(f"{'grader':7}{'weeks':22}{'n':>3}{'mean':>7}{'SD':>7}{'min':>6}{'max':>6}")
    for g, wks in (("A", "1, 2, 7, 8, 9"), ("B", "3, 4, 5, 6")):
        m, sd, mx, mn, n = stats[g]
        print(f"{g:7}{wks:22}{n:>3}{m:>7.2f}{sd:>7.2f}{mn:>6.1f}{mx:>6.1f}")

    print("\n=== zero scores (assistant produced nothing gradable) ===")
    for g in ("A", "B"):
        z = [f"w{w}s{b}" for (w, b), (gg, v) in sorted(GRADES.items()) if gg == g and v == 0]
        print(f"  Grader {g}: {len(z)}/{stats[g][4]}  {' '.join(z)}")

    print("\n=== raw mean by condition, split by grader (NOT comparable across graders) ===")
    print(f"{'condition':14}{'Grader A':>12}{'Grader B':>12}{'n A':>5}{'n B':>5}")
    conds = sorted({r["condition"] for r in rows})
    for c in conds:
        a = [r["raw"] for r in rows if r["condition"] == c and r["grader"] == "A"]
        b = [r["raw"] for r in rows if r["condition"] == c and r["grader"] == "B"]
        pa = f"{st.mean(a):.2f}" if a else "-"
        pb = f"{st.mean(b):.2f}" if b else "-"
        print(f"{c:14}{pa:>12}{pb:>12}{len(a):>5}{len(b):>5}")

    print("\n=== within-week condition ranks (grader-free; 1 = best that week) ===")
    print(f"{'condition':14}{'mean rank':>11}{'n weeks':>9}")
    ranks = {}
    for w in sorted({r["week"] for r in rows}):
        wk = [r for r in rows if r["week"] == w]
        if len(wk) < 2:
            continue
        order = sorted(wk, key=lambda r: -r["raw"])
        i = 0
        while i < len(order):
            j = i
            while j + 1 < len(order) and order[j + 1]["raw"] == order[i]["raw"]:
                j += 1
            avg = (i + j) / 2 + 1
            for r in order[i:j + 1]:
                ranks.setdefault(r["condition"], []).append(avg)
            i = j + 1
    for c in sorted(ranks, key=lambda c: st.mean(ranks[c])):
        print(f"{c:14}{st.mean(ranks[c]):>11.2f}{len(ranks[c]):>9}")
    print(f"\nSaved {OUT}")


if __name__ == "__main__":
    main()
