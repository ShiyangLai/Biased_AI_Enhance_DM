#!/usr/bin/env python3
"""
merge_standalone_raters.py — build data/ai_standalone_grades.csv, the three-rater
composite grade for the stand-alone assistant runs (Extended Data Fig. 6c and
the stand-alone-adjusted specification of the robustness tables).

Three passes graded the same 41 blinded notebooks (build_blinded_package.py):
  humanAB  two teaching assistants, on disjoint weeks: grader A took weeks
           1, 2, 7, 8, 9 (0-5.5 scale as used); grader B weeks 3, 4, 5, 6 (0-10).
           Collated by collate_grades.py into ai_standalone_grades_humans.csv.
  humanC   a third teaching assistant, all nine weeks, 0-10, delivered as
           blinded_grade/weekN/weekN_grades.txt with lines "Submission k: x/10".
  gemini   Gemini 3.1 Pro under the course rubric, all weeks, 0-10; taken from
           results/agent_grades_s45_summary.csv in the deployment repository
           (github.com/Gio-Choi/agent_sandbox). It skipped the seven empty
           questioning-arm notebooks, which every human scored 0.
Its nb_hash column matches the md5 of every graded notebook (34/34), so all
three passes scored identical files.

The questioning arm is excluded, leaving 33 notebooks. Because the raters used
different scales -- and humanAB is two people -- each series is standardised
before averaging, with grader A's and grader B's weeks standardised separately;
the composite is the mean of the three z-scores. Cronbach's alpha for the three
raters is 0.770, against 0.613 for the two humans alone, the same criterion that
put Gemini into the student coding composite. That reliability check is printed.

NOT run by run_all.R (its inputs are withheld); shipped so the derivation is inspectable.
Usage: python3 merge_standalone_raters.py <ai_standalone_grades_humans.csv> <blinded_grade/> <agent_grades_s45_summary.csv> [education/data]
"""
import csv, glob, os, re, statistics as st, sys
humans, folder, gem_csv = sys.argv[1:4]
data = sys.argv[4] if len(sys.argv) > 4 else os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..", "data")
base = {(int(r["week"]), int(r["blinded_id"])): r for r in csv.DictReader(open(humans))}
C = {}
for f in glob.glob(os.path.join(folder, "week*", "week*_grades.txt")):
    w = int(re.search(r"week(\d+)", os.path.basename(f)).group(1))
    for m in re.finditer(r"Submission\s+(\d+)\s*:\s*([0-9.]+)\s*/\s*10", open(f, encoding="utf-8").read()):
        C[(w, int(m.group(1)))] = float(m.group(2))
G = {(int(r["week"]), r["condition"]): float(r["final_score"]) for r in csv.DictReader(open(gem_csv))}
rows = []
for k, r in sorted(base.items()):
    if r["condition"] == "questioning": continue
    rows.append(dict(week=k[0], condition=r["condition"], ab_grader=r["grader"], humanAB=float(r["raw"]),
                     humanC=C[k], gemini=G.get((k[0], r["condition"]), 0.0)))
def z(vals):
    m, s = st.mean(vals), st.pstdev(vals); return [(v - m) / s if s else 0.0 for v in vals]
for g in ("A", "B"):
    idx = [i for i, r in enumerate(rows) if r["ab_grader"] == g]
    for i, v in zip(idx, z([rows[i]["humanAB"] for i in idx])): rows[i]["z_humanAB"] = v
for col in ("humanC", "gemini"):
    for i, v in enumerate(z([r[col] for r in rows])): rows[i]["z_" + col] = v
for r in rows: r["composite"] = st.mean([r["z_humanAB"], r["z_humanC"], r["z_gemini"]])
def alpha(cols):
    items = [[r[c] for r in rows] for c in cols]; tot = [sum(v) for v in zip(*items)]
    return len(cols) / (len(cols) - 1) * (1 - sum(st.pvariance(i) for i in items) / st.pvariance(tot))
out = os.path.join(data, "ai_standalone_grades.csv")
with open(out, "w", newline="") as fh:
    w = csv.DictWriter(fh, fieldnames=list(rows[0])); w.writeheader(); w.writerows(rows)
print(f"wrote {out}: {len(rows)} notebooks | alpha three raters {alpha(['z_humanAB','z_humanC','z_gemini']):.3f}, "
      f"two humans {alpha(['z_humanAB','z_humanC']):.3f}")
