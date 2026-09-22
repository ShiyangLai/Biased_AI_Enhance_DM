#!/usr/bin/env python3
"""
NOT run by run_all.R. This step needs the withheld stand-alone notebooks and the
un-blinding key, so it cannot execute from the repository; it is included so the
derivation of data/ai_standalone_grades.csv is inspectable end to end.


build_blinded_package.py — turn the raw runs_s45 pull into a blinded grading
package plus the un-blinding key.

WHAT GETS SHIPPED. One canonical notebook per (week, condition): weekN/<cond>/weekN.ipynb,
41 in total (week 1 is default-only; weeks 2-9 have all five conditions). The 15
variant files in the repo (*_backup, *_executed, *_mock, *_final, *_cpu, ...) are
NOT shipped: they are working copies, and week9/neutral alone has eight of them, so
including them would hand graders eight near-duplicates of a single condition.

WHY SCRUBBING IS NEEDED. Renaming the folders is not sufficient. Three notebooks
print absolute paths in their cell OUTPUTS that name the condition directly, e.g.
  /home/gio/edu_AI_agent_only/agent_sandbox/runs_s45/week4/neutral/data/census_healthcare.npz
A grader reading a traceback would see the arm. The same paths also carry
"edu_AI_agent_only", which reveals that the work is agent-produced, and the operator
name "gio". All three are rewritten.

WHAT IS DELIBERATELY NOT SCRUBBED. Condition words that are ordinary vocabulary or
substantive content stay: week 9's assignment is literally titled "Alignment, Ethics,
Safety, and Novelty" so "novelty" appears in all five week-9 arms; "default" appears
as PyTorch default initialisation; and week2/novelty's own analysis explores a
"novelty dimension" in word embeddings. That last one is a behavioural signature of
the arm, visible in the work itself -- blinding removes labels, not behaviour, and
stripping it would corrupt the very thing being graded.

BLINDING SCHEME. Condition ids are randomised INDEPENDENTLY WITHIN EACH WEEK, so a
grader who forms an impression of "submission 3" in one week learns nothing about
submission 3 in another. Seed is fixed so the mapping is reproducible; the mapping is
also written to the key CSV, which is written OUTSIDE the zip.

Outputs (relative to this script's directory):
  raw/                     all 56 notebooks, original names  (never send this)
  blinded/weekN/weekN_subM.ipynb
  ai_assignments_blinded.zip
  UNBLINDING_KEY.csv       week, blinded_id, condition, source_path  (never send this)

Run: python3 build_blinded_package.py
"""
import csv
import json
import os
import random
import re
import shutil
import zipfile

SEED = 20260816
CONDS = ["default", "neutral", "novelty", "questioning", "reliability"]
HERE = os.path.dirname(os.path.abspath(__file__))
RAW = os.path.join(HERE, "raw")
BLIND = os.path.join(HERE, "blinded")
ZIP = os.path.join(HERE, "ai_assignments_blinded.zip")
KEY = os.path.join(HERE, "UNBLINDING_KEY.csv")

# Ordered: the condition-naming project path must be rewritten before the generic
# /home/gio/ rule, otherwise the condition segment would survive.
PROJ = re.compile(
    r"/home/gio/edu_AI_agent_only/agent_sandbox/runs_s45/week(\d+)/(?:%s)" % "|".join(CONDS))
RUNS = re.compile(r"(?:[\w./\\-]*)runs_s45[/\\]week(\d+)[/\\](?:%s)" % "|".join(CONDS))
HOME = re.compile(r"/home/gio\b")


def scrub_text(s):
    if not isinstance(s, str) or not s:
        return s, 0
    n = 0
    s, k = PROJ.subn(lambda m: "/workspace/week%s" % m.group(1), s); n += k
    s, k = RUNS.subn(lambda m: "/workspace/week%s" % m.group(1), s); n += k
    s, k = HOME.subn("/home/user", s); n += k
    return s, n


def scrub_field(v):
    """Recursively scrub strings inside a notebook JSON fragment."""
    if isinstance(v, str):
        return scrub_text(v)
    if isinstance(v, list):
        out, n = [], 0
        for item in v:
            r, k = scrub_field(item); out.append(r); n += k
        return out, n
    if isinstance(v, dict):
        out, n = {}, 0
        for key, item in v.items():
            r, k = scrub_field(item); out[key] = r; n += k
        return out, n
    return v, 0


def scrub_notebook(src, dst):
    nb = json.load(open(src, encoding="utf-8"))
    nb, n = scrub_field(nb)
    with open(dst, "w", encoding="utf-8") as fh:
        json.dump(nb, fh, ensure_ascii=False)
    return n


def verify(path):
    """Re-read a blinded notebook and assert no condition-labelling path survives."""
    txt = open(path, encoding="utf-8").read()
    bad = []
    for pat, why in ((PROJ, "project path naming a condition"),
                     (RUNS, "runs_s45 path naming a condition"),
                     (HOME, "operator home directory"),
                     (re.compile(r"edu_AI_agent_only"), "agent-only provenance"),
                     (re.compile(r"agent_sandbox"), "repo name")):
        if pat.search(txt):
            bad.append(why)
    return bad


def main():
    src_root = os.path.join(RAW, "runs_s45")
    if not os.path.isdir(src_root):
        raise SystemExit("raw/runs_s45 not found - run the pull step first")

    shutil.rmtree(BLIND, ignore_errors=True)
    os.makedirs(BLIND, exist_ok=True)
    rng = random.Random(SEED)

    rows, total_sub, problems = [], 0, []
    for w in range(1, 10):
        present = [c for c in CONDS
                   if os.path.exists(os.path.join(src_root, f"week{w}", c, f"week{w}.ipynb"))]
        ids = list(range(1, len(present) + 1))
        rng.shuffle(ids)                      # independent permutation per week
        os.makedirs(os.path.join(BLIND, f"week{w}"), exist_ok=True)
        for cond, bid in zip(present, ids):
            src = os.path.join(src_root, f"week{w}", cond, f"week{w}.ipynb")
            dst = os.path.join(BLIND, f"week{w}", f"week{w}_sub{bid}.ipynb")
            n = scrub_notebook(src, dst)
            bad = verify(dst)
            if bad:
                problems.append((dst, bad))
            rows.append(dict(week=w, blinded_id=bid, blinded_file=f"week{w}/week{w}_sub{bid}.ipynb",
                             condition=cond, replacements=n,
                             source_path=f"runs_s45/week{w}/{cond}/week{w}.ipynb"))
            total_sub += 1

    if problems:
        for p, why in problems:
            print(f"  !! {p}: {', '.join(why)}")
        raise SystemExit("ABORT: identifying strings survived scrubbing; zip not written")

    rows.sort(key=lambda r: (r["week"], r["blinded_id"]))
    with open(KEY, "w", newline="", encoding="utf-8") as fh:
        wr = csv.DictWriter(fh, fieldnames=["week", "blinded_id", "blinded_file",
                                            "condition", "replacements", "source_path"])
        wr.writeheader(); wr.writerows(rows)

    if os.path.exists(ZIP):
        os.remove(ZIP)
    with zipfile.ZipFile(ZIP, "w", zipfile.ZIP_DEFLATED) as z:
        for w in range(1, 10):
            d = os.path.join(BLIND, f"week{w}")
            if not os.path.isdir(d):
                continue
            for f in sorted(os.listdir(d)):
                z.write(os.path.join(d, f), arcname=f"ai_assignments/week{w}/{f}")

    fixed = sum(r["replacements"] for r in rows)
    print(f"blinded {total_sub} notebooks across 9 weeks; {fixed} identifying strings rewritten")
    print(f"verification passed: no condition-naming path, no operator home, no repo name")
    print(f"  zip -> {ZIP}  ({os.path.getsize(ZIP)/1e6:.1f} MB)")
    print(f"  key -> {KEY}   (DO NOT send this to graders)")


if __name__ == "__main__":
    main()
