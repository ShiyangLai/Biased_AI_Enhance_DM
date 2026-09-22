#!/usr/bin/env python3
"""
strip_annotations.py — reduce the two LLM-annotation files to their scores.

The annotation runs (GPT-5.5 over the session logs) wrote, alongside each score,
the evidence they relied on: engagement_annotations carries an `evidence_json`
column quoting student prompts, and delegation_annotations a `rationale` column
paraphrasing each prompt. Both are student text or a close derivative and are
not released. The analyses use only the scores, so this script keeps:

    engagement_annotations.csv   student_id, week, the five 0-3 dimensions
                                 (behavioral, cognitive, emotional, autonomy,
                                 social_presence), overall_engagement, n_exchanges,
                                 n_chars, truncated, model, status
    delegation_annotations.csv   request_id, student_id, week, policy_used,
                                 category, autonomy_score, model, status

request_id is an opaque per-message hash carried over from the log.

NOT run by run_all.R; shipped so the derivation is inspectable.
Usage: python3 strip_annotations.py <engagement_annotations_full.csv> <delegation_annotations_full.csv> [education/data]
"""
import csv, os, sys
data = sys.argv[3] if len(sys.argv) > 3 else os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..", "data")
KEEP = {"engagement_annotations.csv": ["student_id","week","behavioral","cognitive","emotional","autonomy","social_presence",
                                       "overall_engagement","n_exchanges","n_chars","truncated","model","status"],
        "delegation_annotations.csv": ["request_id","student_id","week","policy_used","category","autonomy_score","model","status"]}
for src, (name, cols) in zip(sys.argv[1:3], KEEP.items()):
    rows = list(csv.DictReader(open(src, encoding="utf-8")))
    with open(os.path.join(data, name), "w", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=cols); w.writeheader(); w.writerows({c: r[c] for c in cols} for r in rows)
    print(f"wrote {name}: {len(rows)} rows, dropped {sorted(set(rows[0]) - set(cols))}")
