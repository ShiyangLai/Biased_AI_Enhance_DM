#!/usr/bin/env python3
"""
derive_telemetry.py — build data/telemetry_student_week.csv from the raw session log.

The log behind the classroom deployment (one row per student message: request_id,
ts_in, student_id, week, policy_used, model_id, user_words, user_text) carries
verbatim student prompts and is not released. Every analysis that used it needs
only two per-student-week counts, so this script reduces it to those, complete
over the 351 (student, week) cells of the deployment:

    conv_round   number of student messages in that week's sessions
    conv_length  total words the student wrote to the assistant

A student-week with no messages is written as 0, not omitted. The result
reproduces the conv_round / conv_length columns of bm_student_week.csv exactly
(0 mismatches on its 312 rows), which is the check printed at the end.

NOT run by run_all.R (the log is withheld); shipped so the derivation is inspectable.
Usage: python3 derive_telemetry.py <raw_user_transcript_log.csv> [education/data]
"""
import collections, csv, os, sys
csv.field_size_limit(10**9)
log = sys.argv[1]
data = sys.argv[2] if len(sys.argv) > 2 else os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..", "data")
spine = [(r["student_id"], int(float(r["week"]))) for r in csv.DictReader(open(os.path.join(data, "scores_all_graders_student_week.csv")))]
agg = collections.defaultdict(lambda: [0, 0])
with open(log, encoding="utf-8", errors="replace") as fh:
    for r in csv.DictReader(fh):
        k = (r["student_id"], int(float(r["week"]))); agg[k][0] += 1
        try: agg[k][1] += int(float(r["user_words"] or 0))
        except ValueError: pass
out = os.path.join(data, "telemetry_student_week.csv")
with open(out, "w", newline="") as fh:
    w = csv.writer(fh); w.writerow(["student_id", "week", "conv_round", "conv_length"])
    for k in spine: w.writerow([k[0], k[1], agg[k][0], agg[k][1]])
bm = {(r["student_id"], int(float(r["week"]))): (int(float(r["conv_round"])), int(float(r["conv_length"])))
      for r in csv.DictReader(open(os.path.join(data, "bm_student_week.csv")))}
mism = sum(tuple(agg[k]) != v for k, v in bm.items())
print(f"wrote {out}: {len(spine)} rows; mismatches vs bm_student_week.csv: {mism}")
