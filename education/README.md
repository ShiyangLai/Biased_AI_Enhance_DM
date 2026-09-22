# Experiment 3 — Education

Materials to reproduce the classroom study: a nine-week graduate course
(12 January – 14 March 2026) in which students completed a weekly coding
assignment and a weekly memo with an agentic coding assistant whose
**epistemic orientation** was experimentally manipulated, week by week, within
each student. Every student worked under the unmodified default assistant in
most weeks and was independently randomised to two treatment weeks (uniform
over weeks 2–9, at least two weeks apart), in which the assistant took two
different non-default configurations.

| | Classroom study |
|---|---|
| Students | 39 (40 enrolled; 39 with valid data) |
| Student-weeks | 351 (39 × 9) |
| Assistant arms | Default, Novelty-biased, Reliability-biased, Neutralized, Questioning-oriented |
| Graded deliverables | 277 coding assignments, 298 memos |

The main-text analyses compare the default, novelty-biased and
reliability-biased configurations, an analytic sample of **312 student-weeks**
(273 default, 19 novelty, 20 reliability). The neutralized arm (16 weeks) is
analysed in the Extended Data scripts on a four-arm frame of 328 student-weeks;
the questioning-oriented arm (23 weeks) is out of scope for every analysis here.

Unlike the two survey experiments, treatment is assigned *within* students
across time. The primary specification is therefore a within-student estimator
— outcome on assistant configuration with week and student fixed effects and
student-clustered standard errors — so each student serves as their own control.
The design was powered only for large effects, and the scripts report full
specification ladders rather than a preferred estimate.

---

## Layout

```
education/
├── data/          de-identified student-level data (15 files)
├── code/          analysis scripts, run via code/run_all.R
│   └── upstream/  the transcript-reduction, annotation-stripping and
│                  stand-alone-grading steps that produced several inputs, and
│                  two matplotlib figures; not part of run_all.R (see below)
├── figures/       output directory (created/populated at run time)
└── README.md
```

## Quick start

```r
setwd("education/code")    # scripts use paths relative to this directory
source("run_all.R")        # installs missing packages, then runs everything
```

`run_all.R` takes under a minute on a laptop; the two ordinal `MCMCglmm`
specification checks account for most of it. Figures and regression tables
(as `.md`) land in `../figures/`; the balance and descriptives CSVs are written
to `../data/`.

Scripts are **independent of one another** — each reads the prepared CSVs in
`../data` and writes its own outputs — with one exception: `table_delegation.R`
and `table_verification_share.R` read a contrasts file written by
`delegation_by_arm.R`, which `run_all.R` orders first. Any other script can be
run on its own:

```r
source("_setup.R"); source("bias_magnitude_outcomes.R")
```

`_setup.R` defines `DATA_DIR` and `FIG_DIR`, the `FONT` and `save_png()`
helpers, and the default graphics device, so it must be sourced first in a
fresh session.

### Requirements

R ≥ 4.4 (developed on 4.4.1). `_setup.R` installs anything missing from CRAN —
principally **dplyr**, **readr**, **ggplot2**, **emmeans**, **sandwich**,
**lmtest**, **lme4/lmerTest**, **ordinal**, **MCMCglmm**, **performance**,
**ggpattern**, **patchwork**, and **ragg**.

Figures request the *Avenir* typeface, which ships only with macOS. On other
platforms `_setup.R` aliases it to an available sans-serif; plots then differ in
typeface but in no plotted value. `_setup.R` also routes the default graphics
device to `ragg`, without which several scripts abort under `Rscript` with
`invalid font type`. Under ggplot2 ≥ 4.0 the scripts emit a deprecation
warning for `geom_errorbarh()`; it is harmless.

## Data

Student identifiers are replaced with sequential codes (`student_001` …
`student_064`, shared across every file so joins are preserved); final-project
teams are `proj_01` … `proj_22`. No names, free-text responses or platform
identifiers are included. Two files that would otherwise carry verbatim student
text are reduced before release (see *Upstream steps*).

**Student-week frames**

| File | Rows | Unit |
|---|---|---|
| `scores_all_graders_student_week.csv` | 351 | one row per student × week, all five arms; every grader's coding and memo scores |
| `bm_student_week.csv` | 312 | the three-arm analytic frame with telemetry, prior experience and direction codes |
| `c1_echo_student_week.csv` | 312 | the same, plus each student's pre-survey novelty/reliability lean and the echo-chamber / opposition coding |
| `supplementary_dnr_student_week.csv` | 312 | the same, plus all 26 pre-survey covariates under short codes and the treatment order |
| `coding_regrade_student_week.csv` | 277 | mean of the two human coding grades, submitted weeks |
| `memo_regrade_student_week.csv` | 298 | mean of the two human memo grades, submitted weeks |
| `telemetry_student_week.csv` | 351 | reply turns and words written per student-week, derived from the session log |
| `post_survey_student_week.csv` | 180 | post-assignment survey responses (seven 1–7 items), where completed |

**Student-level frames**

| File | Rows | Unit |
|---|---|---|
| `pre_survey_covariates_student.csv` | 39 | the pre-course questionnaire, verbatim item wording as column names |
| `final_project_student.csv` | 39 | final-project grades, assigned-arm exposure and usage totals per student |
| `final_project_docs.csv` | 25 | one row per assessed final-project write-up: human and LLM essay grades, innovativeness. The 21 rows with a `doc_code` match a team in `final_project_student.csv` and enter the analyses; the other four are single-author write-ups with an LLM innovativeness rating but no human grade or team match |

**Annotations and stand-alone runs**

| File | Rows | Unit |
|---|---|---|
| `engagement_annotations.csv` | 137 | five annotated engagement dimensions (0–3) per student-week with any conversation |
| `delegation_annotations.csv` | 2,697 | delegation category and 0–3 autonomy score per student prompt, biased and neutralized weeks |
| `ai_standalone_grades.csv` | 33 | grades of the assignments each configuration completed with no student, three raters and their composite |
| `week_workload_summary.csv` | 9 | size of each week's assignment template (cells, code lines, words, characters) |

Key columns:

- `student_id`, `week`, `policy` — student, course week (1–9), assigned
  configuration (`DEFAULT`, `NOVELTY_BIASED`, `RELIABILITY_BIASED`, `NEUTRAL`,
  `QUESTIONING_ORIENTED`); `is_treatment_week` flags the two randomised weeks
- `asgn_score` / `memo_score`, `gemini_*`, `ai_*` — the human mean, Gemini 3.1
  Pro and GPT-5.4 grades; `combined_*` are their composites. The analyses use
  `coding_regrade` (two humans) with Gemini mean-aligned and added at equal
  per-grader weight for coding, and the two-human mean alone for the memo
- `conv_round`, `conv_length` — student messages and words in that week's
  sessions; a week with no conversation is 0
- `usefulness`, `comfort`, `perceived_performance`, … — the post-assignment
  survey items (1–7)
- `prior_prog_exp`, `prior_ml_exp` — self-rated prior experience (1–3)
- `direction`, `bias_pooled` — the arm's direction and a biased / not-biased flag
- `lean`, `direction_score`, `bias_side_dominant` — the student's pre-survey
  lean and whether the assigned bias matched it (echo) or opposed it
- `got_nov`, `got_rel`, `got_neu`, `got_ques`, `n_biased_assigned` —
  assigned-arm indicators and the count of directionally biased weeks (0–2)
- `essay`, `innov_mean` — the human final-project grade and the mean GPT-5.4 /
  Gemini 3.1 Pro innovativeness rating of the write-up
- `composite` — the stand-alone grade: three raters standardised and averaged
- `effective_source_chars` and companions — requirement-adjusted template size,
  the pre-treatment weight used in the volume-weighted specifications

The session transcripts (one row per message, with the message text), the
evidence and rationale columns of the two annotation files, the stand-alone
notebooks and their blinding key, the per-grader human scores, and the raw
grading sheets are not included: they carry verbatim student text or would
identify graders. They are available from the authors.

## What each script produces

**Sample description and randomisation checks**

| Script | Output |
|---|---|
| `descriptives_table_education.R` | `descriptives_table_education.{csv,md,tex}` — Table S11: continuous and binary measures, student-level and student-week panels |
| `balance_check_education.R` | `balance_education.csv` — covariate balance, novelty vs reliability, with a re-randomisation null for each covariate |

**Performance**

| Script | Output |
|---|---|
| `plot_performance_treatment.R` | `plot_performance_treatment.png` — three-arm coding and memo ladders |
| `bias_magnitude_outcomes.R` | `bias_magnitude_outcomes.png` — Fig. 2c, 3c, 3d: assignment performance, reply turns and perceived helpfulness, default vs biased, split by direction; `bm_*.csv` contrasts |
| `bias_side_performance.R` | `bias_side_performance{,_4cell}.png` — Fig. 2g: echo chamber vs opposition on both deliverables; `c1_*.csv` |

**Engagement, delegation and perception**

| Script | Output |
|---|---|
| `engagement_coef_heatmap_education.R` | `engagement_coef_heatmap_education.png` — arms × five annotated engagement dimensions against the default baseline |
| `delegation_by_arm.R` | `delegation_by_arm.png` — Extended Data Fig. 2d: delegation composition of student prompts; `d1_delegation_{shares,contrasts}.csv` |

**Downstream: final project**

| Script | Output |
|---|---|
| `final_project_models.R` | `final_project_{models,correlations,doc_correlations}.csv` — dose and exposure models with HC3 standard errors (console) |
| `final_project_outcomes.R` | `final_innov_by_arm.png` — Extended Data Fig. 4j; `final_{essay,innov}_vs_dose.png` — the SI dose figures (Huber robust fits) |

**Stand-alone assistant runs**

| Script | Output |
|---|---|
| `ai_standalone_performance.R` | `ai_standalone_performance.png` — Extended Data Fig. 6c: the assistants' own assignment grades by configuration |

**Extended Data: the neutralized arm**

| Script | Output |
|---|---|
| `neutral_arm_performance.R` | `neutral_arm_performance.png` — Extended Data Fig. 4i; `neutral_arm_performance_{contrasts,pairwise}.csv` |
| `neutral_arm_engagement.R` | `neutral_arm_engagement{,_reply_turns}.png` — Extended Data Fig. 4k |
| `neutral_arm_helpfulness.R` | `neutral_arm_helpfulness{,_all_weeks}.png` — Extended Data Fig. 4l |

**Robustness**

| Script | Output |
|---|---|
| `student_effects_robustness.R` | `bm_student_effects.csv` — clustered-only, random-intercept and fixed-effect estimators side by side |
| `perceived_spec_check.R`, `usefulness_spec_check.R` | `*_spec_check.csv` — ordinal (`clm`, `clmm`, `MCMCglmm`) against linear specifications for the two survey outcomes |

**SI regression tables** (each writes a `.md` table to `../figures/`)

| Script | Table |
|---|---|
| `table_performance_three_arm.R` | novelty and reliability vs default, coding and memo, five specifications |
| `table_performance_by_direction.R` | the same as two-arm ladders (`table_performance_{novelty,reliability,pooled}.md`) |
| `table_echo_chamber.R` | echo chamber vs opposition, coding |
| `table_engagement_three_arm.R` | log reply turns, three arms, use-conditioned and all weeks |
| `table_helpfulness_three_arm.R` | perceived helpfulness, three arms, with an ordinal column |
| `table_delegation.R` | delegation categories and autonomy score by arm |
| `table_verification_share.R` | the single verification-share contrast quoted in the main text |
| `table_performance_four_arm.R` | assignment performance including the neutralized arm |
| `table_engagement_helpfulness_four_arm.R` | reply turns and helpfulness including the neutralized arm |
| `table_final_project.R` | assigned-arm exposure and final-project innovativeness, three inference methods |
| `table_grader_weight_ladders.R` | sensitivity to the grader composite and to the observation weight, with Kish effective *n* |

### Upstream steps

`code/upstream/` holds the Python that produced several inputs and two SI
figures. These are **not** run by `run_all.R`: five of them read the withheld
transcripts, notebooks or per-grader sheets, and the two figure scripts need
`matplotlib` rather than R. They are included so the derivation of every
shipped file is inspectable.

| Script | Produces |
|---|---|
| `derive_telemetry.py` | `telemetry_student_week.csv` from the session log; reproduces the `conv_round` / `conv_length` columns of `bm_student_week.csv` exactly |
| `strip_annotations.py` | the two annotation files with their evidence and rationale columns removed |
| `build_blinded_package.py` | the blinded stand-alone notebooks sent to graders (condition labels removed, identifiers randomised within week) and the un-blinding key |
| `collate_grades.py`, `merge_standalone_raters.py` | `ai_standalone_grades.csv`: the two human passes and Gemini, standardised and combined (Cronbach's α 0.770) |
| `figure_categorical_descriptives.py` | `figures/categorical_descriptives.png` — Fig. S8, the ten categorical pre-survey items |
| `figure_balance_education.py` | `figures/balance_education.png` — Fig. S9, the balance love plot with each covariate's re-randomisation null; reads the CSV written by `balance_check_education.R` |

The two figure scripts run from `education/code` with
`python3 upstream/<script>.py` and need **pandas**, **numpy**, **matplotlib**
and (for the density panel formerly in this folder) **scipy**. None of the
upstream scripts contains credentials; the LLM-annotation steps that produced
the annotation files are described in the SI and are not reproduced here.

## Checking your run

A successful run reproduces, among others:

- `bias_magnitude_outcomes.R` — novelty-biased vs default coding grade,
  +0.541 (student-clustered SE 0.270), adjusted *p* = 0.093; reply turns,
  reliability-biased vs default, +0.610 (0.291); perceived helpfulness,
  novelty-biased vs default, −0.674 (0.279)
- `bias_side_performance.R` — echo chamber vs opposition on coding, −0.014
  (*p* = 0.979)
- `balance_check_education.R` — 18 of 26 covariates past 0.10; under
  re-randomisation a median of 20 (*P* = 0.870), and no covariate past its own
  95th percentile
- `delegation_by_arm.R` — verification-prompt share 0.444 (novelty) vs 0.250
  (reliability), exact permutation *p* = 0.055
- `ai_standalone_performance.R` — stand-alone grades 6.95 (novelty),
  6.06 (default), 6.87 (reliability)
- `final_project_outcomes.R` — innovativeness by assigned exposure, +0.268
  (novelty), +0.437 (reliability), −0.191 (neutralized)

`MCMCglmm` models in the two specification checks are stochastic; seeds are set
where the original analysis set them, but posterior means on the latent scale
still move between runs, so those ordinal coefficients will not reproduce to
three decimals. The balance check (5,000 re-randomisations, seed 42) and the
final-project randomisation inference (20,000 draws, seed 11) are seeded and
reproduce exactly; the delegation permutation test enumerates all 12,376
assignments and is deterministic.
