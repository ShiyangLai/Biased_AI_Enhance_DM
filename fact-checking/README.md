# Experiment 1 — Fact-checking

Materials to reproduce the fact-checking study: two randomised experiments in
which participants judged the veracity of news headlines before and after
consulting one or two LLM assistants whose political stance was experimentally
manipulated.

| | Study 1 (single assistant) | Study 2 (dual assistants) |
|---|---|---|
| Participants | 993 | 1,499 |
| Evaluations (3 headlines each) | 2,979 | 4,497 |
| Assistant stance arms | Default, Neutralized, Republican-leaning, Democrat-leaning | 16 ordered pairs of the four stances |

The main text analyses exclude the neutralized arm, giving an analytic sample of
**814 participants / 2,442 evaluations**; the neutralized arm is analysed
separately in the Extended Data scripts.

---

## Layout

```
fact-checking/
├── data/          de-identified participant-level data (2 files)
├── code/          analysis scripts, run via code/run_all.R
├── figures/       output directory (created/populated at run time)
└── README.md
```

## Quick start

```r
setwd("fact-checking/code")   # scripts use paths relative to this directory
source("run_all.R")           # installs missing packages, then runs everything
```

`run_all.R` takes roughly 20–40 minutes; the ordinal models
(`MCMCglmm`, 25,000 iterations) account for most of that. Figures land in
`../figures/`; regression tables print to the console.

To run a single analysis, source the pipeline first — the scripts share one
environment and later scripts depend on columns earlier ones add:

```r
source("_setup.R"); source("preprcessing.R"); source("second_figure_b1.R")
```

### Requirements

R ≥ 4.4 (developed on 4.4.1). `_setup.R` installs anything missing from CRAN —
principally **dplyr**, **ggplot2**, **lme4/lmerTest**, **emmeans**, **ordinal**,
**MCMCglmm**, **fixest**, **sandwich**, **lmtest**, **mediation**,
**performance**, and **ragg**.

Figures request the *Avenir* typeface, which ships only with macOS. On other
platforms `_setup.R` aliases it to an available sans-serif; plots then differ in
typeface but in no plotted value. `_setup.R` also routes the default graphics
device to `ragg`, without which several scripts abort under `Rscript` with
`invalid font type`.

## Data

| File | Rows | Unit |
|---|---|---|
| `data/encrypted_ai1.csv` | 2,979 | one row per participant × headline, single-assistant study |
| `data/encrypted_ai2.csv` | 4,497 | one row per participant × headline, dual-assistant study |
| `data/five_arm_single_dual.csv` | 3,303 | derived; single/dual five-arm comparison frame, written by `export_five_arm_data.R` |

Participant identifiers are replaced with sequential integers; no free-text
responses, IP addresses, or platform identifiers are included. Key columns:

- `UID`, `NID` — participant and news-item identifiers
- `PreEva`/`PreConf`, `PostEva`/`PostConf` — veracity judgement and confidence,
  before and after the interaction. `preprcessing.R` maps these onto a 0–1
  scale and scores them against `Truth` to build `PrePerformance` and
  `PostPerformance`
- `AIStanceLabel` — the assigned assistant stance (a pair, for Study 2)
- `AICorrectness` / `AI1Correctness`, `AI2Correctness` — the assistant's own
  accuracy on that headline
- `PoliBias` — political slant of the news item (Republican / Neutral / Democrat)
- `ConvLength`, `Engagement*` — conversation length in participant words, and
  five annotated engagement dimensions
- `PerceivedImprove`, `AIInterMean`, `WillRecommendAI` — post-interaction
  perception measures

The LLM-annotation validation inputs (Snopes/PolitiFact items scored by GPT-4o)
and the raw survey exports are not included here: the former are large and the
latter carry platform identifiers. Both are available from the authors.

## What each script produces

**Pipeline**

| Script | Purpose |
|---|---|
| `_setup.R` | Packages, font fallback, graphics device, `../figures/` |
| `preprcessing.R` | Builds `single_ai_processed` and `df2`; derives performance, stance codes, and the participant–assistant stance relationship |

**Single assistant, five treatment arms** (extreme/somewhat Republican, default, somewhat/extreme Democrat)

| Script | Output |
|---|---|
| `first_figure_single_a1_separated.R` | `performance_comparison.png` — post-interaction performance ladder |
| `first_figure_single_a2_separated.R` | Evaluative bias across news slants |
| `first_figure_single_a3_separated.R` | `reply_turns_single.png`, `raddar_single.png` — conversation length, engagement profile |
| `first_single_figure_a4.R` | Persuasion / backfire decomposition (four cells, τ = 0.1) |
| `first_single_figure_a4_separated.R` | The same decomposition split by assistant stance |

**Bias magnitude** (default / moderate / strong)

| Script | Output |
|---|---|
| `second_figure_b1.R` | `second_figure_b1.png` — performance, conversation length, perceived improvement |
| `second_figure_b2.R` | `second_figure_b2.png`, `second_figure_b2_ai_correctness_density.png` — the assistant's own accuracy by bias magnitude |

**Participant–assistant stance relationship** (echo chamber vs. opposition)

`third_figure_c1.R` (`relative_bias_misinfo.png`), `third_figure_c4.R`, `third_figure_c5.R`

**Dual assistants**

| Script | Output |
|---|---|
| `forth_figure_d1.R` | Performance across the five single/dual configurations |
| `forth_figure_d2.R` | Perceived improvement (ordinal MCMCglmm) |
| `forth_figure_d3.R` | Supporting comparisons |
| `forth_figure_d4.R` | `conversation_length.png` |
| `dual_ai_decomposition_v2.R` | `dual_ai_decomposition_v2.png` — six-cell persuasion / backfire / advisor-selection decomposition |
| `export_five_arm_data.R` | Writes `../data/five_arm_single_dual.csv` (3,303 rows), the analysis frame for the five-arm comparison |

**Engagement**

`engagement_coef_heatmap.R` → `engagement_coef_heatmap.png`, seven arms × five annotated engagement dimensions against the single-default baseline.

### Standalone entry point

`five_arm_analysis_standalone.R` reproduces the single-vs-dual five-arm
comparison — performance and perceived improvement, with FDR-adjusted pairwise
contrasts and Hedges' *g* — from `data/five_arm_single_dual.csv` alone, without
running the rest of the pipeline:

```r
setwd("fact-checking/code"); source("_setup.R"); source("five_arm_analysis_standalone.R")
```

The CSV is produced by `export_five_arm_data.R`, which `run_all.R` runs after
`forth_figure_d1.R`. If you have not run the pipeline, generate it first.

**Extended Data**

| Script | Output |
|---|---|
| `extended_neutral_comparison.R` | `extended_neutral_comparison.png`, `_biasgap.png`, `_convlength.png`, `_meaningfulness.png` — the neutralized arm against default and the two partisan arms |
| `ai_correctness_by_arm.R` | `ai_correctness_density_five_arm.png` — assistant accuracy distribution by arm |
| `dual_selection_quality.R` | `dual_selection_quality.png` — whether participants follow the more accurate advisor when the two disagree |

## Checking your run

A successful run reproduces, among others:

- `preprcessing.R` — 2,979 single-assistant rows / 993 participants; 4,497 dual rows / 1,499 participants
- `second_figure_b1.R` — assistant accuracy by bias magnitude: 0.633 (default), 0.684 (moderate), 0.674 (strong)
- `forth_figure_d4.R` — conversation length, Republican-leaning vs default: 8.756 words, 95% CI [5.159, 12.353]

`MCMCglmm` models are stochastic. Seeds are set where the original analysis set
them, but posterior means on the latent scale still move by roughly 0.07–0.18
between runs; the ordinal coefficients will not reproduce to three decimals.

## Notes and known limitations

- Scripts share a global environment and must run in the order given in
  `run_all.R`.
- Five scripts from the working analysis are not included because they do not
  currently run end-to-end (`third_figure_c2.R`, `third_figure_c3.R`,
  `discussion_1.R`, `discussion_2.R`, `SM_dual_analysis.R`). They cover
  supplementary willingness-to-recommend and dual-AI supplementary material, not
  the main-text results.
- `forth_figure_d2.R` and `five_arm_analysis_standalone.R` filtered complete
  cases on `AICorrectness` alone, while `MCMCglmm` errors on an NA in any fixed
  predictor; both now filter on the full predictor set. This drops rows with a
  missing political-ideology response (7 single-assistant, 12 in the five-arm
  frame), so their ordinal estimates shift slightly against earlier runs.
- `five_arm_analysis_standalone.R` also read its input with `readr::read_csv`;
  `MCMCglmm` indexes its response with base `data.frame` semantics and a tibble
  returns a one-column tibble rather than a vector. It now coerces on read.
- `forth_figure_d4.R` had a reporting block referencing an `Effect_Size` column
  that was never constructed; it is now derived from Hedges' *g* using
  conventional cut-offs. No estimate or test is affected.
