# Experiment 2 — Investment

Materials to reproduce the investment study: randomised experiments in which
participants built a $100,000 portfolio from 25 assets, revised it after
consulting one or two LLM assistants whose **risk orientation** was
experimentally manipulated, and were then scored on how that portfolio actually
performed over the following two weeks.

| | Study 1 (single assistant) | Study 2 (dual assistants) |
|---|---|---|
| Participants | 1,344 | 1,011 |
| Allocations (one each) | 1,344 | 1,011 |
| Assistant risk arms | Default, Neutralized, Somewhat/Extremely Risk-Averse, Somewhat/Extremely Risk-Seeking | Default pair, Balanced pair, Opposition pair |

The single-assistant total includes a **priming variant** (*n* = 302) in which
the assistant was the unmodified default and only the *label* shown to
participants was manipulated. The main-text analyses use the six-arm sample
(*n* = 1,042) and exclude the neutralized arm, giving an analytic sample of
**864 participants**; the neutralized arm is analysed separately in
`neutral_arm_analysis.R`.

Unlike the fact-checking study, the outcome here is not graded against a known
truth but realised in the market: each portfolio is scored by its **active
M²**, the risk-standardised return relative to holding the market over the
participant's own 14-day evaluation window. Participants were paid a
performance bonus ranked on exactly this measure and were told so in advance.

---

## Layout

```
investment/
├── data/          de-identified participant-level data (20 files)
├── code/          analysis scripts, run via code/run_all.R
│   └── upstream/  the LLM-annotation and market-data steps that produced
│                  several inputs; not part of run_all.R (see below)
├── figures/       output directory (created/populated at run time)
└── README.md
```

## Quick start

```r
setwd("investment/code")   # scripts use paths relative to this directory
source("run_all.R")        # installs missing packages, then runs everything
```

`run_all.R` takes roughly 30–60 minutes. Two things dominate: the permutation
tests for portfolio diversity (10,000 replicates, twice over) and the ordinal
`MCMCglmm` models in `perception_outcomes.R`. Figures land in `../figures/`;
regression tables print to the console.

Scripts here are **independent of one another** — each reads the prepared CSVs
in `../data` and writes its own figures. Any one can be run on its own:

```r
source("_setup.R"); source("bias_magnitude_outcomes.R")
```

`_setup.R` defines `DATA_DIR` and `FIG_DIR`, which every script uses, so it must
be sourced first in a fresh session. The one exception to independence is
`plot_active_m2_treatment.R`, which sources the small helper
`preprocess_active_m2.R` from the same directory.

### Requirements

R ≥ 4.4 (developed on 4.4.1). `_setup.R` installs anything missing from CRAN —
principally **dplyr**, **ggplot2**, **emmeans**, **sandwich**, **lmtest**,
**clubSandwich**, **MCMCglmm**, **nnet**, **quantreg**, **patchwork**,
**ggdist**, and **ragg**.

Figures request the *Avenir* typeface, which ships only with macOS. On other
platforms `_setup.R` aliases it to an available sans-serif; plots then differ in
typeface but in no plotted value. `_setup.R` also routes the default graphics
device to `ragg`, without which several scripts abort under `Rscript` with
`invalid font type`.

## Data

Participant identifiers are replaced with sequential integers (1–2,410, shared
across every file so joins are preserved); session identifiers are likewise
re-coded. No free-text responses, IP addresses, or platform identifiers are
included. Fifteen participants took part in more than one experiment and carry
the same integer in each, which is why the id range exceeds any single sample.

**Participant-level frames**

| File | Rows | Unit |
|---|---|---|
| `active_m2_treatment_data.csv` | 1,042 | one row per single-assistant participant (six arms) |
| `dual_active_m2.csv` | 1,011 | one row per dual-assistant participant |
| `priming_active_m2.csv` | 302 | the label-only priming variant |
| `participant_covariates.csv` | 1,043 | 16 pre-interaction questionnaire measures, single |
| `dual_covariates.csv` | 1,012 | the same, dual |
| `demographics.csv` | 2,399 | age and sex |
| `participant_portfolios.csv` | 1,043 | pre/post allocation weights over 25 assets, single |
| `dual_portfolios.csv` | 1,012 | the same, dual |
| `perceived_improvement.csv` | 2,055 | pre/post confidence, perceived improvement, expected performance |
| `perceived_ai_role.csv` | 1,023 | tool vs influencing-agent item |
| `reply_turns.csv` | 2,053 | follow-up message counts |
| `engagement_annotations.csv` | 1,042 | five annotated engagement dimensions, single |
| `engagement_annotations_dual.csv` | 1,011 | the same, dual |

**Assistant recommendations and market data**

| File | Rows | Unit |
|---|---|---|
| `ai_only_by_wave.csv` | 1,029 | the single assistant's *own* recommended portfolio |
| `ai_portfolios_dual_extracted.csv` | 1,097 | both assistants' recommended portfolios |
| `daily_returns.csv` | 49 | daily asset returns spanning the evaluation windows |

**Derived frames** (regenerated by `upstream/`, shipped so `run_all.R` needs no
network or API access)

| File | Rows | Produced by |
|---|---|---|
| `counterfactual_regime_m2.csv` | 1,043 | `upstream/counterfactual_regime_m2.py` |
| `counterfactual_windows_arm_m2.csv` | 9,732 | the same; window-level arm means, provided for reanalysis and not read by `run_all.R` |
| `grs_data.csv`, `grs_regime_extremes.csv` | 1,043 | `upstream/grs_regime_extremes.py` |
| `positioning_map_cross_domain_data.csv` | 12 | `positioning_map_cross_domain.R` |

Key columns:

- `participantId`, `wave`, `session_id` — participant, recruitment wave, session
- `ai_group` — assigned assistant orientation (single); `dual_condition`,
  `persona_ai1`, `persona_ai2`, `side_assignment` (dual)
- `pre_active_m2_ann`, `post_active_m2_ann` — annualised active M² of the
  participant's initial and revised portfolio, scored on their own window
- `pre_w_*`, `post_w_*`, `ai_w_*`, `ai1_w_*`, `ai2_w_*` — portfolio weights
- `risk_pref_score` — the 0–1 risk-preference composite used to assign dual
  pairs relative to each participant (see the SI for its construction)
- `pre_conf`, `post_conf`, `perceived_improve`, `perf_exp` — perception measures
- `behavioral`, `cognitive`, `emotional`, `autonomy`, `social_presence` —
  annotated engagement dimensions (0–3)

The raw survey exports, conversation transcripts, and the CloudResearch
assignment files are not included: they carry platform identifiers and verbatim
participant text. They are available from the authors.

## What each script produces

**Sample description and randomisation checks**

| Script | Output |
|---|---|
| `descriptives_table_investment.R` | `descriptives_table_investment.csv` — continuous and binary measures by study |
| `balance_check_investment.R` | `balance_investment_{single,dual}.csv` — covariate balance, pooled biased vs default (ASMD / Cramér's V) |

**Single assistant: performance**

| Script | Output |
|---|---|
| `plot_active_m2_treatment.R` | `active_m2_treatment_effect{,_by_wave}.png` — the five-arm performance ladder |
| `plot_grs_treatment.R` | `grs_treatment_by_regime.png` — ex-ante mean-variance efficiency under three pre-study frontiers |
| `wave_favored_ai_type.R` | `wave_favored_ai_type.png` — which orientation each wave's market rewarded |
| `plot_active_m2_priming.R` | `active_m2_priming_effect.png` — the label-only manipulation |

**Bias magnitude and direction**

| Script | Output |
|---|---|
| `bias_magnitude_outcomes.R` | `bias_magnitude_outcomes.png` — performance, follow-up participation, confidence across default/moderate/strong, split by direction |
| `ai_advice_by_bias_magnitude.R` | `ai_advice_by_bias_magnitude{,_split}.png`, `ai_advice_distribution_by_arm.png` — the assistant's own portfolio quality |
| `bias_side_performance.R` | `bias_side_performance{,_by_wave}.png` — echo chamber vs opposition |
| `bias_side_grs.R` | `bias_side_grs*.png` — the same on ex-ante efficiency |
| `persuasion_backfire_performance.R` | four-cell persuasion / backfire decomposition (console) |

**Counterfactual market regimes**

| Script | Output |
|---|---|
| `counterfactual_bull_m2.R` | `counterfactual_m2_by_regime.png` and two companions — the same portfolios re-scored on bear/neutral/bull windows |
| `active_m2_treatment_bull_overlay.R` | `active_m2_treatment_bull_overlay.png` — the main-text overlay for the risk-seeking arms |
| `bias_side_counterfactual.R` | `bias_side_real_vs_bull.png` — echo/opposition under both regimes |

**Portfolio structure and diversity**

| Script | Output |
|---|---|
| `portfolio_structure.R` | `cross_participant_diversity.png` — dispersion among single-assistant arms |
| `diversity_single_vs_dual.R` | `diversity_single_vs_dual.png` — dispersion across all five conditions, with bootstrap and permutation inference |

**Engagement and perception**

| Script | Output |
|---|---|
| `engagement_by_arm.R` | `engagement_reply_turns.png`, `engagement_radar.png` |
| `engagement_coef_heatmap_investment.R` | `engagement_coef_heatmap_investment{,_rawp}.png` — five conditions × five dimensions |
| `reply_turns_single_vs_dual.R` | `followup_participation_single_vs_dual.png` |
| `perception_outcomes.R` | confidence and perceived-improvement figures (ordinal `MCMCglmm`) |
| `perceived_ai_role.R` | `perceived_ai_role.png` — tool vs influencing agent by bias magnitude |

**Dual assistants**

| Script | Output |
|---|---|
| `active_m2_single_vs_dual.R` | `active_m2_single_vs_dual_*.png` — the five-condition comparison |
| `persuasion_backfire_dual.R` | six-cell persuasion / backfire / advisor-selection decomposition (console) |
| `advisor_selection_dual.R` | whether participants move toward the better advisor when the two disagree (console) |

**Extended Data**

| Script | Output |
|---|---|
| `neutral_arm_analysis.R` | `neutral_arm_*.png` — the explicitly neutralized assistant on performance, diversity, participation and confidence |
| `positioning_map_cross_domain.R` | `positioning_map_cross_domain.png` — the investment arms against the fact-checking arms on a shared plane |

### Upstream steps

`code/upstream/` holds the Python that produced several inputs. These are **not**
run by `run_all.R`: two of them call paid LLM APIs, two download market history,
and all of them read the raw exports that are not released here. They are
included so the derivation of every shipped file is inspectable.

| Script | Produces |
|---|---|
| `ai_portfolio_extraction{,_dual,_priming}.py` | the assistants' recommended portfolios, extracted from transcripts with GPT-4o |
| `engagement_annotation_investment.py`, `engagement_annotation_dual.py` | the five engagement dimensions, annotated with a fixed rubric |
| `counterfactual_regime_m2.py` | the regime counterfactual frames |
| `grs_regime_extremes.py` | the GRS frontiers |
| `compute_active_m2_bonus.py` | the performance bonus actually paid |
| `figure_s1_investment_descriptives.py`, `figure_balance_investment.py` | two figures drawn in matplotlib rather than ggplot |

All read their API keys from the environment (`OPENAI_API_KEY`) and contain no
credentials.

## Checking your run

A successful run reproduces, among others:

- `plot_active_m2_treatment.R` — extremely risk-averse vs default, +0.825
  percentage points of annualised active M² (HC3 SE 0.163)
- `ai_advice_by_bias_magnitude.R` — the assistant's own portfolio quality by
  magnitude: −3.140% (no bias), −2.390% (moderate), −2.102% (strong)
- `diversity_single_vs_dual.R` — change in cross-participant dispersion:
  −0.175 for the single default arm, +0.013 for single biased
- `counterfactual_bull_m2.R` — under bull windows the risk-seeking arms exceed
  the default by 1.16 and 1.02 percentage points

Permutation and bootstrap results move in the last reported digit between runs
where no seed is set. `MCMCglmm` models are stochastic: seeds are set where the
original analysis set them, but posterior means on the latent scale still move
by roughly 0.07–0.18 between runs, so the ordinal coefficients will not
reproduce to three decimals.
