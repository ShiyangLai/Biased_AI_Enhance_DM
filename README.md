# Biased AI Enhances Human Decision-Making But Reduces Trust

Replication materials for a set of experiments on how the stance of an LLM
assistant shapes human judgement — whether stance-diverse advice improves
accuracy, and what it costs in engagement and trust.

## Experiments

| Folder | Experiment | Status |
|---|---|---|
| [`fact-checking/`](fact-checking) | Judging the veracity of news headlines with one or two politically steered assistants (*n* = 993 and *n* = 1,499) | available |
| [`investment/`](investment) | Building a portfolio with one or two assistants steered toward risk aversion or risk seeking, scored on realised two-week market performance (*n* = 1,344 and *n* = 1,011) | available |

Further experiments will be added as additional top-level folders, each
self-contained with its own `data/`, `code/`, and `README.md`.

## Getting started

Each experiment folder is independent. For the fact-checking study:

```r
setwd("fact-checking/code")
source("run_all.R")
```

For the investment study:

```r
setwd("investment/code")
source("run_all.R")
```

See each folder's `README.md` for data documentation, requirements, and a
script-by-script guide.

The two studies differ in how their code is organised. The fact-checking
scripts share a global environment and must run in the order given in
`run_all.R`. The investment scripts are independent: each reads the prepared
CSVs in `investment/data` and can be run on its own after `_setup.R`. One
investment script, `positioning_map_cross_domain.R`, reads fact-checking data to
place the two studies on a common plane; it is the only cross-folder dependency.

## Data

Participant data are de-identified: identifiers are replaced with sequential
integers, and free-text responses and platform identifiers are removed. Raw
survey exports and conversation transcripts are available from the authors.
