# Biased AI Enhances Human Decision-Making But Reduces Trust

Replication materials for a set of experiments on how the political stance of an
LLM assistant shapes human judgement — whether stance-diverse advice improves
accuracy, and what it costs in engagement and trust.

## Experiments

| Folder | Experiment | Status |
|---|---|---|
| [`fact-checking/`](fact-checking) | Judging the veracity of news headlines with one or two politically steered assistants (*n* = 993 and *n* = 1,499) | available |

Further experiments will be added as additional top-level folders, each
self-contained with its own `data/`, `code/`, and `README.md`.

## Getting started

Each experiment folder is independent. For the fact-checking study:

```r
setwd("fact-checking/code")
source("run_all.R")
```

See [`fact-checking/README.md`](fact-checking/README.md) for data documentation,
requirements, and a script-by-script guide.

## Data

Participant data are de-identified: identifiers are replaced with sequential
integers, and free-text responses and platform identifiers are removed.
