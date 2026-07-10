# Transparent reproduction layer: studies 05 and 13

This package replaces the earlier opaque reproduction scripts with transparent scripts that show the actual statistical tests.

## Files

```text
R/reproduce/study_05.R
R/reproduce/study_13.R
scripts/run_reproduction_studies_05_13.R
metadata/study_05_reproduction_notes.md
metadata/study_13_reproduction_notes.md
data/raw/study_05/README.md
data/raw/study_13/README.md
```

## Raw data expected locally

```text
data/raw/study_05/HardLovettBrady_Data_Shared.csv
data/raw/study_13/DATA_Cleaned_and_coded_for_condition.sav
```

Raw data are not included in this ZIP.

## Run

From the project root:

```r
source("scripts/run_reproduction_studies_05_13.R")
```

The script writes:

```text
outputs/reproduced/study_05_recomputed.csv
outputs/reproduced/study_13_recomputed.csv
outputs/reproduced/recomputed_results_studies_05_13.csv
```

## Principle

The manual extraction file is not overwritten. These scripts generate a separate recomputation layer.
