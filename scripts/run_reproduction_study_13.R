# scripts/run_reproduction_study_13.R
source("R/reproduce/00_reproduction_helpers.R")
source("R/reproduce/study_13.R")

study_13_results <- reproduce_study_13()
print(study_13_results)
message("Study 13 recomputation complete: outputs/reproduced/study_13_recomputed.csv")
