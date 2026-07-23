# scripts/run_reproduction_study_14.R
source("R/reproduce/00_reproduction_helpers.R")
source("R/reproduce/study_14.R")

study_14_results <- reproduce_study_14()
print(study_14_results)
message("Study 14 recomputation complete: outputs/reproduced/study_14_recomputed.csv")
