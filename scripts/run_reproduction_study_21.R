# scripts/run_reproduction_study_21.R
source("R/reproduce/00_reproduction_helpers.R")
source("R/reproduce/study_21.R")

study_21_results <- reproduce_study_21()
print(study_21_results)
message("Study 21 recomputation complete: outputs/reproduced/study_21_recomputed.csv")
