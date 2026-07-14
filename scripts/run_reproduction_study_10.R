# scripts/run_reproduction_study_10.R
source("R/reproduce/00_reproduction_helpers.R")
source("R/reproduce/study_10.R")

study_10_results <- reproduce_study_10()
print(study_10_results)
message("Study 10 recomputation complete: outputs/reproduced/study_10_recomputed.csv")
