# scripts/run_reproduction_study_33.R
source("R/reproduce/00_reproduction_helpers.R")
source("R/reproduce/study_33.R")

study_33_results <- reproduce_study_33()
print(study_33_results)
message("Study 33 recomputation complete: outputs/reproduced/study_33_recomputed.csv")
