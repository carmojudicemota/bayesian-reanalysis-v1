# scripts/run_reproduction_study_05.R
source("R/reproduce/00_reproduction_helpers.R")
source("R/reproduce/study_05.R")

study_05_results <- reproduce_study_05()
print(study_05_results)
message("Study 05 recomputation complete: outputs/reproduced/study_05_recomputed.csv")
