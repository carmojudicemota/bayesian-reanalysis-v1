# scripts/run_reproduction_study_20.R
source("R/reproduce/00_reproduction_helpers.R")
source("R/reproduce/study_20.R")

study_20_results <- reproduce_study_20()
print(study_20_results)
message("Study 20 recomputation complete: outputs/reproduced/study_20_recomputed.csv")
