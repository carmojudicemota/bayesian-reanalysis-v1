# scripts/run_reproduction_study_30.R
source("R/reproduce/00_reproduction_helpers.R")
source("R/reproduce/study_30.R")

study_30_results <- reproduce_study_30()
print(study_30_results)
message("Study 30 recomputation complete: outputs/reproduced/study_30_recomputed.csv")
