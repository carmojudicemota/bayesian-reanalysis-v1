# scripts/run_reproduction_study_41.R
source("R/reproduce/00_reproduction_helpers.R")
source("R/reproduce/study_41.R")

study_41_results <- reproduce_study_41()
print(study_41_results)
message("Study 41 recomputation complete: outputs/reproduced/study_41_recomputed.csv")
