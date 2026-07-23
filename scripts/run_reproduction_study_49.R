# scripts/run_reproduction_study_49.R
source("R/reproduce/00_reproduction_helpers.R")
source("R/reproduce/study_49.R")

study_49_results <- reproduce_study_49()
print(study_49_results)
message("Study 49 recomputation complete: outputs/reproduced/study_49_recomputed.csv")
