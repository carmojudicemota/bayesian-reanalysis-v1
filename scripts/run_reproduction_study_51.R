# scripts/run_reproduction_study_51.R
source("R/reproduce/00_reproduction_helpers.R")
source("R/reproduce/study_51.R")

study_51_results <- reproduce_study_51()
print(study_51_results)
message("Study 51 recomputation complete: outputs/reproduced/study_51_recomputed.csv")
