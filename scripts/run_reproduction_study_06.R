# scripts/run_reproduction_study_06.R
source("R/reproduce/00_reproduction_helpers.R")
source("R/reproduce/study_06.R")

study_06_results <- reproduce_study_06()
print(study_06_results)
message("Study 06 recomputation complete: outputs/reproduced/study_06_recomputed.csv")
