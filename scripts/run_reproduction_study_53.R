# scripts/run_reproduction_study_53.R
source("R/reproduce/00_reproduction_helpers.R")
source("R/reproduce/study_53.R")

study_53_results <- reproduce_study_53()
print(study_53_results)
message("Study 53 recomputation complete: outputs/reproduced/study_53_recomputed.csv")
