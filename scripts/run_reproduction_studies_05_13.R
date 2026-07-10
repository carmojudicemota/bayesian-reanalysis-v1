# scripts/run_reproduction_studies_05_13.R
source("R/reproduce/00_reproduction_helpers.R")
source("R/reproduce/study_05.R")
source("R/reproduce/study_13.R")

study_05_results <- reproduce_study_05()
study_13_results <- reproduce_study_13()
all_recomputed_results <- dplyr::bind_rows(study_05_results, study_13_results)
readr::write_csv(all_recomputed_results, "outputs/reproduced/recomputed_results_studies_05_13.csv")
print(all_recomputed_results)
message("Studies 05 and 13 recomputation complete.")
