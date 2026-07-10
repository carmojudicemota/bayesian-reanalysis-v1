# scripts/run_all_reproductions.R
# Runs every current study-specific information-recovery script.

source("R/reproduce/00_reproduction_helpers.R")

study_scripts <- c(
  "R/reproduce/study_05.R",
  "R/reproduce/study_06.R",
  "R/reproduce/study_10.R",
  "R/reproduce/study_13.R",
  "R/reproduce/study_14.R",
  "R/reproduce/study_20.R",
  "R/reproduce/study_21.R",
  "R/reproduce/study_30.R",
  "R/reproduce/study_33.R",
  "R/reproduce/study_41.R",
  "R/reproduce/study_49.R",
  "R/reproduce/study_51.R",
  "R/reproduce/study_53.R"
)

for (script in study_scripts) source(script)

results <- dplyr::bind_rows(
  reproduce_study_05(),
  reproduce_study_06(),
  reproduce_study_10(),
  reproduce_study_13(),
  reproduce_study_14(),
  reproduce_study_20(),
  reproduce_study_21(),
  reproduce_study_30(),
  reproduce_study_33(),
  reproduce_study_41(),
  reproduce_study_49(),
  reproduce_study_51(),
  reproduce_study_53()
)

readr::write_csv(results, "outputs/reproduced/all_recomputed_results_current_studies.csv")
print(results)
message("All recomputation scripts complete: outputs/reproduced/all_recomputed_results_current_studies.csv")
