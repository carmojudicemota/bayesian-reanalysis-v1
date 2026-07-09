# scripts/run_reproduction_studies_05_13.R
# Run transparent recomputation scripts for study_5 and study_13.
# This script generates outputs/reproduced/*.csv from raw data.

required_packages <- c("readr", "dplyr", "tibble", "haven")

missing_packages <- required_packages[!vapply(
  required_packages,
  requireNamespace,
  quietly = TRUE,
  FUN.VALUE = logical(1)
)]

if (length(missing_packages) > 0) {
  stop(
    "Install missing packages before running reproduction: ",
    paste(missing_packages, collapse = ", "),
    "\nUse install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))"
  )
}

source("R/reproduce/study_05.R")
source("R/reproduce/study_13.R")

study_05_results <- reproduce_study_05()
study_13_results <- reproduce_study_13()

all_recomputed_results <- dplyr::bind_rows(
  study_05_results,
  study_13_results
)

dir.create("outputs/reproduced", recursive = TRUE, showWarnings = FALSE)

readr::write_csv(
  all_recomputed_results,
  "outputs/reproduced/recomputed_results_studies_05_13.csv"
)

message("Reproduction complete for studies 05 and 13.")
message("Wrote: outputs/reproduced/study_05_recomputed.csv")
message("Wrote: outputs/reproduced/study_13_recomputed.csv")
message("Wrote: outputs/reproduced/recomputed_results_studies_05_13.csv")

all_recomputed_results
