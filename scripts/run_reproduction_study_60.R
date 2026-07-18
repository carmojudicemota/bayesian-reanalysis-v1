#!/usr/bin/env Rscript

# Run from the repository root:
# source("scripts/run_reproduction_study_60.R")

required_packages <- c("haven", "car", "readr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
]
if (length(missing_packages) > 0L) {
  stop(
    "Install the required packages before running Study 60: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

source("R/reproduce/study_60.R")

sav_path <- file.path(
  "data", "raw", "study_60",
  "Syllabus_Safety_Cues_Instructor_Gender_Data_Sp21_OSF.sav"
)
out_dir <- file.path("outputs", "reproduced")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

reproduction <- reproduce_study_60(sav_path)

readr::write_csv(
  reproduction$results,
  file.path(out_dir, "study_60_recomputed.csv"),
  na = ""
)
readr::write_csv(
  reproduction$audit,
  file.path(out_dir, "study_60_recomputation_audit.csv"),
  na = ""
)

message("Study 60 reproduction completed.")
message("Main output: outputs/reproduced/study_60_recomputed.csv")
message("Audit output: outputs/reproduced/study_60_recomputation_audit.csv")
