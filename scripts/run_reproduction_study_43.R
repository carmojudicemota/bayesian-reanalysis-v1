if (file.exists("R/reproduce/00_reproduction_helpers.R")) {
  source("R/reproduce/00_reproduction_helpers.R", local = TRUE)
}

source("R/reproduce/study_43.R", local = TRUE)

reproduce_study_43()
