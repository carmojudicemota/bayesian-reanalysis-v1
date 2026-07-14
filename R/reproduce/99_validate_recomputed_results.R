# R/reproduce/99_validate_recomputed_results.R
# Validates the recomputed output files after scripts have been run.

source("R/reproduce/00_reproduction_helpers.R")
check_required_packages(c("readr", "dplyr", "purrr", "tibble"))

files <- list.files("outputs/reproduced", pattern = "_recomputed\\.csv$", full.names = TRUE)
if (length(files) == 0) stop("No recomputed CSV files found in outputs/reproduced.", call. = FALSE)

validate_one <- function(path) {
  dat <- readr::read_csv(path, show_col_types = FALSE)
  missing_columns <- setdiff(recomputed_result_columns, names(dat))
  dat <- standardise_recomputed_output(dat)
  dat$source_file <- basename(path)
  dat$missing_schema_columns <- paste(missing_columns, collapse = "; ")
  dat$has_p_value <- !is.na(dat$p_value)
  dat$has_test_statistic <- !is.na(dat$t_value) | !is.na(dat$f_value) | !is.na(dat$z_value) | !is.na(dat$chi2_value) | !is.na(dat$r_value)
  dat$has_sample_information <- !is.na(dat$n_total) | (!is.na(dat$n1) & !is.na(dat$n2)) | !is.na(dat$n_eff)
  dat
}

validation_report <- purrr::map_dfr(files, validate_one)
readr::write_csv(validation_report, "outputs/reproduced/recomputed_validation_report.csv")
validation_summary <- validation_report |>
  dplyr::count(source_file, recomputation_status, bayesian_input_status)
readr::write_csv(validation_summary, "outputs/reproduced/recomputed_validation_summary.csv")
print(validation_summary)
