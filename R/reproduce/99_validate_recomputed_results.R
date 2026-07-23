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

  checkable <- !is.na(dat$t_value) & !is.na(dat$t_df) & !is.na(dat$p_value) &
    !is.na(dat$p_sidedness) & dat$p_sidedness %in% c("one_sided", "two_sided")
  p_two <- 2 * stats::pt(-abs(dat$t_value), dat$t_df)
  dat$p_expected_from_t <- ifelse(dat$p_sidedness == "one_sided", p_two / 2, p_two)
  dat$p_matches_sidedness <- ifelse(
    checkable,
    abs(dat$p_value - dat$p_expected_from_t) <= pmax(1e-9, 1e-4 * dat$p_expected_from_t),
    NA
  )
  dat
}

validation_report <- purrr::map_dfr(files, validate_one)
readr::write_csv(validation_report, "outputs/reproduced/recomputed_validation_report.csv")
validation_summary <- validation_report |>
  dplyr::count(source_file, recomputation_status, bayesian_input_status)
readr::write_csv(validation_summary, "outputs/reproduced/recomputed_validation_summary.csv")
print(validation_summary)

# Fail fast on any directionality inconsistency, so a bad p never reaches claims.
bad_sidedness <- validation_report |>
  dplyr::filter(!is.na(p_matches_sidedness) & !p_matches_sidedness) |>
  dplyr::select(source_file, id, study_id, p_sidedness, p_value, p_expected_from_t,
                t_value, t_df)
if (nrow(bad_sidedness) > 0) {
  print(as.data.frame(bad_sidedness))
  stop(
    nrow(bad_sidedness), " recomputed row(s) store a p-value that does not match ",
    "their p_sidedness label. Fix the reproduction script so the stored p equals ",
    "the p the article reports (sidedness follows the original published test).",
    call. = FALSE
  )
}
message("Directionality check passed: every recomputed p matches its p_sidedness label.")
