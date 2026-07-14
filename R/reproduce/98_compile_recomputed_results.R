library(readr)
library(dplyr)
library(purrr)
library(stringr)

dir.create("outputs/reproduced", recursive = TRUE, showWarnings = FALSE)

standard_columns <- c(
  "id",
  "study_id",
  "study_DOI",
  "recomputation_status",
  "stat_test",
  "reported_result",
  "p_value",
  "p_operator",
  "p_sidedness",
  "t_value",
  "t_df",
  "f_value",
  "f_df1",
  "f_df2",
  "z_value",
  "chi2_value",
  "chi2_df",
  "r_value",
  "n1",
  "n2",
  "n_total",
  "n_eff",
  "effect_size_type",
  "effect_size_value",
  "estimate",
  "se_estimate",
  "raw_data_file",
  "raw_variable_names",
  "model_formula",
  "contrast_direction",
  "extraction_note"
)

numeric_columns <- c(
  "id",
  "p_value",
  "t_value",
  "t_df",
  "f_value",
  "f_df1",
  "f_df2",
  "z_value",
  "chi2_value",
  "chi2_df",
  "r_value",
  "n1",
  "n2",
  "n_total",
  "n_eff",
  "effect_size_value",
  "estimate",
  "se_estimate"
)

files <- list.files(
  path = "outputs/reproduced",
  pattern = "^study_[0-9]+_recomputed\\.csv$",
  full.names = TRUE
)

if (length(files) == 0) {
  stop("No individual study recomputed CSV files found in outputs/reproduced/.")
}

read_one_recomputed_file <- function(path) {
  dat <- readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  )
  
  missing_columns <- setdiff(standard_columns, names(dat))
  
  for (col in missing_columns) {
    dat[[col]] <- NA_character_
  }
  
  dat |>
    mutate(source_file = basename(path)) |>
    select(source_file, all_of(standard_columns))
}

compiled <- purrr::map_dfr(files, read_one_recomputed_file)

compiled <- compiled |>
  mutate(
    across(
      all_of(numeric_columns),
      ~ suppressWarnings(as.numeric(.x))
    )
  ) |>
  arrange(id, study_id)

duplicate_ids <- compiled |>
  filter(!is.na(id)) |>
  count(id) |>
  filter(n > 1)

if (nrow(duplicate_ids) > 0) {
  print(duplicate_ids)
  stop("Duplicate result ids found. Fix duplicated ids before compiling.")
}

missing_ids <- compiled |>
  filter(is.na(id))

if (nrow(missing_ids) > 0) {
  print(missing_ids)
  stop("Some rows have missing id values. Fix them before compiling.")
}

readr::write_csv(
  compiled,
  "outputs/reproduced/all_recomputed_results_current_studies.csv"
)

summary_table <- compiled |>
  count(study_id, recomputation_status, stat_test, name = "n_results") |>
  arrange(study_id)

readr::write_csv(
  summary_table,
  "outputs/reproduced/all_recomputed_results_summary.csv"
)

message("Compiled ", nrow(compiled), " recomputed result rows from ", length(files), " study files.")
message("Written to outputs/reproduced/all_recomputed_results_current_studies.csv")