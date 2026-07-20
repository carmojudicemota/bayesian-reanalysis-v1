normalise_study_id <- function(x) {
  number <- suppressWarnings(
    as.integer(sub(".*?([0-9]+).*", "\\1", x))
  )
  
  ifelse(
    is.na(number),
    x,
    sprintf("study_%02d", number)
  )
}


build_verified_results_draft <- function(
    input_path =
      "outputs/reproduced/all_recomputed_results_current_studies.csv",
    output_path =
      "data/derived/verified_results_draft.csv"
) {
  results <- readr::read_csv(
    input_path,
    show_col_types = FALSE,
    na = c("", "NA")
  )
  
  required_columns <- c(
    "source_file",
    "id",
    "study_id",
    "study_DOI",
    "recomputation_status",
    "stat_test",
    "reported_result",
    "p_value"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(results)
  )
  
  if (length(missing_columns) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  
  if (anyDuplicated(results$id)) {
    stop("The source id column is not unique.")
  }
  
  verified <- results |>
    dplyr::mutate(
      source_row_id = id,
      source_study_id = study_id,
      study_id = normalise_study_id(study_id)
    ) |>
    dplyr::arrange(study_id, source_row_id) |>
    dplyr::group_by(study_id) |>
    dplyr::mutate(
      result_number = dplyr::row_number(),
      result_id = sprintf(
        "%s_result_%02d",
        study_id,
        result_number
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::relocate(
      result_id,
      source_row_id,
      source_study_id,
      .before = source_file
    ) |>
    dplyr::select(
      -id,
      -result_number
    )
  
  if (anyDuplicated(verified$result_id)) {
    stop("Generated result_id values are not unique.")
  }
  
  dir.create(
    dirname(output_path),
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  readr::write_csv(
    verified,
    output_path,
    na = ""
  )
  
  message(
    "Created ",
    output_path,
    " with ",
    nrow(verified),
    " results from ",
    dplyr::n_distinct(verified$study_id),
    " studies."
  )
  
  invisible(verified)
}

# NOTE: no build call at file scope. Sourcing this file must only define
# functions; scripts/00_run_all.R calls build_verified_results_draft() explicitly.
