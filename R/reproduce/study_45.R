reproduce_study_45 <- function(
  input_path = "data/raw/study_45/Untitled3.sav",
  output_path = "outputs/reproduced/study_45_recomputed.csv"
) {
  required_packages <- c("haven", "readr", "tibble")
  missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing_packages) > 0) {
    stop(
      "Install required package(s) before running this script: ",
      paste(missing_packages, collapse = ", "),
      call. = FALSE
    )
  }

  if (!file.exists(input_path)) {
    stop("Raw data file not found: ", input_path, call. = FALSE)
  }

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

  raw_data <- haven::read_sav(input_path)

  friedman_variables <- c(
    "AVG_Aware",
    "AVG_Dev",
    "AVG_Explain",
    "AVG_Import"
  )

  missing_variables <- setdiff(friedman_variables, names(raw_data))
  if (length(missing_variables) > 0) {
    stop(
      "The following variables are missing from the raw data: ",
      paste(missing_variables, collapse = ", "),
      call. = FALSE
    )
  }

  analysis_data <- raw_data[, friedman_variables]
  analysis_data <- analysis_data[stats::complete.cases(analysis_data), ]

  n_total <- nrow(analysis_data)
  number_of_conditions <- length(friedman_variables)

  if (n_total == 0) {
    stop("No complete cases available for the Friedman test.", call. = FALSE)
  }

  # This mirrors the original SPSS syntax:
  # NPAR TESTS
  #   /FRIEDMAN = AVG_Aware AVG_Dev AVG_Explain AVG_Import
  #   /MISSING LISTWISE.
  friedman_result <- stats::friedman.test(as.matrix(analysis_data))

  chi_square_value <- unname(friedman_result$statistic)
  chi_square_df <- unname(friedman_result$parameter)
  p_value <- unname(friedman_result$p.value)

  # Kendall's W for a Friedman test is chi-square divided by n * (k - 1).
  kendalls_w <- chi_square_value / (n_total * (number_of_conditions - 1))

  dimension_means <- vapply(analysis_data, mean, numeric(1), na.rm = TRUE)
  dimension_sds <- vapply(analysis_data, stats::sd, numeric(1), na.rm = TRUE)
  dimension_medians <- vapply(analysis_data, stats::median, numeric(1), na.rm = TRUE)

  extraction_note <- paste0(
    "Recomputed from the original author dataset using complete cases on AVG_Aware, AVG_Dev, AVG_Explain, and AVG_Import. ",
    "This mirrors the uploaded SPSS syntax NPAR TESTS /FRIEDMAN=AVG_Aware AVG_Dev AVG_Explain AVG_Import /MISSING LISTWISE. ",
    "Complete-case n = ", n_total, "; k = ", number_of_conditions, "; ",
    "means: AVG_Aware = ", round(dimension_means[["AVG_Aware"]], 6),
    ", AVG_Dev = ", round(dimension_means[["AVG_Dev"]], 6),
    ", AVG_Explain = ", round(dimension_means[["AVG_Explain"]], 6),
    ", AVG_Import = ", round(dimension_means[["AVG_Import"]], 6), ". ",
    "SDs: AVG_Aware = ", round(dimension_sds[["AVG_Aware"]], 6),
    ", AVG_Dev = ", round(dimension_sds[["AVG_Dev"]], 6),
    ", AVG_Explain = ", round(dimension_sds[["AVG_Explain"]], 6),
    ", AVG_Import = ", round(dimension_sds[["AVG_Import"]], 6), ". ",
    "Medians: AVG_Aware = ", round(dimension_medians[["AVG_Aware"]], 6),
    ", AVG_Dev = ", round(dimension_medians[["AVG_Dev"]], 6),
    ", AVG_Explain = ", round(dimension_medians[["AVG_Explain"]], 6),
    ", AVG_Import = ", round(dimension_medians[["AVG_Import"]], 6), ". ",
    "Friedman chi-square = ", round(chi_square_value, 6),
    ", df = ", chi_square_df,
    ", p = ", signif(p_value, 6),
    ", Kendall's W = ", round(kendalls_w, 6), "."
  )

  results <- tibble::tibble(
    id = 34,
    study_id = "study_45",
    study_DOI = "10.1177/00986283241247181",
    recomputation_status = "recomputed_from_original_author_friedman_test",
    stat_test = "friedman_test",
    reported_result = "chi-square(3) = 1321.10, p < .001, W = 0.81",
    p_value = p_value,
    p_operator = "<",
    p_sidedness = "omnibus",
    t_value = NA_real_,
    t_df = NA_real_,
    f_value = NA_real_,
    f_df1 = NA_real_,
    f_df2 = NA_real_,
    z_value = NA_real_,
    chi2_value = chi_square_value,
    chi2_df = chi_square_df,
    r_value = NA_real_,
    n1 = NA_real_,
    n2 = NA_real_,
    n_total = n_total,
    n_eff = n_total,
    effect_size_type = "kendalls_w",
    effect_size_value = kendalls_w,
    estimate = NA_real_,
    se_estimate = NA_real_,
    raw_data_file = input_path,
    raw_variable_names = paste(friedman_variables, collapse = "; "),
    model_formula = "SPSS NPAR TESTS /FRIEDMAN=AVG_Aware AVG_Dev AVG_Explain AVG_Import /MISSING LISTWISE",
    contrast_direction = "overall difference among psychological-literacy dimensions: awareness, development, confidence/explain, and importance",
    extraction_note = extraction_note
  )

  readr::write_csv(results, output_path)

  message("Wrote ", output_path)
  invisible(results)
}
