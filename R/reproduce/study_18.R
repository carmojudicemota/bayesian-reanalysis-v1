# Study 18 transparent reconstruction
# Pownall et al. (2022), DOI: 10.1177/00986283221130298
# Target row: id 12
# Reconstructs the one-way MANOVA comparing Psychology, non-Psychology STEM,
# and Humanities/non-STEM students across eight psychological literacy attributes.

reproduce_study_18 <- function(
    input_path = "data/raw/study_18/Psychological_literacy_subject_study_finaldataset.sav",
    output_path = "outputs/reproduced/study_18_recomputed.csv"
) {
  required_packages <- c("haven", "dplyr", "tibble", "readr")
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

  skill_item_groups <- list(
    Skill_1_avg_recomputed = c("Skill_1_Awareness", "Skill_1_Development", "Skill_1_Confidence", "Skill_1_Importance"),
    Skill_2_avg_recomputed = c("Skill_2_Awareness", "Skill_2_Development", "Skill_2_Confidence", "Skill_2_Importance"),
    Skill_3_avg_recomputed = c("Skill_3_Awareness", "Skill_3_Development", "Skill_3_Confidence", "Skill_3_Importance"),
    Skill_4_avg_recomputed = c("Skill_4_Awareness", "Skill_4_Development", "Skill_4_Confidence", "Skill_4_Importance"),
    Skill_5_avg_recomputed = c("Skill_5_Awareness", "Skill_5_Development", "Skill_5_Confidence", "Skill_5_Importance"),
    Skill_6_avg_recomputed = c("Skill_6_Awareness", "Skill_6_Development", "Skill_6_Confidence", "Skill_6_Importance"),
    Skill_7_avg_recomputed = c("Skill_7_Awareness", "Skill_7_Development", "Skill_7_Confidence", "Skill_7_Importance"),
    Skill_8_avg_recomputed = c("Skill_8_Awareness", "Skill_8_Development", "Skill_8_Confidence", "Skill_8_Importance")
  )

  required_columns <- c("STEM_NONSTEM_PSYCH", unlist(skill_item_groups))
  missing_columns <- setdiff(required_columns, names(raw_data))

  if (length(missing_columns) > 0) {
    stop(
      "The following required columns are missing from the raw data: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  analysis_data <- raw_data

  for (new_column in names(skill_item_groups)) {
    item_columns <- skill_item_groups[[new_column]]
    analysis_data[[new_column]] <- rowMeans(
      as.data.frame(analysis_data[, item_columns]),
      na.rm = FALSE
    )
  }

  outcome_columns <- names(skill_item_groups)

  analysis_data <- analysis_data |>
    dplyr::filter(
      !is.na(STEM_NONSTEM_PSYCH),
      dplyr::if_all(dplyr::all_of(outcome_columns), ~ !is.na(.x))
    ) |>
    dplyr::mutate(
      subject_group = factor(
        STEM_NONSTEM_PSYCH,
        levels = c(1, 2, 3),
        labels = c("Psychology", "non-Psychology STEM", "Humanities/non-STEM")
      )
    )

  if (nrow(analysis_data) == 0) {
    stop("No complete cases remain for the MANOVA after filtering.", call. = FALSE)
  }

  manova_formula <- stats::as.formula(
    paste0(
      "cbind(",
      paste(outcome_columns, collapse = ", "),
      ") ~ subject_group"
    )
  )

  manova_fit <- stats::manova(manova_formula, data = analysis_data)
  pillai_table <- as.data.frame(summary(manova_fit, test = "Pillai")$stats)
  pillai_table$term <- trimws(rownames(pillai_table))

  subject_group_row <- pillai_table |>
    dplyr::filter(term == "subject_group")

  if (nrow(subject_group_row) != 1) {
    stop("Could not extract the subject_group MANOVA row.", call. = FALSE)
  }

  f_value <- unname(subject_group_row[["approx F"]])
  f_df1 <- unname(subject_group_row[["num Df"]])
  f_df2 <- unname(subject_group_row[["den Df"]])
  p_value <- unname(subject_group_row[["Pr(>F)"]])
  pillai_trace <- unname(subject_group_row[["Pillai"]])
  eta_p2 <- (f_value * f_df1) / ((f_value * f_df1) + f_df2)

  group_sizes <- analysis_data |>
    dplyr::count(subject_group, name = "n") |>
    dplyr::mutate(group_n = paste0(subject_group, " n=", n)) |>
    dplyr::pull(group_n) |>
    paste(collapse = "; ")

  results <- tibble::tibble(
    id = 12,
    study_id = "study_18",
    study_DOI = "10.1177/00986283221130298",
    recomputation_status = "recomputed_from_raw_data",

    stat_test = "manova",
    reported_result = "F(16, 574) = 36.66, p < .001, Pillai's Trace = 1.011, eta_p2 = .505",

    p_value = p_value,
    p_operator = ifelse(p_value < .001, "<", "="),
    p_sidedness = "omnibus",

    t_value = NA_real_,
    t_df = NA_real_,
    f_value = f_value,
    f_df1 = f_df1,
    f_df2 = f_df2,
    z_value = NA_real_,
    chi2_value = NA_real_,
    chi2_df = NA_real_,
    r_value = NA_real_,

    n1 = NA_real_,
    n2 = NA_real_,
    n_total = nrow(analysis_data),
    n_eff = NA_real_,

    effect_size_type = "eta_p2",
    effect_size_value = eta_p2,

    estimate = NA_real_,
    se_estimate = NA_real_,

    raw_data_file = input_path,
    raw_variable_names = paste(c("STEM_NONSTEM_PSYCH", unlist(skill_item_groups)), collapse = "; "),
    model_formula = paste(deparse(manova_formula), collapse = " "),
    contrast_direction = "Subject-group effect across eight psychological literacy attribute averages: Psychology vs non-Psychology STEM vs Humanities/non-STEM",
    extraction_note = paste0(
      "One-way MANOVA with Pillai trace, matching the article's decision to report Pillai's Trace. ",
      "Skill averages were recomputed from Awareness, Development, Confidence, and Importance item columns. ",
      "Complete-case n = ", nrow(analysis_data), "; ", group_sizes,
      ". Pillai trace = ", round(pillai_trace, 6), "."
    ),
    pillai_trace = pillai_trace
  )

  readr::write_csv(results, output_path)
  return(results)
}
