# R/reproduce/00_reproduction_helpers.R
# Shared output/schema helpers only. Study-specific scripts still do the actual
# raw-data reconstruction and statistical calculations explicitly.

recomputed_result_columns <- c(
  "id", "study_id", "study_DOI", "recomputation_status",
  "stat_test", "reported_result", "reported_p_value", "reported_p_operator",
  "reported_p_sidedness", "reported_effect_size_type", "reported_effect_size_value",
  "p_value", "p_operator", "p_sidedness",
  "t_value", "t_df", "f_value", "f_df1", "f_df2", "z_value",
  "chi2_value", "chi2_df", "r_value",
  "n1", "n2", "n_total", "n_eff",
  "effect_size_type", "effect_size_value",
  "estimate", "se_estimate",
  "raw_data_file", "raw_variable_names", "model_formula",
  "contrast_direction", "analysis_label", "statistic_source",
  "bayesian_input_status", "extraction_note"
)

check_required_packages <- function(packages) {
  missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_packages) > 0) {
    stop(
      "Install required package(s) before running this script: ",
      paste(missing_packages, collapse = ", "),
      "\nUse install.packages(c(",
      paste(sprintf('"%s"', missing_packages), collapse = ", "),
      "))",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

check_required_columns <- function(dat, required_columns, study_id) {
  missing_columns <- setdiff(required_columns, names(dat))
  if (length(missing_columns) > 0) {
    stop(
      study_id, " is missing required column(s): ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

resolve_existing_file <- function(candidate_paths, label = "raw data file") {
  candidate_paths <- unique(candidate_paths[!is.na(candidate_paths) & nzchar(candidate_paths)])
  existing <- candidate_paths[file.exists(candidate_paths)]
  if (length(existing) > 0) return(existing[[1]])
  stop(
    "Could not find ", label, ". Tried:\n  ",
    paste(candidate_paths, collapse = "\n  "),
    call. = FALSE
  )
}

standardise_recomputed_output <- function(dat) {
  for (col in setdiff(recomputed_result_columns, names(dat))) {
    dat[[col]] <- NA
  }
  dat[, c(recomputed_result_columns, setdiff(names(dat), recomputed_result_columns)), drop = FALSE]
}

write_recomputed_results <- function(rows, output_path) {
  rows <- standardise_recomputed_output(rows)
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(rows, output_path)
  rows
}

make_recomputed_row <- function(
    id, study_id, study_DOI, recomputation_status,
    stat_test, reported_result,
    reported_p_value = NA_real_, reported_p_operator = NA_character_,
    reported_p_sidedness = NA_character_,
    reported_effect_size_type = NA_character_, reported_effect_size_value = NA_real_,
    p_value = NA_real_, p_operator = "=", p_sidedness = NA_character_,
    t_value = NA_real_, t_df = NA_real_,
    f_value = NA_real_, f_df1 = NA_real_, f_df2 = NA_real_,
    z_value = NA_real_, chi2_value = NA_real_, chi2_df = NA_real_,
    r_value = NA_real_,
    n1 = NA_real_, n2 = NA_real_, n_total = NA_real_, n_eff = NA_real_,
    effect_size_type = NA_character_, effect_size_value = NA_real_,
    estimate = NA_real_, se_estimate = NA_real_,
    raw_data_file = NA_character_, raw_variable_names = NA_character_,
    model_formula = NA_character_, contrast_direction = NA_character_,
    analysis_label = NA_character_, statistic_source = NA_character_,
    bayesian_input_status = NA_character_, extraction_note = NA_character_
) {
  tibble::tibble(
    id = id,
    study_id = study_id,
    study_DOI = study_DOI,
    recomputation_status = recomputation_status,
    stat_test = stat_test,
    reported_result = reported_result,
    reported_p_value = reported_p_value,
    reported_p_operator = reported_p_operator,
    reported_p_sidedness = reported_p_sidedness,
    reported_effect_size_type = reported_effect_size_type,
    reported_effect_size_value = reported_effect_size_value,
    p_value = p_value,
    p_operator = p_operator,
    p_sidedness = p_sidedness,
    t_value = t_value,
    t_df = t_df,
    f_value = f_value,
    f_df1 = f_df1,
    f_df2 = f_df2,
    z_value = z_value,
    chi2_value = chi2_value,
    chi2_df = chi2_df,
    r_value = r_value,
    n1 = n1,
    n2 = n2,
    n_total = n_total,
    n_eff = n_eff,
    effect_size_type = effect_size_type,
    effect_size_value = effect_size_value,
    estimate = estimate,
    se_estimate = se_estimate,
    raw_data_file = raw_data_file,
    raw_variable_names = raw_variable_names,
    model_formula = model_formula,
    contrast_direction = contrast_direction,
    analysis_label = analysis_label,
    statistic_source = statistic_source,
    bayesian_input_status = bayesian_input_status,
    extraction_note = extraction_note
  )
}

extract_aov_row <- function(aov_model, term, label) {
  tab <- as.data.frame(summary(aov_model)[[1]])
  tab$term <- trimws(rownames(tab))
  row <- tab[tab$term == term, , drop = FALSE]
  residual <- tab[tab$term == "Residuals", , drop = FALSE]
  if (nrow(row) != 1 || nrow(residual) != 1) {
    stop(
      "Could not extract ANOVA term '", term, "' for ", label,
      ". Available terms: ", paste(tab$term, collapse = ", "),
      call. = FALSE
    )
  }
  list(effect = row, residual = residual, table = tab)
}

eta_p2_from_f <- function(f_value, df1, df2) {
  (f_value * df1) / ((f_value * df1) + df2)
}

bayes_status_from_test <- function(stat_test) {
  switch(
    stat_test,
    "one_sample_t_test" = "ready_jzs_ttest",
    "paired_t_test" = "ready_jzs_ttest",
    "independent_t_test" = "ready_jzs_ttest",
    "welch_independent_t_test" = "ready_jzs_ttest_summary_level",
    "pearson_correlation" = "ready_correlation_diagnostic",
    "sem_contrast" = "ready_normal_prior_diagnostic",
    "factorial_between_anova" = "ready_anova_diagnostic_not_direct_jzs",
    "not_classified"
  )
}
