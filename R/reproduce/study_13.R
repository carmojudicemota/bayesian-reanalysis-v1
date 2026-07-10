# R/reproduce/study_13.R
# Study 13: Hawkins et al. DOI: 10.1177/00986283221142016
# Reconstructs rows id 8 and id 9 from the SPSS file.

if (!exists("make_recomputed_row")) source("R/reproduce/00_reproduction_helpers.R")

recompute_study_13_welch_t <- function(dat, id, analysis_label, outcome,
                                       reported_result, reported_p_value,
                                       reported_effect_size_value,
                                       raw_data_file) {
  check_required_columns(dat, c("condition", outcome), "study_13")

  condition_numeric <- as.numeric(dat$condition)
  education <- dat[[outcome]][condition_numeric == 1]
  control <- dat[[outcome]][condition_numeric == 0]
  education <- education[!is.na(education)]
  control <- control[!is.na(control)]

  test_result <- stats::t.test(
    x = education,
    y = control,
    var.equal = FALSE,
    alternative = "two.sided"
  )

  n1 <- length(education)
  n2 <- length(control)
  mean1 <- mean(education)
  mean2 <- mean(control)
  sd1 <- stats::sd(education)
  sd2 <- stats::sd(control)
  mean_difference <- mean1 - mean2
  se_difference <- sqrt(sd1^2 / n1 + sd2^2 / n2)
  pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
  cohens_d_pooled <- mean_difference / pooled_sd
  t_value <- as.numeric(test_result$statistic)
  t_df <- as.numeric(test_result$parameter)
  d_from_t_df <- 2 * t_value / sqrt(t_df)
  n_eff <- (n1 * n2) / (n1 + n2)

  make_recomputed_row(
    id = id,
    study_id = "study_13",
    study_DOI = "10.1177/00986283221142016",
    recomputation_status = "recomputed_from_raw_data",
    stat_test = "welch_independent_t_test",
    reported_result = reported_result,
    reported_p_value = reported_p_value,
    reported_p_operator = "<",
    reported_p_sidedness = "two_sided",
    reported_effect_size_type = "cohens_d",
    reported_effect_size_value = reported_effect_size_value,
    p_value = as.numeric(test_result$p.value),
    p_operator = "=",
    p_sidedness = "two_sided",
    t_value = t_value,
    t_df = t_df,
    n1 = n1,
    n2 = n2,
    n_total = n1 + n2,
    n_eff = n_eff,
    effect_size_type = "cohens_d_article_t_df_transform",
    effect_size_value = d_from_t_df,
    estimate = mean_difference,
    se_estimate = se_difference,
    raw_data_file = raw_data_file,
    raw_variable_names = paste("condition", outcome, sep = "; "),
    model_formula = paste0(outcome, " ~ condition; Welch independent-samples t-test"),
    contrast_direction = "education intervention minus control; condition 1 minus condition 0",
    analysis_label = analysis_label,
    statistic_source = "stats::t.test(var.equal = FALSE) on raw vectors",
    bayesian_input_status = bayes_status_from_test("welch_independent_t_test"),
    extraction_note = paste0(
      "Recovered Welch t, Satterthwaite df, group sizes, exact p, mean difference and Welch SE. ",
      "effect_size_value stores 2*t/sqrt(df), matching the article convention; pooled-SD d = ",
      round(cohens_d_pooled, 6), ". Education n = ", n1, "; control n = ", n2, "."
    )
  )
}

reproduce_study_13 <- function(
    data_path = "data/raw/study_13/DATA_Cleaned_and_coded_for_condition.sav",
    output_path = "outputs/reproduced/study_13_recomputed.csv"
) {
  check_required_packages(c("haven", "dplyr", "tibble", "readr"))
  data_path <- resolve_existing_file(c(
    data_path,
    "data/raw/study_13/DATA_Cleaned_and_coded_for_condition.sav",
    "data/raw/study_13/DATA Cleaned and coded for condition.sav"
  ), "study_13 SPSS file")

  dat <- haven::read_sav(data_path)

  rows <- dplyr::bind_rows(
    recompute_study_13_welch_t(
      dat, 8, "subjective_knowledge", "subj_total",
      "t_sat(455.40) = 14.45, p < .0005, d = 1.35",
      0.0005, 1.35, data_path
    ),
    recompute_study_13_welch_t(
      dat, 9, "objective_knowledge", "obj_total",
      "t_sat(466.17) = 9.79, p < .001, d = 0.91",
      0.001, 0.91, data_path
    )
  )

  write_recomputed_results(rows, output_path)
}
