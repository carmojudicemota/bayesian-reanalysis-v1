# R/reproduce/study_05.R
# Study 5: Hard, Lovett, & Brady. DOI: 10.1037/stl0000136
# Reconstructs rows id 3 and id 4 from the raw CSV.

if (!exists("make_recomputed_row")) source("R/reproduce/00_reproduction_helpers.R")

recompute_study_05_independent_t <- function(dat, id, analysis_label, outcome,
                                             reported_result, reported_p_value,
                                             reported_effect_size_value,
                                             raw_data_file) {
  check_required_columns(dat, c("Speciality", outcome), "study_05")

  psychology <- dat[[outcome]][dat$Speciality == 1]
  nonpsychology <- dat[[outcome]][dat$Speciality == 0]
  psychology <- psychology[!is.na(psychology)]
  nonpsychology <- nonpsychology[!is.na(nonpsychology)]

  test_result <- stats::t.test(
    x = psychology,
    y = nonpsychology,
    var.equal = TRUE,
    alternative = "two.sided"
  )

  n1 <- length(psychology)
  n2 <- length(nonpsychology)
  mean1 <- mean(psychology)
  mean2 <- mean(nonpsychology)
  sd1 <- stats::sd(psychology)
  sd2 <- stats::sd(nonpsychology)

  pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
  mean_difference <- mean1 - mean2
  se_difference <- pooled_sd * sqrt(1 / n1 + 1 / n2)
  cohens_d <- mean_difference / pooled_sd
  n_eff <- (n1 * n2) / (n1 + n2)

  make_recomputed_row(
    id = id,
    study_id = "study_05",
    study_DOI = "10.1037/stl0000136",
    recomputation_status = "recomputed_from_raw_data",
    stat_test = "independent_t_test",
    reported_result = reported_result,
    reported_p_value = reported_p_value,
    reported_p_operator = "<",
    reported_p_sidedness = "two_sided",
    reported_effect_size_type = "cohens_d",
    reported_effect_size_value = reported_effect_size_value,
    p_value = as.numeric(test_result$p.value),
    p_operator = "=",
    p_sidedness = "two_sided",
    t_value = as.numeric(test_result$statistic),
    t_df = as.numeric(test_result$parameter),
    n1 = n1,
    n2 = n2,
    n_total = n1 + n2,
    n_eff = n_eff,
    effect_size_type = "cohens_d",
    effect_size_value = cohens_d,
    estimate = mean_difference,
    se_estimate = se_difference,
    raw_data_file = raw_data_file,
    raw_variable_names = paste("Speciality", outcome, sep = "; "),
    model_formula = paste0(outcome, " ~ Speciality; Student independent-samples t-test, equal variances"),
    contrast_direction = "psychology students minus nonpsychology students; Speciality 1 minus Speciality 0",
    analysis_label = analysis_label,
    statistic_source = "stats::t.test(var.equal = TRUE) on raw vectors",
    bayesian_input_status = bayes_status_from_test("independent_t_test"),
    extraction_note = paste0(
      "Recovered group sizes, t, df, exact p, pooled-SD d, mean difference and SE. ",
      "Psychology n = ", n1, "; nonpsychology n = ", n2,
      "; psychology mean = ", round(mean1, 6), "; nonpsychology mean = ", round(mean2, 6),
      "; psychology SD = ", round(sd1, 6), "; nonpsychology SD = ", round(sd2, 6), "."
    )
  )
}

reproduce_study_05 <- function(
    data_path = "data/raw/study_05/HardLovettBrady_Data_Shared.csv",
    output_path = "outputs/reproduced/study_05_recomputed.csv"
) {
  check_required_packages(c("readr", "dplyr", "tibble"))
  data_path <- resolve_existing_file(c(
    data_path,
    "data/raw/study_05/HardLovettBrady_Data_Shared.csv"
  ), "study_05 raw CSV")

  dat <- readr::read_csv(data_path, show_col_types = FALSE)

  rows <- dplyr::bind_rows(
    recompute_study_05_independent_t(
      dat, 3,
      "senior_year_16_item_quiz_performance",
      "16ItemQuizFollowupPerformance",
      "t(154) = 3.26, p = .001, d = 0.74",   # CORRECTED: manuscript typed "p < .001" but jamovi reports p = .001; recomputed p = .0013572, two-tailed (Peter Allen, 17 Jul).
      0.001, 0.74, data_path
    ),
    recompute_study_05_independent_t(
      dat, 4,
      "number_of_additional_psychology_courses",
      "NumPsychClass",
      "t(154) = 16.06, p < .001, d = 3.63",
      0.001, 3.63, data_path
    )
  )

  write_recomputed_results(rows, output_path)
}
