# R/reproduce/study_49.R
# Study 49: Gordon, Hughes, & Smith. DOI: 10.1177/00986283251325842
# Reconstructs rows id 37 and 38: paired lectured vs nonlectured final tests.

if (!exists("make_recomputed_row")) source("R/reproduce/00_reproduction_helpers.R")

recompute_study_49_paired <- function(dat, id, analysis_label, lectured_var, nonlectured_var,
                                      reported_result, reported_p_value, reported_effect_size_value,
                                      raw_data_file) {
  check_required_columns(dat, c(lectured_var, nonlectured_var), "study_49")
  lectured <- dat[[lectured_var]]
  nonlectured <- dat[[nonlectured_var]]
  keep <- stats::complete.cases(lectured, nonlectured)
  lectured <- lectured[keep]
  nonlectured <- nonlectured[keep]

  test_result <- stats::t.test(x = lectured, y = nonlectured, paired = TRUE, alternative = "two.sided")
  diff <- lectured - nonlectured
  n <- length(diff)
  mean_difference <- mean(diff)
  sd_difference <- stats::sd(diff)
  se_difference <- sd_difference / sqrt(n)
  d_z <- mean_difference / sd_difference
  t_value <- as.numeric(test_result$statistic)
  t_df <- as.numeric(test_result$parameter)
  p_two_sided <- as.numeric(test_result$p.value)
  p_one_sided_article_direction <- stats::pt(q = -abs(t_value), df = t_df)

  make_recomputed_row(
    id = id,
    study_id = "study_49",
    study_DOI = "10.1177/00986283251325842",
    recomputation_status = "recomputed_from_raw_data_reporting_inconsistency_detected",
    stat_test = "paired_t_test",
    reported_result = reported_result,
    reported_p_value = reported_p_value,
    reported_p_operator = "=",
    reported_p_sidedness = "two_sided_in_index_but_article_matches_one_sided",
    reported_effect_size_type = "cohens_d",
    reported_effect_size_value = reported_effect_size_value,
    p_value = p_one_sided_article_direction,
    p_operator = "=",
    p_sidedness = "one_sided",
    t_value = t_value,
    t_df = t_df,
    n_total = n,
    n_eff = n,
    effect_size_type = "cohens_dz_paired",
    effect_size_value = d_z,
    estimate = mean_difference,
    se_estimate = se_difference,
    raw_data_file = raw_data_file,
    raw_variable_names = paste(lectured_var, nonlectured_var, sep = "; "),
    model_formula = paste0("paired t-test: ", lectured_var, " - ", nonlectured_var),
    contrast_direction = "lectured content minus nonlectured content",
    analysis_label = analysis_label,
    statistic_source = "stats::t.test(paired = TRUE) on complete pairs; stored p is one-sided because it matches reported p",
    bayesian_input_status = bayes_status_from_test("paired_t_test"),
    extraction_note = paste0(
      "Recovered paired n, t, df, mean difference, SE and paired dz. The reported p matches one-sided p = ",
      signif(p_one_sided_article_direction, 6), ", not two-sided p = ", signif(p_two_sided, 6),
      ". This row is flagged so the Bayesian layer does not silently treat the reported p as two-sided."
    )
  )
}

reproduce_study_49 <- function(
    data_path = "data/raw/study_49/Qualtrics_Final_Test_Data_Jan_2024.csv",
    output_path = "outputs/reproduced/study_49_recomputed.csv"
) {
  check_required_packages(c("readr", "dplyr", "tibble"))
  data_path <- resolve_existing_file(c(
    data_path,
    "data/raw/study_49/Qualtrics_Final_Test_Data_Jan_2024.csv",
    "data/raw/study_49/Qualtrics Final Test Data Jan 2024 copy.csv"
  ), "study_49 CSV")

  dat <- readr::read_csv(data_path, show_col_types = FALSE)
  rows <- dplyr::bind_rows(
    recompute_study_49_paired(
      dat, 37, "final_short_essay_test_lectured_vs_nonlectured",
      "VIDEO_WRITTEN_AVG", "NONVIDEO_WRITTEN_AVG",
      "t(27) = 1.20, p = .119, d = 0.23", 0.119, 0.23, data_path
    ),
    recompute_study_49_paired(
      dat, 38, "final_multiple_choice_test_lectured_vs_nonlectured",
      "VIDEO_MC_AVG", "NONVIDEO_MC_AVG",
      "t(27) = 0.47, p = .319, d = 0.09", 0.319, 0.09, data_path
    )
  )
  write_recomputed_results(rows, output_path)
}
