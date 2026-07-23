# R/reproduce/study_33.R
# Study 33: Freedman, Oates, & Kirk. DOI: 10.1037/stl0000227
# Reconstructs rows id 20 and 21: one-sample t-tests against midpoint 3.

if (!exists("make_recomputed_row")) source("R/reproduce/00_reproduction_helpers.R")

recompute_study_33_one_sample <- function(dat, id, analysis_label, outcome,
                                          reported_result, reported_p_value,
                                          reported_effect_size_value,
                                          raw_data_file) {
  midpoint <- 3
  x <- as.numeric(dat[[outcome]])
  x <- x[!is.na(x)]
  test_result <- stats::t.test(x = x, mu = midpoint, alternative = "two.sided")
  n <- length(x)
  mean_x <- mean(x)
  sd_x <- stats::sd(x)
  estimate <- mean_x - midpoint
  se_estimate <- sd_x / sqrt(n)
  d <- estimate / sd_x

  make_recomputed_row(
    id = id,
    study_id = "study_33",
    study_DOI = "10.1037/stl0000227",
    recomputation_status = "recomputed_from_raw_data",
    stat_test = "one_sample_t_test",
    reported_result = reported_result,
    reported_p_value = reported_p_value,
    reported_p_operator = "=",
    reported_p_sidedness = "two_sided",
    reported_effect_size_type = "cohens_d",
    reported_effect_size_value = reported_effect_size_value,
    p_value = as.numeric(test_result$p.value),
    p_operator = "=",
    p_sidedness = "two_sided",
    t_value = as.numeric(test_result$statistic),
    t_df = as.numeric(test_result$parameter),
    n_total = n,
    n_eff = n,
    effect_size_type = "cohens_d_one_sample",
    effect_size_value = d,
    estimate = estimate,
    se_estimate = se_estimate,
    raw_data_file = raw_data_file,
    raw_variable_names = outcome,
    model_formula = paste0("one-sample t-test: ", outcome, " against mu = 3"),
    contrast_direction = paste0(outcome, " mean minus scale midpoint 3"),
    analysis_label = analysis_label,
    statistic_source = "stats::t.test(mu = 3)",
    bayesian_input_status = bayes_status_from_test("one_sample_t_test"),
    extraction_note = paste0("Recovered n, t, df, exact p, one-sample d, estimate and SE. Mean = ", round(mean_x, 6), "; SD = ", round(sd_x, 6), ".")
  )
}

reproduce_study_33 <- function(
    data_path = "data/raw/study_33/ProcessedDataSPSS.sav",
    output_path = "outputs/reproduced/study_33_recomputed.csv"
) {
  check_required_packages(c("haven", "dplyr", "tibble", "readr"))
  data_path <- resolve_existing_file(c(data_path, "data/raw/study_33/ProcessedDataSPSS.sav"), "study_33 SPSS file")
  dat <- haven::read_sav(data_path)
  check_required_columns(dat, c("Engagement_Avg", "SocialPsychRealWorld_Avg"), "study_33")

  rows <- dplyr::bind_rows(
    recompute_study_33_one_sample(
      dat, 20, "engagement_with_pandemic_related_research", "Engagement_Avg",
      "t(11) = 4.27, p = .001, d = 1.23, 95% CI [0.58, 1.81]",
      0.001, 1.23, data_path
    ),
    recompute_study_33_one_sample(
      dat, 21, "applicability_of_social_psychology", "SocialPsychRealWorld_Avg",
      "t(11) = 16.03, p < .001, d = 4.63, 95% CI [1.41, 1.86]",  # CORRECTED: Ali's phase-2 sheet mistyped this as p = .001; the manuscript reports p < .001. Recomputed p = 5.650749e-09. (Peter Allen, 17 Jul)
      0.001, 4.63, data_path
    )
  )

  write_recomputed_results(rows, output_path)
}
