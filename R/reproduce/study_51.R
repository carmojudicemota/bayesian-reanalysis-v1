# R/reproduce/study_51.R
# Study 51: Clinton-Lisell & Kelly. DOI: 10.1177/14757257231197359
# Reconstructs rows id 39 and 40: public sharing vs closed control independent t-tests.

if (!exists("make_recomputed_row")) source("R/reproduce/00_reproduction_helpers.R")

recompute_study_51_independent_t <- function(dat, id, analysis_label, outcome,
                                             reported_result, reported_p_value,
                                             reported_p_operator, reported_effect_size_value,
                                             inconsistency_note, raw_data_file) {
  check_required_columns(dat, c("control0share1", outcome), "study_51")
  condition <- as.numeric(dat$control0share1)
  public <- as.numeric(dat[[outcome]])[condition == 1]
  closed <- as.numeric(dat[[outcome]])[condition == 0]
  public <- public[!is.na(public)]
  closed <- closed[!is.na(closed)]

  test_result <- stats::t.test(x = public, y = closed, var.equal = TRUE, alternative = "two.sided")
  n1 <- length(public)
  n2 <- length(closed)
  mean1 <- mean(public)
  mean2 <- mean(closed)
  sd1 <- stats::sd(public)
  sd2 <- stats::sd(closed)
  pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
  mean_difference <- mean1 - mean2
  se_difference <- pooled_sd * sqrt(1 / n1 + 1 / n2)
  d <- mean_difference / pooled_sd
  n_eff <- (n1 * n2) / (n1 + n2)

  status <- if (nzchar(inconsistency_note)) "recomputed_from_raw_data_reporting_inconsistency_detected" else "recomputed_from_raw_data"

  make_recomputed_row(
    id = id,
    study_id = "study_51",
    study_DOI = "10.1177/14757257231197359",
    recomputation_status = status,
    stat_test = "independent_t_test",
    reported_result = reported_result,
    reported_p_value = reported_p_value,
    reported_p_operator = reported_p_operator,
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
    effect_size_value = d,
    estimate = mean_difference,
    se_estimate = se_difference,
    raw_data_file = raw_data_file,
    raw_variable_names = paste("control0share1", outcome, sep = "; "),
    model_formula = paste0(outcome, " ~ control0share1; Student independent-samples t-test, equal variances"),
    contrast_direction = "public sharing condition minus closed control condition; control0share1 1 minus 0",
    analysis_label = analysis_label,
    statistic_source = "stats::t.test(var.equal = TRUE) on raw vectors",
    bayesian_input_status = bayes_status_from_test("independent_t_test"),
    extraction_note = paste0(
      "Recovered group sizes, t, df, exact p, pooled-SD d, mean difference and SE. ",
      "Public n = ", n1, "; closed n = ", n2,
      "; public mean = ", round(mean1, 6), "; closed mean = ", round(mean2, 6), ". ",
      inconsistency_note
    )
  )
}

reproduce_study_51 <- function(
    data_path = "data/raw/study_51/Untitled3.sav",
    output_path = "outputs/reproduced/study_51_recomputed.csv"
) {
  check_required_packages(c("haven", "dplyr", "tibble", "readr"))
  data_path <- resolve_existing_file(c(data_path, "data/raw/study_51/Untitled3.sav"), "study_51 SPSS file")
  dat <- haven::read_sav(data_path)

  rows <- dplyr::bind_rows(
    recompute_study_51_independent_t(
      dat, 39, "perceived_learning_public_sharing_vs_closed_control", "knowledgeskills",
      "t(100) = 2.79, p < .01, d = 0.55", 0.01, "<", 0.55, "", data_path
    ),
    recompute_study_51_independent_t(
      dat, 40, "anxiety_public_sharing_vs_closed_control", "anxiety",
      "t(100) = -0.95, p < .01, d = -0.19", 0.01, "<", -0.19,
      "Reported p < .01 is incompatible with t = -0.95 and df = 100; recomputed p is retained for the Bayesian layer.",
      data_path
    )
  )
  write_recomputed_results(rows, output_path)
}
