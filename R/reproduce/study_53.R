# R/reproduce/study_53.R
# Study 53: Wong et al., Study 1. DOI: 10.1037/stl0000306
# Reconstructs rows id 41 and 42: eye-level vs lecture-hall independent t-tests.

if (!exists("make_recomputed_row")) source("R/reproduce/00_reproduction_helpers.R")

recompute_study_53_independent_t <- function(dat, id, analysis_label, outcome,
                                             reported_result, reported_p_value,
                                             reported_p_operator, reported_effect_size_value,
                                             raw_data_file) {
  eye <- as.numeric(dat[[outcome]])[dat$condition == "eye_level"]
  lecture <- as.numeric(dat[[outcome]])[dat$condition == "lecture_hall"]
  eye <- eye[!is.na(eye)]
  lecture <- lecture[!is.na(lecture)]

  test_result <- stats::t.test(x = eye, y = lecture, var.equal = TRUE, alternative = "two.sided")
  n1 <- length(eye)
  n2 <- length(lecture)
  mean1 <- mean(eye)
  mean2 <- mean(lecture)
  sd1 <- stats::sd(eye)
  sd2 <- stats::sd(lecture)
  pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
  mean_difference <- mean1 - mean2
  se_difference <- pooled_sd * sqrt(1 / n1 + 1 / n2)
  d <- mean_difference / pooled_sd
  n_eff <- (n1 * n2) / (n1 + n2)

  make_recomputed_row(
    id = id,
    study_id = "study_53",
    study_DOI = "10.1037/stl0000306",
    recomputation_status = "recomputed_from_raw_data",
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
    raw_variable_names = paste("Condition_string; filter_$", outcome, sep = "; "),
    model_formula = paste0(outcome, " ~ Condition_string, filtered to filter_$ == 1; Student independent-samples t-test, equal variances"),
    contrast_direction = "eye-level condition minus lecture-hall condition",
    analysis_label = analysis_label,
    statistic_source = "stats::t.test(var.equal = TRUE) on raw vectors after filter_$ == 1",
    bayesian_input_status = bayes_status_from_test("independent_t_test"),
    extraction_note = paste0(
      "Recovered group sizes, t, df, exact p, pooled-SD d, mean difference and SE. ",
      "Eye-level n = ", n1, "; lecture-hall n = ", n2,
      "; eye-level mean = ", round(mean1, 6), "; lecture-hall mean = ", round(mean2, 6), "."
    )
  )
}

reproduce_study_53 <- function(
    data_path = "data/raw/study_53/Study_1_SPSS.sav",
    output_path = "outputs/reproduced/study_53_recomputed.csv"
) {
  check_required_packages(c("haven", "dplyr", "tibble", "readr"))
  data_path <- resolve_existing_file(c(
    data_path,
    "data/raw/study_53/Study_1_SPSS.sav",
    "data/raw/study_53/Study 1 SPSS.sav"
  ), "study_53 SPSS file")

  dat <- haven::read_sav(data_path)
  check_required_columns(dat, c("Condition_string", "filter_$", "ImmediacyBehaviors_verbal", "Likeability_Measure"), "study_53")
  keep_filter <- !is.na(dat[["filter_$"]]) & dat[["filter_$"]] == 1
  dat <- dat[keep_filter, ]
  dat$condition <- ifelse(dat$Condition_string == "eye-level", "eye_level",
                          ifelse(dat$Condition_string == "lecture-hall", "lecture_hall", NA_character_))
  dat <- dat[!is.na(dat$condition), ]

  rows <- dplyr::bind_rows(
    recompute_study_53_independent_t(
      dat, 41, "verbal_immediacy_eye_level_vs_lecture_hall", "ImmediacyBehaviors_verbal",
      "t(246) = 3.29, p = .00, d = 0.42", 0.001, "<", 0.42, data_path
    ),
    recompute_study_53_independent_t(
      dat, 42, "likeability_eye_level_vs_lecture_hall", "Likeability_Measure",
      "t(246) = 2.79, p = .01, d = 0.35", 0.01, "=", 0.35, data_path
    )
  )

  write_recomputed_results(rows, output_path)
}
