# R/reproduce/study_13.R
# Study 13: Hawkins, Camp, & Schunke (2022/2025)
# DOI: 10.1177/00986283221142016
#
# Purpose:
# Recompute the two Welch independent-samples t-tests used in the Bayesian
# reanalysis index for study_13 directly from the SPSS .sav file.
#
# Target rows in analysis_index_template.csv:
#   id = 8: subjective knowledge
#   id = 9: objective knowledge
#
# Data assumptions from uploaded SPSS syntax:
#   T-TEST GROUPS=condition(0 1)
#   condition == 0: control
#   condition == 1: education intervention
#
# Test used:
#   Welch independent-samples t-test, two-sided, unequal variances.
#   This matches the paper's Satterthwaite/Welch t-tests.

check_required_columns_study_13 <- function(dat, required_columns) {
  missing_columns <- setdiff(required_columns, names(dat))

  if (length(missing_columns) > 0) {
    stop(
      "study_13 is missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  invisible(TRUE)
}


recompute_study_13_welch_t <- function(
    dat,
    id,
    analysis_label,
    outcome,
    reported_result,
    reported_effect_size_value
) {

  check_required_columns_study_13(
    dat = dat,
    required_columns = c("condition", outcome)
  )

  # Haven imports SPSS labelled variables as numeric labelled vectors.
  # The SPSS syntax uses GROUPS=condition(0 1), so the numeric values
  # 0 and 1 are the relevant group codes.
  condition_numeric <- as.numeric(dat$condition)

  # ------------------------------------------------------------
  # 1. Select the two groups explicitly.
  # ------------------------------------------------------------

  education <- dat[[outcome]][condition_numeric == 1]
  control <- dat[[outcome]][condition_numeric == 0]

  education <- education[!is.na(education)]
  control <- control[!is.na(control)]

  # ------------------------------------------------------------
  # 2. Recompute the exact Welch t-test from the raw data.
  # ------------------------------------------------------------

  test_result <- stats::t.test(
    x = education,
    y = control,
    var.equal = FALSE,
    alternative = "two.sided"
  )

  # ------------------------------------------------------------
  # 3. Recompute descriptive quantities from the same raw vectors.
  # ------------------------------------------------------------

  n1 <- length(education)
  n2 <- length(control)

  mean1 <- mean(education)
  mean2 <- mean(control)

  sd1 <- stats::sd(education)
  sd2 <- stats::sd(control)

  mean_difference <- mean1 - mean2

  # Welch standard error.
  se_difference <- sqrt(sd1^2 / n1 + sd2^2 / n2)

  # Pooled-SD Cohen's d. This is a conventional descriptive SMD.
  pooled_sd <- sqrt(
    ((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) /
      (n1 + n2 - 2)
  )

  cohens_d_pooled <- mean_difference / pooled_sd

  # The article states that Cohen's d was transformed from the appropriate
  # t value. The reported d values are closely reproduced by 2t / sqrt(df).
  # We store this value in effect_size_value to match the article target,
  # while preserving the pooled-SD d in the notes.
  t_value <- as.numeric(test_result$statistic)
  t_df <- as.numeric(test_result$parameter)
  d_from_t_df <- 2 * t_value / sqrt(t_df)

  n_eff <- (n1 * n2) / (n1 + n2)

  # ------------------------------------------------------------
  # 4. Return one standardised recomputed row.
  # ------------------------------------------------------------

  tibble::tibble(
    id = id,
    study_id = "study_13",
    study_DOI = "10.1177/00986283221142016",
    analysis_label = analysis_label,
    stat_test = "welch_independent_t_test",
    reported_result = reported_result,

    p_value = as.numeric(test_result$p.value),
    p_operator = "=",
    p_sidedness = "two_sided",

    t_value = t_value,
    t_df = t_df,

    f_value = NA_real_,
    f_df1 = NA_real_,
    f_df2 = NA_real_,

    z_value = NA_real_,

    chi2_value = NA_real_,
    chi2_df = NA_real_,

    r_value = NA_real_,

    n1 = n1,
    n2 = n2,
    n_total = n1 + n2,
    n_eff = n_eff,

    effect_size_type = "cohens_d",
    effect_size_value = d_from_t_df,

    estimate = mean_difference,
    se_estimate = se_difference,

    statistic_source = "recomputed_from_raw_data",

    notes = paste0(
      "Recomputed from SPSS .sav using Welch independent-samples t-test. ",
      "Group variable: condition; 1 = education, 0 = control. ",
      "Education n1 = ", n1, "; control n2 = ", n2, ". ",
      "Mean education = ", round(mean1, 6), "; mean control = ", round(mean2, 6), ". ",
      "SD education = ", round(sd1, 6), "; SD control = ", round(sd2, 6), ". ",
      "Reported article d = ", reported_effect_size_value, ". ",
      "effect_size_value stores d transformed as 2*t/sqrt(df), matching the article's reported d convention. ",
      "The conventional pooled-SD Cohen's d recomputed from raw data is ", round(cohens_d_pooled, 6), "."
    )
  )
}


reproduce_study_13 <- function(
    data_path = "data/raw/study_13/DATA_Cleaned_and_coded_for_condition.sav",
    output_path = "outputs/reproduced/study_13_recomputed.csv"
) {

  if (!requireNamespace("haven", quietly = TRUE)) {
    stop(
      "Package 'haven' is required to read SPSS .sav files. ",
      "Install it with install.packages('haven')."
    )
  }

  if (!file.exists(data_path)) {
    stop(
      "Missing SPSS .sav file for study_13: ", data_path, "\n",
      "Expected file name: DATA_Cleaned_and_coded_for_condition.sav"
    )
  }

  dat <- haven::read_sav(data_path)

  rows <- dplyr::bind_rows(

    recompute_study_13_welch_t(
      dat = dat,
      id = 8,
      analysis_label = "key_result_subjective_knowledge",
      outcome = "subj_total",
      reported_result = "t_sat(455.40) = 14.45, p < .0005, d = 1.35",
      reported_effect_size_value = 1.35
    ),

    recompute_study_13_welch_t(
      dat = dat,
      id = 9,
      analysis_label = "second_result_objective_knowledge",
      outcome = "obj_total",
      reported_result = "t_sat(466.17) = 9.79, p < .0005, d = 0.91",
      reported_effect_size_value = 0.91
    )
  )

  dir.create(
    path = dirname(output_path),
    recursive = TRUE,
    showWarnings = FALSE
  )

  readr::write_csv(
    x = rows,
    file = output_path
  )

  rows
}
