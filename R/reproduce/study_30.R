# R/reproduce/study_30.R
# Study 30: Jern. DOI: 10.1037/stl0000104
# Reconstructs row id 19: PCTE posttest minus pretest paired t-test.

if (!exists("make_recomputed_row")) source("R/reproduce/00_reproduction_helpers.R")

reproduce_study_30 <- function(
    data_path = "data/raw/study_30/data_scoresOnly.csv",
    output_path = "outputs/reproduced/study_30_recomputed.csv"
) {
  check_required_packages(c("readr", "tibble"))
  data_path <- resolve_existing_file(c(
    data_path,
    "data/raw/study_30/data_scoresOnly.csv",
    "data/raw/study_30/data_scoresOnly copy.csv"
  ), "study_30 CSV")

  dat <- readr::read_csv(data_path, show_col_types = FALSE)
  check_required_columns(dat, c("Subject", "Pre-test", "Post-test"), "study_30")

  pre <- dat[["Pre-test"]]
  post <- dat[["Post-test"]]
  keep <- stats::complete.cases(pre, post)
  pre <- pre[keep]
  post <- post[keep]

  test_result <- stats::t.test(x = post, y = pre, paired = TRUE, alternative = "two.sided")
  diff <- post - pre
  n <- length(diff)
  mean_difference <- mean(diff)
  sd_difference <- stats::sd(diff)
  se_difference <- sd_difference / sqrt(n)
  d_z <- mean_difference / sd_difference

  rows <- make_recomputed_row(
    id = 19,
    study_id = "study_30",
    study_DOI = "10.1037/stl0000104",
    recomputation_status = "recomputed_from_raw_data",
    stat_test = "paired_t_test",
    reported_result = "t(36) = -1.21, p = .23",
    reported_p_value = 0.23,
    reported_p_operator = "=",
    reported_p_sidedness = "two_sided",
    p_value = as.numeric(test_result$p.value),
    p_operator = "=",
    p_sidedness = "two_sided",
    t_value = as.numeric(test_result$statistic),
    t_df = as.numeric(test_result$parameter),
    n_total = n,
    n_eff = n,
    effect_size_type = "cohens_dz_paired",
    effect_size_value = d_z,
    estimate = mean_difference,
    se_estimate = se_difference,
    raw_data_file = data_path,
    raw_variable_names = "Subject; Pre-test; Post-test",
    model_formula = "paired t-test: Post-test - Pre-test",
    contrast_direction = "posttest minus pretest",
    analysis_label = "pcte_posttest_minus_pretest",
    statistic_source = "stats::t.test(paired = TRUE) on complete pairs",
    bayesian_input_status = bayes_status_from_test("paired_t_test"),
    extraction_note = paste0(
      "Recovered paired n, t, df, exact p, mean difference, SE and paired dz. ",
      "N = ", n, "; pre mean = ", round(mean(pre), 6), "; post mean = ", round(mean(post), 6), "."
    )
  )

  write_recomputed_results(rows, output_path)
}
