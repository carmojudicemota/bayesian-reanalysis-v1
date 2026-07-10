# R/reproduce/study_41.R
# Study 41: Kelly & Parrish. DOI: 10.1177/00986283251313760
# Reconstructs rows id 28 and 29: paired pre-post skill ratings.

if (!exists("make_recomputed_row")) source("R/reproduce/00_reproduction_helpers.R")

recompute_study_41_paired <- function(dat, id, analysis_label, pre_var, post_var,
                                      reported_result, reported_effect_size_value,
                                      raw_data_file) {
  pre <- as.numeric(dat[[pre_var]])
  post <- as.numeric(dat[[post_var]])
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

  make_recomputed_row(
    id = id,
    study_id = "study_41",
    study_DOI = "10.1177/00986283251313760",
    recomputation_status = "recomputed_from_raw_data",
    stat_test = "paired_t_test",
    reported_result = reported_result,
    reported_p_value = 0.001,
    reported_p_operator = "<",
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
    effect_size_type = "cohens_dz_paired",
    effect_size_value = d_z,
    estimate = mean_difference,
    se_estimate = se_difference,
    raw_data_file = raw_data_file,
    raw_variable_names = paste(pre_var, post_var, sep = "; "),
    model_formula = paste0("paired t-test: ", post_var, " - ", pre_var),
    contrast_direction = "post minus pre",
    analysis_label = analysis_label,
    statistic_source = "stats::t.test(paired = TRUE) on complete pairs",
    bayesian_input_status = bayes_status_from_test("paired_t_test"),
    extraction_note = paste0(
      "Recovered paired n, t, df, exact p, mean difference, SE and paired dz. ",
      "Pre mean = ", round(mean(pre), 6), "; post mean = ", round(mean(post), 6),
      "; pre SD = ", round(stats::sd(pre), 6), "; post SD = ", round(stats::sd(post), 6), "."
    )
  )
}

reproduce_study_41 <- function(
    data_path = "data/raw/study_41/Untitled3.sav",
    output_path = "outputs/reproduced/study_41_recomputed.csv"
) {
  check_required_packages(c("haven", "dplyr", "tibble", "readr"))
  data_path <- resolve_existing_file(c(data_path, "data/raw/study_41/Untitled3.sav"), "study_41 SPSS file")
  dat <- haven::read_sav(data_path)
  check_required_columns(dat, c("PRE_Analyze", "POST_Analyze", "PRE_Intrdata", "POST_Intrdata"), "study_41")

  rows <- dplyr::bind_rows(
    recompute_study_41_paired(
      dat, 28, "statistically_analyze_data", "PRE_Analyze", "POST_Analyze",
      "t(36) = 8.53, p < .001, d = 1.56", 1.56, data_path
    ),
    recompute_study_41_paired(
      dat, 29, "interpret_data_by_relating_results_to_hypothesis", "PRE_Intrdata", "POST_Intrdata",
      "t(36) = 6.15, p < .001, d = 1.14", 1.14, data_path
    )
  )

  write_recomputed_results(rows, output_path)
}
