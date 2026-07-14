# R/reproduce/study_14.R
# Study 14: Anglin & Edlund. DOI: 10.1177/1475725719859453
# Reconstructs rows id 10 and id 11: Pearson correlations with pairwise complete observations.

if (!exists("make_recomputed_row")) source("R/reproduce/00_reproduction_helpers.R")

recompute_study_14_correlation <- function(dat, id, analysis_label, x_var, y_var,
                                           reported_result, reported_df,
                                           reported_r, raw_data_file) {
  check_required_columns(dat, c(x_var, y_var), "study_14")

  x <- as.numeric(dat[[x_var]])
  y <- as.numeric(dat[[y_var]])
  keep <- stats::complete.cases(x, y)
  x <- x[keep]
  y <- y[keep]

  cor_result <- stats::cor.test(x = x, y = y, method = "pearson", alternative = "two.sided")
  n_total <- length(x)
  r_value <- as.numeric(cor_result$estimate)
  t_df <- n_total - 2
  t_value <- r_value * sqrt(t_df / (1 - r_value^2))

  make_recomputed_row(
    id = id,
    study_id = "study_14",
    study_DOI = "10.1177/1475725719859453",
    recomputation_status = "recomputed_from_raw_data_pairwise_complete",
    stat_test = "pearson_correlation",
    reported_result = reported_result,
    reported_p_value = 0.001,
    reported_p_operator = "<",
    reported_p_sidedness = "two_sided",
    reported_effect_size_type = "correlation_r",
    reported_effect_size_value = reported_r,
    p_value = as.numeric(cor_result$p.value),
    p_operator = "=",
    p_sidedness = "two_sided",
    t_value = as.numeric(t_value),
    t_df = as.numeric(t_df),
    r_value = r_value,
    n_total = n_total,
    n_eff = n_total,
    effect_size_type = "correlation_r",
    effect_size_value = r_value,
    raw_data_file = raw_data_file,
    raw_variable_names = paste(x_var, y_var, sep = "; "),
    model_formula = paste0("Pearson correlation: ", x_var, " with ", y_var, "; pairwise complete observations"),
    contrast_direction = paste0("positive association between ", x_var, " and ", y_var),
    analysis_label = analysis_label,
    statistic_source = "stats::cor.test(method = 'pearson')",
    bayesian_input_status = bayes_status_from_test("pearson_correlation"),
    extraction_note = paste0(
      "Recovered r, equivalent t, df, exact p and pairwise N. ",
      "Article df = ", reported_df, "; recomputed df = ", t_df, "; pairwise N = ", n_total, "."
    )
  )
}

reproduce_study_14 <- function(
    data_path = "data/raw/study_14/Perceived_need_for_reform_and_teaching_of_best_research_practices.sav",
    output_path = "outputs/reproduced/study_14_recomputed.csv"
) {
  check_required_packages(c("haven", "dplyr", "tibble", "readr"))
  data_path <- resolve_existing_file(c(
    data_path,
    "data/raw/study_14/Perceived_need_for_reform_and_teaching_of_best_research_practices.sav",
    "data/raw/study_14/Perceived need for reform and teaching of best research practices 11-3-17.sav"
  ), "study_14 SPSS file")

  dat <- haven::read_sav(data_path)

  rows <- dplyr::bind_rows(
    recompute_study_14_correlation(
      dat, 10, "reform_with_graduate_replication_teaching", "reform", "grad_replication",
      "r(56) = .46, p < .001", 56, 0.46, data_path
    ),
    recompute_study_14_correlation(
      dat, 11, "reform_with_advanced_undergraduate_teaching", "reform", "Adv_topics",
      "r(228) = .30, p < .001", 228, 0.30, data_path
    )
  )

  write_recomputed_results(rows, output_path)
}
