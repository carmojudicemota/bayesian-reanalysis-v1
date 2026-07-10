# R/reproduce/study_21.R
# Study 21: Chopik et al. DOI: 10.1177/0098628318762900
# Reconstructs row id 15: paired pre-post trust comparison.

if (!exists("make_recomputed_row")) source("R/reproduce/00_reproduction_helpers.R")

reproduce_study_21 <- function(
    data_path = "data/raw/study_21/Spring2016.sav",
    output_path = "outputs/reproduced/study_21_recomputed.csv"
) {
  check_required_packages(c("haven", "tibble", "readr"))
  data_path <- resolve_existing_file(c(data_path, "data/raw/study_21/Spring2016.sav"), "study_21 SPSS file")
  dat <- haven::read_sav(data_path)
  check_required_columns(dat, c("trust1", "trust2"), "study_21")

  pre <- as.numeric(dat$trust1)
  post <- as.numeric(dat$trust2)
  keep <- stats::complete.cases(pre, post)
  pre <- pre[keep]
  post <- post[keep]

  test_result <- stats::t.test(x = post, y = pre, paired = TRUE, alternative = "two.sided")
  diff <- post - pre
  n <- length(diff)
  mean_pre <- mean(pre)
  mean_post <- mean(post)
  sd_pre <- stats::sd(pre)
  sd_post <- stats::sd(post)
  mean_difference <- mean(diff)
  sd_difference <- stats::sd(diff)
  se_difference <- sd_difference / sqrt(n)
  pooled_marginal_sd <- sqrt((sd_pre^2 + sd_post^2) / 2)
  d_article <- mean_difference / pooled_marginal_sd
  d_z <- mean_difference / sd_difference

  rows <- make_recomputed_row(
    id = 15,
    study_id = "study_21",
    study_DOI = "10.1177/0098628318762900",
    recomputation_status = "recomputed_from_raw_data",
    stat_test = "paired_t_test",
    reported_result = "Pre: M = 5.31, SD = 0.96; Post: M = 4.94, SD = 1.10; d = -0.36; p < .001",
    reported_p_value = 0.001,
    reported_p_operator = "<",
    reported_p_sidedness = "two_sided",
    reported_effect_size_type = "cohens_d",
    reported_effect_size_value = -0.36,
    p_value = as.numeric(test_result$p.value),
    p_operator = "=",
    p_sidedness = "two_sided",
    t_value = as.numeric(test_result$statistic),
    t_df = as.numeric(test_result$parameter),
    n_total = n,
    n_eff = n,
    effect_size_type = "cohens_d_marginal_pooled_sd",
    effect_size_value = d_article,
    estimate = mean_difference,
    se_estimate = se_difference,
    raw_data_file = data_path,
    raw_variable_names = "trust1; trust2",
    model_formula = "paired t-test: trust2 - trust1",
    contrast_direction = "post-lecture trust minus pre-lecture trust",
    analysis_label = "trust_post_minus_pre",
    statistic_source = "stats::t.test(paired = TRUE) on complete pairs",
    bayesian_input_status = bayes_status_from_test("paired_t_test"),
    extraction_note = paste0(
      "Recovered paired n, t, df, exact p, post-pre mean difference and SE. ",
      "Stored effect size uses marginal pooled-SD d to match article; paired dz = ", round(d_z, 6),
      "; mean pre = ", round(mean_pre, 6), "; mean post = ", round(mean_post, 6), "."
    )
  )

  write_recomputed_results(rows, output_path)
}
