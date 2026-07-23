# R/reproduce/study_06.R
# Study 6: Kelly & Clinton-Lisell. DOI: 10.1177/14757257241295302
# Reconstructs row id 5: PSYC 111 simple effect of social annotation on belonging.

if (!exists("make_recomputed_row")) source("R/reproduce/00_reproduction_helpers.R")

reproduce_study_06 <- function(
    data_path = "data/raw/study_06/Social_Annotation_and_SOB_SOC_Data.sav",
    output_path = "outputs/reproduced/study_06_recomputed.csv"
) {
  check_required_packages(c("haven", "dplyr", "tibble", "readr"))
  data_path <- resolve_existing_file(c(
    data_path,
    "data/raw/study_06/Social_Annotation_and_SOB_SOC_Data.sav",
    "data/raw/study_06/Social Annotation and SOB_SOC Data.sav"
  ), "study_06 SPSS file")

  dat <- haven::read_sav(data_path)
  check_required_columns(dat, c("BCBSTotalScore", "Condition", "Course"), "study_06")

  analysis_data <- tibble::tibble(
    belonging = as.numeric(dat$BCBSTotalScore),
    condition_numeric = as.numeric(dat$Condition),
    course_numeric = as.numeric(dat$Course),
    condition_label = as.character(haven::as_factor(dat$Condition)),
    course_label = as.character(haven::as_factor(dat$Course))
  )
  analysis_data <- analysis_data[stats::complete.cases(analysis_data[, c("belonging", "condition_numeric", "course_numeric")]), ]
  analysis_data$condition_factor <- factor(analysis_data$condition_numeric)
  analysis_data$course_factor <- factor(analysis_data$course_numeric)

  full_model <- stats::lm(belonging ~ condition_factor * course_factor, data = analysis_data)
  full_anova <- stats::anova(full_model)
  residual_mse <- full_anova["Residuals", "Mean Sq"]
  residual_df <- full_anova["Residuals", "Df"]

  psyc111_value <- 1
  psyc111 <- analysis_data[analysis_data$course_numeric == psyc111_value, ]
  if (nrow(psyc111) == 0) stop("No PSYC 111 rows found using Course == 1.", call. = FALSE)

  condition_values <- sort(unique(psyc111$condition_numeric))
  if (length(condition_values) != 2) {
    stop("Expected two condition values inside PSYC 111, found: ", paste(condition_values, collapse = ", "), call. = FALSE)
  }

  condition_labels <- unique(psyc111[, c("condition_numeric", "condition_label")])
  condition_labels <- condition_labels[order(condition_labels$condition_numeric), ]

  individual_value <- condition_labels$condition_numeric[
    grepl("individual|control", condition_labels$condition_label, ignore.case = TRUE)
  ][1]
  social_value <- condition_labels$condition_numeric[
    grepl("social|perusall|annotation", condition_labels$condition_label, ignore.case = TRUE) &
      !grepl("individual", condition_labels$condition_label, ignore.case = TRUE)
  ][1]

  if (is.na(individual_value) || is.na(social_value)) {
    individual_value <- condition_values[1]
    social_value <- condition_values[2]
  }

  individual <- psyc111$belonging[psyc111$condition_numeric == individual_value]
  social <- psyc111$belonging[psyc111$condition_numeric == social_value]

  individual_n <- length(individual)
  social_n <- length(social)
  individual_mean <- mean(individual)
  social_mean <- mean(social)
  individual_sd <- stats::sd(individual)
  social_sd <- stats::sd(social)

  mean_difference <- social_mean - individual_mean
  simple_effect_ss <- mean_difference^2 / (1 / social_n + 1 / individual_n)
  f_value <- simple_effect_ss / residual_mse
  p_value <- stats::pf(f_value, df1 = 1, df2 = residual_df, lower.tail = FALSE)
  eta_p2 <- eta_p2_from_f(f_value, 1, residual_df)
  se_difference <- sqrt(residual_mse * (1 / social_n + 1 / individual_n))

  rows <- make_recomputed_row(
    id = 5,
    study_id = "study_06",
    study_DOI = "10.1177/14757257241295302",
    recomputation_status = "recomputed_from_raw_data_simple_effect",
    stat_test = "factorial_between_anova",
    reported_result = "PSYC 111 simple effect: social annotation > individual annotation; p < .001, eta_p2 = .288",
    reported_p_value = 0.001,
    reported_p_operator = "<",
    reported_p_sidedness = "omnibus",
    reported_effect_size_type = "eta_p2",
    reported_effect_size_value = 0.288,
    p_value = as.numeric(p_value),
    p_operator = "=",
    p_sidedness = "omnibus",
    f_value = as.numeric(f_value),
    f_df1 = 1,
    f_df2 = as.numeric(residual_df),
    n1 = individual_n,
    n2 = social_n,
    n_total = individual_n + social_n,
    n_eff = NA_real_,
    effect_size_type = "eta_p2",
    effect_size_value = as.numeric(eta_p2),
    estimate = as.numeric(mean_difference),
    se_estimate = as.numeric(se_difference),
    raw_data_file = data_path,
    raw_variable_names = "BCBSTotalScore; Condition; Course",
    model_formula = "BCBSTotalScore ~ Condition * Course; simple effect of Condition within Course == 1 using full-model residual MSE",
    contrast_direction = "PSYC 111 social annotation minus PSYC 111 individual annotation",
    analysis_label = "psyc111_social_annotation_vs_individual_annotation_belonging",
    statistic_source = "manual simple-effect F from full factorial ANOVA residual error",
    bayesian_input_status = bayes_status_from_test("factorial_between_anova"),
    extraction_note = paste0(
      "Recovered simple-effect F, df, exact p, eta_p2, cell sizes, mean difference and SE. ",
      "Course value for PSYC 111 = ", psyc111_value,
      "; individual value = ", individual_value, "; social value = ", social_value,
      "; individual n = ", individual_n, "; social n = ", social_n,
      "; individual mean = ", round(individual_mean, 6), "; social mean = ", round(social_mean, 6),
      "; residual MSE = ", round(residual_mse, 6), "; residual df = ", residual_df, "."
    )
  )

  write_recomputed_results(rows, output_path)
}
