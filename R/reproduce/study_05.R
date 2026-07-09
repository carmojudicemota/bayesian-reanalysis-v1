# R/reproduce/study_05.R
# Study 5: Hard, Lovett, & Brady (2019)
# DOI: 10.1037/stl0000136
#
# Purpose:
# Recompute the two independent-samples t-tests used in the Bayesian
# reanalysis index for study_5 directly from the raw CSV.
#
# Target rows:
#   id = 3: senior-year 16-item quiz performance
#   id = 4: number of additional psychology courses
#
# Group variable:
#   Speciality == 1: psychology students, including majors/minors
#   Speciality == 0: nonpsychology students
#
# Test:
#   Student independent-samples t-test, two-sided, equal variances.

reproduce_study_05 <- function(
    data_path = "data/raw/study_05/HardLovettBrady_Data_Shared.csv",
    output_path = "outputs/reproduced/study_05_recomputed.csv"
) {
  
  if (!file.exists(data_path)) {
    stop(
      "Missing raw CSV for study_05: ", data_path, "\n",
      "Expected file name: HardLovettBrady_Data_Shared.csv"
    )
  }
  
  dat <- readr::read_csv(
    file = data_path,
    show_col_types = FALSE
  )
  
  required_columns <- c(
    "Speciality",
    "16ItemQuizFollowupPerformance",
    "NumPsychClass"
  )
  
  missing_columns <- setdiff(required_columns, names(dat))
  
  if (length(missing_columns) > 0) {
    stop(
      "study_05 is missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  
  # ============================================================
  # Row 3: senior-year 16-item quiz performance
  # ============================================================
  
  quiz_psychology <- dat$`16ItemQuizFollowupPerformance`[
    dat$Speciality == 1
  ]
  
  quiz_nonpsychology <- dat$`16ItemQuizFollowupPerformance`[
    dat$Speciality == 0
  ]
  
  quiz_psychology <- quiz_psychology[!is.na(quiz_psychology)]
  quiz_nonpsychology <- quiz_nonpsychology[!is.na(quiz_nonpsychology)]
  
  quiz_t_test <- stats::t.test(
    x = quiz_psychology,
    y = quiz_nonpsychology,
    var.equal = TRUE,
    alternative = "two.sided"
  )
  
  quiz_n1 <- length(quiz_psychology)
  quiz_n2 <- length(quiz_nonpsychology)
  
  quiz_mean1 <- mean(quiz_psychology)
  quiz_mean2 <- mean(quiz_nonpsychology)
  
  quiz_sd1 <- stats::sd(quiz_psychology)
  quiz_sd2 <- stats::sd(quiz_nonpsychology)
  
  quiz_pooled_sd <- sqrt(
    ((quiz_n1 - 1) * quiz_sd1^2 + (quiz_n2 - 1) * quiz_sd2^2) /
      (quiz_n1 + quiz_n2 - 2)
  )
  
  quiz_mean_difference <- quiz_mean1 - quiz_mean2
  
  quiz_se_difference <- quiz_pooled_sd * sqrt(
    1 / quiz_n1 + 1 / quiz_n2
  )
  
  quiz_cohens_d <- quiz_mean_difference / quiz_pooled_sd
  
  quiz_n_eff <- (quiz_n1 * quiz_n2) / (quiz_n1 + quiz_n2)
  
  row_3 <- tibble::tibble(
    id = 3,
    study_id = "study_5",
    study_DOI = "10.1037/stl0000136",
    analysis_label = "key_result_senior_quiz_performance",
    stat_test = "independent_t_test",
    reported_result = "t(154) = 3.26, p = .001, d = 0.74",
    
    p_value = as.numeric(quiz_t_test$p.value),
    p_operator = "=",
    p_sidedness = "two_sided",
    
    t_value = as.numeric(quiz_t_test$statistic),
    t_df = as.numeric(quiz_t_test$parameter),
    
    f_value = NA_real_,
    f_df1 = NA_real_,
    f_df2 = NA_real_,
    
    z_value = NA_real_,
    
    chi2_value = NA_real_,
    chi2_df = NA_real_,
    
    r_value = NA_real_,
    
    n1 = quiz_n1,
    n2 = quiz_n2,
    n_total = quiz_n1 + quiz_n2,
    n_eff = quiz_n_eff,
    
    effect_size_type = "cohens_d",
    effect_size_value = quiz_cohens_d,
    
    estimate = quiz_mean_difference,
    se_estimate = quiz_se_difference,
    
    statistic_source = "recomputed_from_raw_data",
    
    notes = paste0(
      "Recomputed from raw CSV using Student independent-samples t-test. ",
      "Outcome: senior-year 16-item quiz performance. ",
      "Group variable: Speciality; 1 = psychology students, 0 = nonpsychology students. ",
      "Psychology students n1 = ", quiz_n1, "; nonpsychology students n2 = ", quiz_n2, ". ",
      "Mean psychology = ", round(quiz_mean1, 6), "; mean nonpsychology = ", round(quiz_mean2, 6), ". ",
      "SD psychology = ", round(quiz_sd1, 6), "; SD nonpsychology = ", round(quiz_sd2, 6), "."
    )
  )
  
  # ============================================================
  # Row 4: number of additional psychology courses
  # ============================================================
  
  courses_psychology <- dat$NumPsychClass[
    dat$Speciality == 1
  ]
  
  courses_nonpsychology <- dat$NumPsychClass[
    dat$Speciality == 0
  ]
  
  courses_psychology <- courses_psychology[!is.na(courses_psychology)]
  courses_nonpsychology <- courses_nonpsychology[!is.na(courses_nonpsychology)]
  
  courses_t_test <- stats::t.test(
    x = courses_psychology,
    y = courses_nonpsychology,
    var.equal = TRUE,
    alternative = "two.sided"
  )
  
  courses_n1 <- length(courses_psychology)
  courses_n2 <- length(courses_nonpsychology)
  
  courses_mean1 <- mean(courses_psychology)
  courses_mean2 <- mean(courses_nonpsychology)
  
  courses_sd1 <- stats::sd(courses_psychology)
  courses_sd2 <- stats::sd(courses_nonpsychology)
  
  courses_pooled_sd <- sqrt(
    ((courses_n1 - 1) * courses_sd1^2 + (courses_n2 - 1) * courses_sd2^2) /
      (courses_n1 + courses_n2 - 2)
  )
  
  courses_mean_difference <- courses_mean1 - courses_mean2
  
  courses_se_difference <- courses_pooled_sd * sqrt(
    1 / courses_n1 + 1 / courses_n2
  )
  
  courses_cohens_d <- courses_mean_difference / courses_pooled_sd
  
  courses_n_eff <- (courses_n1 * courses_n2) / (courses_n1 + courses_n2)
  
  row_4 <- tibble::tibble(
    id = 4,
    study_id = "study_5",
    study_DOI = "10.1037/stl0000136",
    analysis_label = "second_result_number_of_additional_psychology_courses",
    stat_test = "independent_t_test",
    reported_result = "t(154) = 16.06, p = .001, d = 3.63",
    
    p_value = as.numeric(courses_t_test$p.value),
    p_operator = "=",
    p_sidedness = "two_sided",
    
    t_value = as.numeric(courses_t_test$statistic),
    t_df = as.numeric(courses_t_test$parameter),
    
    f_value = NA_real_,
    f_df1 = NA_real_,
    f_df2 = NA_real_,
    
    z_value = NA_real_,
    
    chi2_value = NA_real_,
    chi2_df = NA_real_,
    
    r_value = NA_real_,
    
    n1 = courses_n1,
    n2 = courses_n2,
    n_total = courses_n1 + courses_n2,
    n_eff = courses_n_eff,
    
    effect_size_type = "cohens_d",
    effect_size_value = courses_cohens_d,
    
    estimate = courses_mean_difference,
    se_estimate = courses_se_difference,
    
    statistic_source = "recomputed_from_raw_data",
    
    notes = paste0(
      "Recomputed from raw CSV using Student independent-samples t-test. ",
      "Outcome: number of additional psychology courses. ",
      "Group variable: Speciality; 1 = psychology students, 0 = nonpsychology students. ",
      "Psychology students n1 = ", courses_n1, "; nonpsychology students n2 = ", courses_n2, ". ",
      "Mean psychology = ", round(courses_mean1, 6), "; mean nonpsychology = ", round(courses_mean2, 6), ". ",
      "SD psychology = ", round(courses_sd1, 6), "; SD nonpsychology = ", round(courses_sd2, 6), "."
    )
  )
  
  # ============================================================
  # Save standardised recomputed rows
  # ============================================================
  
  rows <- dplyr::bind_rows(
    row_3,
    row_4
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
