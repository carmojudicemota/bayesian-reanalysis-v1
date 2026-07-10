# Transparent recomputation for Study 55
# Bates et al. (2024), DOI: 10.1177/14757257241301733
# Target index row: id = 43
# Recomputes the Experiment 1 one-way Welch ANOVA:
# final test score by quiz condition
# using the author file with failed-mastery participants and timed-out participants removed.

reproduce_study_55 <- function(
    input_path = "data/raw/study_55/MQ_Exp_1_data_removed_failed_mastery_Ss_and_timed_out.csv",
    output_path = "outputs/reproduced/study_55_recomputed.csv"
) {
  if (!file.exists(input_path)) {
    stop("Raw data file not found: ", input_path, call. = FALSE)
  }

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

  data <- utils::read.csv(input_path, check.names = FALSE, stringsAsFactors = FALSE)

  required_columns <- c("Condition", "Final Test Score")
  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0) {
    stop(
      "Missing required column(s) in raw data: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  analysis_data <- data[, required_columns]
  names(analysis_data) <- c("condition", "final_test_score")

  analysis_data$condition <- trimws(as.character(analysis_data$condition))
  analysis_data$final_test_score <- suppressWarnings(as.numeric(analysis_data$final_test_score))

  # Match the author's Experiment 1 analysis file:
  # failed-mastery participants and timed-out participants removed.
  # Additional summary columns in the CSV are ignored.
  analysis_data <- analysis_data[stats::complete.cases(analysis_data), , drop = FALSE]
  analysis_data <- analysis_data[analysis_data$condition != "", , drop = FALSE]

  expected_levels <- c("one", "two", "three", "mastery")
  found_levels <- sort(unique(analysis_data$condition))
  if (!all(expected_levels %in% found_levels)) {
    stop(
      "Expected condition levels not found. Found: ",
      paste(found_levels, collapse = "; "),
      call. = FALSE
    )
  }

  analysis_data$condition <- factor(analysis_data$condition, levels = expected_levels)
  analysis_data <- analysis_data[!is.na(analysis_data$condition), , drop = FALSE]

  n_total <- nrow(analysis_data)
  group_counts <- table(analysis_data$condition)
  group_means <- tapply(analysis_data$final_test_score, analysis_data$condition, mean)
  group_sds <- tapply(analysis_data$final_test_score, analysis_data$condition, stats::sd)

  # Welch ANOVA, matching the reported inferential test.
  welch_fit <- stats::oneway.test(final_test_score ~ condition, data = analysis_data, var.equal = FALSE)

  f_value <- unname(welch_fit$statistic)
  f_df1 <- unname(welch_fit$parameter[1])
  f_df2 <- unname(welch_fit$parameter[2])
  p_value <- unname(welch_fit$p.value)

  # The article reports omega squared from the ordinary one-way ANOVA sums of squares,
  # while using Welch's ANOVA for F because Levene's test was significant.
  ordinary_fit <- stats::aov(final_test_score ~ condition, data = analysis_data)
  ordinary_table <- as.data.frame(summary(ordinary_fit)[[1]])
  ss_between <- ordinary_table$`Sum Sq`[1]
  ss_within <- ordinary_table$`Sum Sq`[2]
  df_between <- ordinary_table$Df[1]
  ms_within <- ordinary_table$`Mean Sq`[2]
  ss_total <- ss_between + ss_within
  omega2 <- (ss_between - df_between * ms_within) / (ss_total + ms_within)

  status <- if (
    isTRUE(abs(f_value - 35.64) < 0.05) &&
      isTRUE(abs(f_df1 - 3) < 0.001) &&
      isTRUE(abs(f_df2 - 79.57) < 0.05) &&
      isTRUE(n_total == 154)
  ) {
    "recomputed_from_author_experiment_1_cleaned_data"
  } else {
    "recomputed_but_does_not_match_article"
  }

  group_summary <- paste(
    names(group_counts),
    paste0(
      "n=", as.integer(group_counts),
      ", M=", sprintf("%.6f", as.numeric(group_means)),
      ", SD=", sprintf("%.6f", as.numeric(group_sds))
    ),
    sep = ": ",
    collapse = "; "
  )

  extraction_note <- paste0(
    "Recomputed from the author's Experiment 1 cleaned data file with failed-mastery participants and timed-out participants removed. ",
    "The analysis uses only Condition and Final Test Score; additional summary columns in the CSV are ignored. ",
    "Welch's one-way ANOVA gives F(", sprintf("%.0f", f_df1), ", ", sprintf("%.6f", f_df2), ") = ", sprintf("%.6f", f_value),
    ", p = ", format(p_value, scientific = TRUE, digits = 6), ". ",
    "Omega squared is computed from the ordinary one-way ANOVA sums of squares, matching the article's reported omega2. ",
    "Group summaries: ", group_summary, ". Reported BF10 = 2.06e8 is retained as article metadata but is not recomputed here."
  )

  results <- data.frame(
    id = 43,
    study_id = "study_55",
    study_DOI = "10.1177/14757257241301733",
    recomputation_status = status,
    stat_test = "welch_anova",
    reported_result = "F(3, 79.57) = 35.64, p < .001, omega2 = .273, BF10 = 2.06e8",
    p_value = p_value,
    p_operator = "<",
    p_sidedness = "omnibus",
    t_value = NA_real_,
    t_df = NA_real_,
    f_value = f_value,
    f_df1 = f_df1,
    f_df2 = f_df2,
    z_value = NA_real_,
    chi2_value = NA_real_,
    chi2_df = NA_real_,
    r_value = NA_real_,
    n1 = NA_integer_,
    n2 = NA_integer_,
    n_total = n_total,
    n_eff = NA_real_,
    effect_size_type = "omega2",
    effect_size_value = omega2,
    estimate = NA_real_,
    se_estimate = NA_real_,
    raw_data_file = input_path,
    raw_variable_names = "Condition; Final Test Score",
    model_formula = "Welch one-way ANOVA: Final Test Score ~ Condition, var.equal = FALSE",
    contrast_direction = "Experiment 1 final test performance differs across one quiz, two quizzes, three quizzes, and mastery quizzing",
    extraction_note = extraction_note,
    stringsAsFactors = FALSE
  )

  utils::write.csv(results, output_path, row.names = FALSE, na = "NA")

  message("Wrote: ", output_path)
  invisible(results)
}
