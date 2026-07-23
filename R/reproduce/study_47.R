# Transparent recomputation for Study 47
# Boysen & Osgood (2024), DOI: 10.1177/00986283231226187
# Target index rows: id = 35 and id = 36
# Recomputes the SPSS mixed GLM:
# GLM PercentRember PercentCorrectMC PercentCorrectOE BY Condition
#   /WSFACTOR=factor1 3 Polynomial
#   /METHOD=SSTYPE(3)
#   /PRINT=DESCRIPTIVE ETASQ
#   /WSDESIGN=factor1
#   /DESIGN=Condition.

reproduce_study_47 <- function(
    input_path = "data/raw/study_47/Outside_Assistance_Dataset.sav",
    output_path = "outputs/reproduced/study_47_recomputed.csv"
) {
  required_packages <- c("haven")
  missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_packages) > 0) {
    stop(
      "Install required package(s) before running this script: ",
      paste(missing_packages, collapse = ", "),
      call. = FALSE
    )
  }

  if (!file.exists(input_path)) {
    stop("Raw data file not found: ", input_path, call. = FALSE)
  }

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

  data <- as.data.frame(haven::read_sav(input_path))

  required_columns <- c("Condition", "PercentRember", "PercentCorrectMC", "PercentCorrectOE")
  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0) {
    stop(
      "Missing required column(s) in raw data: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  prediction_vars <- c("PercentRember", "PercentCorrectMC", "PercentCorrectOE")

  analysis_data <- data[, c("Condition", prediction_vars)]

  # Match SPSS GLM listwise deletion for the repeated-measures model.
  analysis_data <- analysis_data[stats::complete.cases(analysis_data), , drop = FALSE]

  for (var in prediction_vars) {
    analysis_data[[var]] <- as.numeric(analysis_data[[var]])
  }

  if (inherits(analysis_data$Condition, "haven_labelled")) {
    analysis_data$Condition <- haven::as_factor(analysis_data$Condition, levels = "values")
  } else {
    analysis_data$Condition <- as.factor(analysis_data$Condition)
  }

  analysis_data$Condition <- droplevels(analysis_data$Condition)

  if (nlevels(analysis_data$Condition) != 2) {
    stop("Expected Condition to have exactly two levels after listwise deletion.", call. = FALSE)
  }

  n_total <- nrow(analysis_data)
  group_counts <- table(analysis_data$Condition)
  n_condition_1 <- as.integer(group_counts[1])
  n_condition_2 <- as.integer(group_counts[2])

  # --------------------------------------------------------------------------
  # Between-subjects main effect of Condition.
  # In the SPSS repeated-measures GLM, the Condition effect is equivalent to a
  # one-way ANOVA on each participant's mean across the three prediction scores.
  # --------------------------------------------------------------------------
  analysis_data$row_mean <- rowMeans(analysis_data[, prediction_vars, drop = FALSE])
  between_fit <- stats::lm(row_mean ~ Condition, data = analysis_data)
  between_table <- as.data.frame(stats::anova(between_fit))

  condition_row <- which(rownames(between_table) == "Condition")
  if (length(condition_row) != 1) {
    stop("Could not extract Condition row from between-subjects ANOVA.", call. = FALSE)
  }

  f_condition <- unname(between_table$`F value`[condition_row])
  df1_condition <- unname(between_table$Df[condition_row])
  df2_condition <- unname(between_table$Df[which(rownames(between_table) == "Residuals")])
  p_condition <- unname(between_table$`Pr(>F)`[condition_row])
  eta_condition <- (f_condition * df1_condition) / ((f_condition * df1_condition) + df2_condition)

  t_condition <- sqrt(f_condition)
  d_condition <- t_condition * sqrt(1 / n_condition_1 + 1 / n_condition_2)

  # --------------------------------------------------------------------------
  # Within-subject factor1 x Condition interaction.
  # SPSS reports the uncorrected/sphericity-assumed interaction:
  # F(2, 238) = 2.90, p = .057, eta_p2 = .024.
  # A fixed participant term absorbs all between-subject differences, leaving
  # the repeated-measures residual for factor1 and factor1:Condition.
  # --------------------------------------------------------------------------
  long_data <- data.frame(
    participant = factor(rep(seq_len(n_total), times = length(prediction_vars))),
    Condition = rep(analysis_data$Condition, times = length(prediction_vars)),
    factor1 = factor(
      rep(prediction_vars, each = n_total),
      levels = prediction_vars
    ),
    score = as.numeric(unlist(analysis_data[, prediction_vars], use.names = FALSE)),
    stringsAsFactors = FALSE
  )

  within_fit <- stats::lm(score ~ participant + factor1 + Condition:factor1, data = long_data)
  within_table <- as.data.frame(stats::anova(within_fit))

  interaction_row <- grep("Condition:factor1|factor1:Condition", rownames(within_table))
  if (length(interaction_row) != 1) {
    stop("Could not extract Condition x factor1 interaction from within-subjects ANOVA.", call. = FALSE)
  }

  residual_row <- which(rownames(within_table) == "Residuals")
  f_interaction <- unname(within_table$`F value`[interaction_row])
  df1_interaction <- unname(within_table$Df[interaction_row])
  df2_interaction <- unname(within_table$Df[residual_row])
  p_interaction <- unname(within_table$`Pr(>F)`[interaction_row])
  eta_interaction <- (f_interaction * df1_interaction) / ((f_interaction * df1_interaction) + df2_interaction)

  status_condition <- if (
    isTRUE(abs(f_condition - 4.98) < 0.05) &&
      isTRUE(df1_condition == 1) &&
      isTRUE(df2_condition == 119)
  ) {
    "recomputed_from_author_spss_glm"
  } else {
    "recomputed_but_does_not_match_article"
  }

  status_interaction <- if (
    isTRUE(abs(f_interaction - 2.90) < 0.05) &&
      isTRUE(df1_interaction == 2) &&
      isTRUE(df2_interaction == 238)
  ) {
    "recomputed_from_author_spss_glm"
  } else {
    "recomputed_but_does_not_match_article"
  }

  condition_levels <- paste(levels(analysis_data$Condition), collapse = "; ")

  note_common <- paste0(
    "Recomputed from the author's SPSS mixed GLM using listwise-complete cases on Condition, ",
    paste(prediction_vars, collapse = ", "),
    ". Within-subject levels are ordered as in the SPSS syntax: PercentRember, PercentCorrectMC, PercentCorrectOE. ",
    "n_total = ", n_total,
    "; Condition levels = ", condition_levels,
    "; group counts = ", paste(names(group_counts), as.integer(group_counts), sep = ":", collapse = "; "), "."
  )

  results <- data.frame(
    id = c(35, 36),
    study_id = c("study_47", "study_47"),
    study_DOI = c("10.1177/00986283231226187", "10.1177/00986283231226187"),
    recomputation_status = c(status_condition, status_interaction),
    stat_test = c("independent_t_test", "mixed_anova"),
    reported_result = c(
      "Between-subjects Condition effect: F(1,119)=4.98 = t(119)^2; t(119)=2.23, p=.027 (equal-variance independent t on each subject's mean of the three prediction scores)",
      "F(2, 238) = 2.90, p = .057, eta_p2 = .024"
    ),
    p_value = c(p_condition, p_interaction),
    p_operator = c("=", "="),
    p_sidedness = c("two_sided", "omnibus"),
    t_value = c(t_condition, NA_real_),
    t_df = c(df2_condition, NA_real_),
    f_value = c(NA_real_, f_interaction),
    f_df1 = c(NA_real_, df1_interaction),
    f_df2 = c(NA_real_, df2_interaction),
    z_value = c(NA_real_, NA_real_),
    chi2_value = c(NA_real_, NA_real_),
    chi2_df = c(NA_real_, NA_real_),
    r_value = c(NA_real_, NA_real_),
    n1 = c(n_condition_1, n_condition_1),
    n2 = c(n_condition_2, n_condition_2),
    n_total = c(n_total, n_total),
    n_eff = c(NA_real_, NA_real_),
    effect_size_type = c("cohens_d_pooled", "eta_p2"),
    effect_size_value = c(d_condition, eta_interaction),
    estimate = c(NA_real_, NA_real_),
    se_estimate = c(NA_real_, NA_real_),
    raw_data_file = c(input_path, input_path),
    raw_variable_names = c(
      "Condition; PercentRember; PercentCorrectMC; PercentCorrectOE",
      "Condition; PercentRember; PercentCorrectMC; PercentCorrectOE"
    ),
    model_formula = c(
      "SPSS GLM PercentRember PercentCorrectMC PercentCorrectOE BY Condition /WSFACTOR=factor1 3 Polynomial /WSDESIGN=factor1 /DESIGN=Condition",
      "SPSS GLM PercentRember PercentCorrectMC PercentCorrectOE BY Condition /WSFACTOR=factor1 3 Polynomial /WSDESIGN=factor1 /DESIGN=Condition"
    ),
    contrast_direction = c(
      "between-subjects main effect of testing condition on average predicted future performance",
      "testing condition by predicted-performance format interaction across free recall, multiple-choice, and short-answer predictions"
    ),
    extraction_note = c(
      paste0(note_common, " Row 35 is the between-subjects Condition effect (Tests of Between-Subjects Effects). ",
             "WAVE 1: this effect is exactly an equal-variance independent-samples t-test of Condition on each ",
             "subject's mean of the three prediction scores. F(1,119)=t(119)^2, df 119 = ", n_total, "-2, so ",
             "t = sqrt(F) = ", signif(t_condition, 7), ", Cohen's d(pooled) = ", signif(d_condition, 7),
             ", p = ", signif(p_condition, 6), " (two-sided, matching the non-directional published F). ",
             "Stored as independent_t_test so the Wave-1 paired/independent-t Bayes-factor procedure applies."),
      paste0(note_common, " Row 36 extracts the factor1 x Condition effect from Tests of Within-Subjects Effects, sphericity assumed.")
    ),
    stringsAsFactors = FALSE
  )

  utils::write.csv(results, output_path, row.names = FALSE, na = "NA")

  message("Wrote: ", output_path)
  invisible(results)
}
