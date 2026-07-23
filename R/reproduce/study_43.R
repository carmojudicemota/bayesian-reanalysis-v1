reproduce_study_43 <- function(
    input_path = "data/raw/study_43/Datafile.sav",
    output_path = "outputs/reproduced/study_43_recomputed.csv"
) {
  required_packages <- c("haven", "dplyr", "tibble", "readr")
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

  data <- haven::read_sav(input_path)

  required_columns <- c(
    "Crit_Score_Testing_old",
    "Crit_Score_Testing_New",
    "Crit_Score_Restudy_old",
    "Crit_Score_Restudy_New",
    "Lecture",
    "questiontype_crit"
  )

  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0) {
    stop(
      "Required column(s) missing from raw data: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  analysis_data <- data |>
    dplyr::select(dplyr::all_of(required_columns)) |>
    dplyr::mutate(
      dplyr::across(
        c(
          Crit_Score_Testing_old,
          Crit_Score_Testing_New,
          Crit_Score_Restudy_old,
          Crit_Score_Restudy_New
        ),
        as.numeric
      ),
      Lecture = as.factor(Lecture),
      questiontype_crit = as.factor(questiontype_crit)
    ) |>
    dplyr::filter(
      !is.na(Crit_Score_Testing_old),
      !is.na(Crit_Score_Testing_New),
      !is.na(Crit_Score_Restudy_old),
      !is.na(Crit_Score_Restudy_New),
      !is.na(Lecture),
      !is.na(questiontype_crit)
    ) |>
    dplyr::mutate(
      learning_condition_by_old_new =
        (Crit_Score_Testing_old - Crit_Score_Testing_New) -
        (Crit_Score_Restudy_old - Crit_Score_Restudy_New),
      testing_minus_restudy_reviewed =
        Crit_Score_Testing_old - Crit_Score_Restudy_old,
      testing_minus_restudy_unreviewed =
        Crit_Score_Testing_New - Crit_Score_Restudy_New,
      reviewed_minus_unreviewed =
        ((Crit_Score_Testing_old + Crit_Score_Restudy_old) / 2) -
        ((Crit_Score_Testing_New + Crit_Score_Restudy_New) / 2),
      testing_minus_restudy_overall =
        ((Crit_Score_Testing_old + Crit_Score_Testing_New) / 2) -
        ((Crit_Score_Restudy_old + Crit_Score_Restudy_New) / 2)
    )

  if (nrow(analysis_data) == 0) {
    stop("No complete cases available for the Study_43 target analysis.", call. = FALSE)
  }

  old_contrasts <- options("contrasts")
  on.exit(options(old_contrasts), add = TRUE)
  options(contrasts = c("contr.sum", "contr.poly"))

  extract_intercept_test <- function(outcome) {
    model_formula <- stats::as.formula(
      paste0(outcome, " ~ Lecture + questiontype_crit")
    )

    model_fit <- stats::lm(model_formula, data = analysis_data)
    model_summary <- summary(model_fit)

    t_value <- unname(model_summary$coefficients["(Intercept)", "t value"])
    df_error <- stats::df.residual(model_fit)
    f_value <- t_value^2
    p_value <- stats::pf(f_value, df1 = 1, df2 = df_error, lower.tail = FALSE)
    eta_p2 <- (f_value * 1) / ((f_value * 1) + df_error)

    list(
      t_value = t_value,
      f_value = f_value,
      df_error = df_error,
      p_value = p_value,
      eta_p2 = eta_p2
    )
  }

  target_test <- extract_intercept_test("learning_condition_by_old_new")
  reviewed_followup <- extract_intercept_test("testing_minus_restudy_reviewed")
  unreviewed_followup <- extract_intercept_test("testing_minus_restudy_unreviewed")
  old_new_main <- extract_intercept_test("reviewed_minus_unreviewed")
  learning_condition_main <- extract_intercept_test("testing_minus_restudy_overall")

  n_total <- nrow(analysis_data)
  lecture_counts <- paste(
    names(table(analysis_data$Lecture)),
    as.integer(table(analysis_data$Lecture)),
    sep = "=",
    collapse = "; "
  )
  question_type_counts <- paste(
    names(table(analysis_data$questiontype_crit)),
    as.integer(table(analysis_data$questiontype_crit)),
    sep = "=",
    collapse = "; "
  )

  # Three saved rows, because study_43 supplies THREE distinct published effects and
  # two of them are our claim targets. Previously only the interaction was saved and
  # the two targets existed solely as text inside extraction_note, which left both
  # claims with a blank source_result_id:
  #   id 30 -> learning condition x question type interaction, F(1,64) = 5.18   (not a target)
  #   id 52 -> follow-up testing effect for REVIEWED content, F(1,64) = 5.33    (claim_01, key)
  #   id 53 -> question-type main effect, F(1,64) = 19.09                        (claim_02, second)
  # All three reproduce the article exactly (Glaser & Richter, 2023, pp. 4-5).
  shared_model_formula <- paste(
      "SPSS GLM Crit_Score_Testing_old Crit_Score_Testing_New",
      "Crit_Score_Restudy_old Crit_Score_Restudy_New BY Lecture questiontype_crit",
      "/WSFACTOR = Learning_condition 2 Polynomial Old_vs_New 2 Polynomial",
      "/WSDESIGN = Learning_condition Old_vs_New Learning_condition*Old_vs_New",
      "/DESIGN = Lecture questiontype_crit"
    )

  shared_note <- paste0(
      "Recomputed from the author's SPSS GLM target using complete cases on the four criterion-score variables. ",
      "The target within-subject interaction was reconstructed as the contrast ",
      "(Testing_old - Testing_New) - (Restudy_old - Restudy_New), then tested as the intercept ",
      "in lm(contrast ~ Lecture + questiontype_crit) using sum contrasts, matching the SPSS repeated-measures GLM with Lecture and questiontype_crit as between-subject factors and no Lecture x questiontype_crit term. ",
      "n_total = ", n_total, "; Lecture counts: ", lecture_counts, "; questiontype_crit counts: ", question_type_counts, ". ",
      "Target interaction: F(1, ", target_test$df_error, ") = ", round(target_test$f_value, 6),
      ", p = ", signif(target_test$p_value, 6), ", eta_p2 = ", round(target_test$eta_p2, 6), ". ",
      "Follow-up reviewed/tested content contrast: F(1, ", reviewed_followup$df_error, ") = ", round(reviewed_followup$f_value, 6),
      ", p = ", signif(reviewed_followup$p_value, 6), ", eta_p2 = ", round(reviewed_followup$eta_p2, 6), ". ",
      "Follow-up unreviewed-content contrast: F(1, ", unreviewed_followup$df_error, ") = ", round(unreviewed_followup$f_value, 6),
      ", p = ", signif(unreviewed_followup$p_value, 6), ", eta_p2 = ", round(unreviewed_followup$eta_p2, 6), ". ",
      "Question-type main effect: F(1, ", old_new_main$df_error, ") = ", round(old_new_main$f_value, 6),
      ", p = ", signif(old_new_main$p_value, 6), ", eta_p2 = ", round(old_new_main$eta_p2, 6), ". ",
      "Learning-condition main effect: F(1, ", learning_condition_main$df_error, ") = ", round(learning_condition_main$f_value, 6),
      ", p = ", signif(learning_condition_main$p_value, 6), ", eta_p2 = ", round(learning_condition_main$eta_p2, 6), "."
    )

  make_row_43 <- function(id, test, contrast_var, reported_result,
                          contrast_direction, row_note) {
    tibble::tibble(
      id = id,
      study_id = "study_43",
      study_DOI = "10.1177/00986283231218943",
      recomputation_status = "recomputed_from_author_spss_repeated_measures_glm",
      stat_test = "mixed_anova",
      reported_result = reported_result,
      p_value = test$p_value,
      p_operator = "=",
      p_sidedness = "omnibus",
      t_value = NA_real_, t_df = NA_real_,
      f_value = test$f_value, f_df1 = 1, f_df2 = test$df_error,
      z_value = NA_real_, chi2_value = NA_real_, chi2_df = NA_real_, r_value = NA_real_,
      n1 = NA_real_, n2 = NA_real_,
      n_total = n_total, n_eff = NA_real_,
      effect_size_type = "eta_p2",
      effect_size_value = test$eta_p2,
      estimate = mean(analysis_data[[contrast_var]]),
      se_estimate = stats::sd(analysis_data[[contrast_var]]) / sqrt(n_total),
      raw_data_file = input_path,
      raw_variable_names = paste(required_columns, collapse = "; "),
      model_formula = shared_model_formula,
      contrast_direction = contrast_direction,
      extraction_note = paste0(row_note, " ", shared_note)
    )
  }

  results <- dplyr::bind_rows(
    make_row_43(
      30, target_test, "learning_condition_by_old_new",
      "Learning condition x question type interaction: F(1, 64) = 5.18, p = .026, eta_p2 = .08",
      paste("Testing effect for reviewed/practiced content is larger than the",
            "testing-minus-restudy difference for unreviewed lecture content."),
      "ROW: learning condition x question type interaction. Not a claim target; retained as published context."
    ),
    make_row_43(
      52, reviewed_followup, "testing_minus_restudy_reviewed",
      "Testing effect for reviewed content: F(1, 64) = 5.33, p = .024, eta_p2 = .08",
      paste("Final-test performance on questions referring to TESTED content exceeds",
            "performance on questions referring to RESTUDIED content, within reviewed content."),
      paste("ROW: follow-up simple effect for REVIEWED content -- this is Almuhanna's KEY target",
            "for study_43 (reported F(1,64) = 5.33, p = .024, eta_p2 = .08; marked Fully reproducible).",
            "Article: Glaser & Richter (2023), 'Follow-up tests revealed a testing effect for questions",
            "referring to reviewed content.'")
    ),
    make_row_43(
      53, old_new_main, "reviewed_minus_unreviewed",
      "Question-type main effect: F(1, 64) = 19.09, p < .001, eta_p2 = .23",
      paste("Final-test performance on questions referring to REVIEWED (tested or restudied)",
            "content exceeds performance on questions referring to content NOT reviewed.")
      ,
      paste("ROW: question-type main effect -- this is Almuhanna's SECOND target for study_43",
            "(reported F(1,64) = 19.09, p < .001, eta_p2 = .23; marked Fully reproducible).",
            "Article: 'We found a strong main effect for question type.'")
    )
  )

  if (exists("standardise_recomputed_output", mode = "function")) {
    results <- standardise_recomputed_output(results)
  }

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(results, output_path)

  message("Wrote ", output_path)
  invisible(results)
}
