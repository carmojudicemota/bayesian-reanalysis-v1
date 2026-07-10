# R/reproduce/study_03.R
# Study 03: Wickline, Ford, Gurung, & Appleby (2025), DOI 10.1037/stl0000432.
# Targeted reconstruction of the Type-III 2 x 2 MANOVA reported for syllabus
# snapshot and instructor snapshot effects on three perception outcomes.
#
# The important detail is that the article/SPSS analysis uses Type-III tests in
# an unbalanced 2 x 2 design. Base R's stats::manova() gives sequential tests by
# default, so this script explicitly reconstructs the Type-III Wilks tests using
# effect-coded factors and the multivariate linear model matrices.

reproduce_study_03 <- function(
    input_path = NULL,
    output_path = "outputs/reproduced/study_03_recomputed.csv"
) {
  required_packages <- c("haven", "dplyr", "tibble", "readr")
  missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_packages) > 0) {
    stop(
      "Install required package(s) before running this script: ",
      paste(missing_packages, collapse = ", "),
      "\nUse install.packages(c(",
      paste(sprintf('"%s"', missing_packages), collapse = ", "),
      "))",
      call. = FALSE
    )
  }

  if (is.null(input_path)) {
    candidate_paths <- c(
      "data/raw/study_03/Snapshot_MASTER_noID_cleaned_03.11.24.sav",
      "data/raw/study_03/Snapshot MASTER_noID cleaned 03.11.24.sav"
    )
    existing <- candidate_paths[file.exists(candidate_paths)]
    if (length(existing) == 0) {
      stop(
        "Could not find Study 03 raw data. Tried:\n  ",
        paste(candidate_paths, collapse = "\n  "),
        call. = FALSE
      )
    }
    input_path <- existing[[1]]
  }

  raw <- haven::read_sav(input_path)

  required_columns <- c("TBC_COMP", "TBC_CARE", "percept", "snap_syll", "snap_inst")
  missing_columns <- setdiff(required_columns, names(raw))
  if (length(missing_columns) > 0) {
    stop(
      "Study 03 is missing required column(s): ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  as_numeric_clean <- function(x) {
    x <- haven::zap_labels(x)
    if (is.factor(x)) x <- as.character(x)
    suppressWarnings(as.numeric(x))
  }

  dat <- tibble::tibble(
    TBC_COMP = as_numeric_clean(raw$TBC_COMP),
    TBC_CARE = as_numeric_clean(raw$TBC_CARE),
    percept = as_numeric_clean(raw$percept),
    snap_syll = as_numeric_clean(raw$snap_syll),
    snap_inst = as_numeric_clean(raw$snap_inst)
  ) |>
    dplyr::filter(stats::complete.cases(TBC_COMP, TBC_CARE, percept, snap_syll, snap_inst))

  if (!all(dat$snap_syll %in% c(0, 1)) || !all(dat$snap_inst %in% c(0, 1))) {
    stop("snap_syll and snap_inst must be coded 0/1 for this reconstruction.", call. = FALSE)
  }

  # Effect coding is essential for reproducing SPSS Type-III tests in this
  # unbalanced 2 x 2 design. Coding is: absent = -1, present = +1.
  syllabus_effect <- ifelse(dat$snap_syll == 1, 1, -1)
  instructor_effect <- ifelse(dat$snap_inst == 1, 1, -1)

  X <- cbind(
    intercept = 1,
    syllabus_snapshot = syllabus_effect,
    instructor_snapshot = instructor_effect,
    syllabus_x_instructor = syllabus_effect * instructor_effect
  )

  Y <- as.matrix(dat[, c("TBC_COMP", "TBC_CARE", "percept")])
  colnames(Y) <- c("TBC_COMP", "TBC_CARE", "percept")

  xtx_inv <- solve(crossprod(X))
  coefficients <- xtx_inv %*% crossprod(X, Y)
  residuals <- Y - X %*% coefficients
  error_sscp <- crossprod(residuals)

  error_df <- nrow(Y) - qr(X)$rank
  n_outcomes <- ncol(Y)

  compute_type3_wilks <- function(coefficient_name, label) {
    coefficient_index <- match(coefficient_name, colnames(X))
    if (is.na(coefficient_index)) {
      stop("Could not find coefficient for ", label, call. = FALSE)
    }

    L <- matrix(0, nrow = 1, ncol = ncol(X))
    L[1, coefficient_index] <- 1

    hypothesis_sscp <- t(L %*% coefficients) %*%
      solve(L %*% xtx_inv %*% t(L)) %*%
      (L %*% coefficients)

    wilks_lambda <- as.numeric(det(error_sscp) / det(error_sscp + hypothesis_sscp))

    # For a 1-df hypothesis with p multivariate outcomes, Wilks' Lambda has the
    # exact F transformation used here: F(p, error_df - p + 1).
    f_df1 <- n_outcomes
    f_df2 <- error_df - n_outcomes + 1
    f_value <- ((1 - wilks_lambda) / wilks_lambda) * (f_df2 / f_df1)
    p_value <- stats::pf(f_value, f_df1, f_df2, lower.tail = FALSE)

    list(
      wilks_lambda = wilks_lambda,
      f_value = f_value,
      f_df1 = f_df1,
      f_df2 = f_df2,
      p_value = p_value,
      effect_size_value = 1 - wilks_lambda
    )
  }

  interaction_test <- compute_type3_wilks(
    coefficient_name = "syllabus_x_instructor",
    label = "syllabus snapshot x instructor snapshot interaction"
  )

  syllabus_test <- compute_type3_wilks(
    coefficient_name = "syllabus_snapshot",
    label = "syllabus snapshot main effect"
  )

  group_counts <- dat |>
    dplyr::count(snap_syll, snap_inst, name = "n") |>
    dplyr::arrange(snap_syll, snap_inst)

  group_counts_text <- paste(
    paste0(
      "snap_syll=", group_counts$snap_syll,
      ", snap_inst=", group_counts$snap_inst,
      ", n=", group_counts$n
    ),
    collapse = "; "
  )

  row1 <- tibble::tibble(
    id = 1,
    study_id = "study_3",
    study_DOI = "10.1037/stl0000432",
    recomputation_status = "recomputed_from_raw_data_type_III_manova",
    stat_test = "manova",
    reported_result = "F(3, 164) = 2.14, p = .10, Wilks' lambda = .96, eta_p2 = .04",
    reported_p_value = 0.10,
    reported_p_operator = "=",
    reported_p_sidedness = "omnibus",
    reported_effect_size_type = "eta_p2",
    reported_effect_size_value = 0.04,
    p_value = interaction_test$p_value,
    p_operator = "=",
    p_sidedness = "omnibus",
    t_value = NA_real_,
    t_df = NA_real_,
    f_value = interaction_test$f_value,
    f_df1 = interaction_test$f_df1,
    f_df2 = interaction_test$f_df2,
    z_value = NA_real_,
    chi2_value = NA_real_,
    chi2_df = NA_real_,
    r_value = NA_real_,
    n1 = NA_real_,
    n2 = NA_real_,
    n_total = nrow(dat),
    n_eff = nrow(dat),
    effect_size_type = "eta_p2_from_wilks",
    effect_size_value = interaction_test$effect_size_value,
    estimate = interaction_test$wilks_lambda,
    se_estimate = NA_real_,
    raw_data_file = basename(input_path),
    raw_variable_names = "TBC_COMP; TBC_CARE; percept; snap_syll; snap_inst",
    model_formula = "cbind(TBC_COMP, TBC_CARE, percept) ~ snap_syll * snap_inst, Type-III MANOVA with effect-coded factors",
    contrast_direction = "Whether the effect of syllabus snapshot presence differs depending on instructor snapshot presence across the three perception outcomes.",
    analysis_label = "Syllabus snapshot x instructor snapshot interaction on perception outcomes",
    statistic_source = "Manual Type-III multivariate linear model reconstruction; Wilks exact F transformation for one-df effect",
    bayesian_input_status = "ready_manova_diagnostic_not_direct_jzs",
    extraction_note = paste0(
      "Complete-case N = ", nrow(dat), ". Cell counts: ", group_counts_text,
      ". The estimate column stores Wilks' lambda; eta_p2_from_wilks is 1 - Wilks' lambda."
    ),
    wilks_lambda = interaction_test$wilks_lambda
  )

  row2 <- tibble::tibble(
    id = 2,
    study_id = "study_3",
    study_DOI = "10.1037/stl0000432",
    recomputation_status = "recomputed_from_raw_data_type_III_manova",
    stat_test = "manova",
    reported_result = "F(3, 164) = 0.78, p = .51, Wilks' lambda = .99, eta_p2 = .01",
    reported_p_value = 0.51,
    reported_p_operator = "=",
    reported_p_sidedness = "omnibus",
    reported_effect_size_type = "eta_p2",
    reported_effect_size_value = 0.01,
    p_value = syllabus_test$p_value,
    p_operator = "=",
    p_sidedness = "omnibus",
    t_value = NA_real_,
    t_df = NA_real_,
    f_value = syllabus_test$f_value,
    f_df1 = syllabus_test$f_df1,
    f_df2 = syllabus_test$f_df2,
    z_value = NA_real_,
    chi2_value = NA_real_,
    chi2_df = NA_real_,
    r_value = NA_real_,
    n1 = NA_real_,
    n2 = NA_real_,
    n_total = nrow(dat),
    n_eff = nrow(dat),
    effect_size_type = "eta_p2_from_wilks",
    effect_size_value = syllabus_test$effect_size_value,
    estimate = syllabus_test$wilks_lambda,
    se_estimate = NA_real_,
    raw_data_file = basename(input_path),
    raw_variable_names = "TBC_COMP; TBC_CARE; percept; snap_syll; snap_inst",
    model_formula = "cbind(TBC_COMP, TBC_CARE, percept) ~ snap_syll * snap_inst, Type-III MANOVA with effect-coded factors",
    contrast_direction = "Main effect of syllabus snapshot presence across teacher competence, teacher care, and general course/instructor perceptions.",
    analysis_label = "Syllabus snapshot main effect on perception outcomes",
    statistic_source = "Manual Type-III multivariate linear model reconstruction; Wilks exact F transformation for one-df effect",
    bayesian_input_status = "ready_manova_diagnostic_not_direct_jzs",
    extraction_note = paste0(
      "Complete-case N = ", nrow(dat), ". Cell counts: ", group_counts_text,
      ". The estimate column stores Wilks' lambda; eta_p2_from_wilks is 1 - Wilks' lambda."
    ),
    wilks_lambda = syllabus_test$wilks_lambda
  )

  results <- dplyr::bind_rows(row1, row2)

  if (exists("standardise_recomputed_output", mode = "function")) {
    results <- standardise_recomputed_output(results)
  }

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(results, output_path)

  message("Wrote Study 03 recomputed results to: ", output_path)
  invisible(results)
}
