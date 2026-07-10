reproduce_study_44 <- function(
    input_path = "data/raw/study_44/Untitled2.sav",
    output_path = "outputs/reproduced/study_44_recomputed.csv"
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

  raw <- haven::read_sav(input_path)

  required_columns <- c(
    "METADATA", "Group", "V6",
    "OUTCOMES1", "V8", "V10", "V11", "V12", "V13", "V15"
  )

  missing_columns <- setdiff(required_columns, names(raw))
  if (length(missing_columns) > 0) {
    stop(
      "Missing required column(s): ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  as_number <- function(x) {
    suppressWarnings(as.numeric(as.character(x)))
  }

  dat <- raw |>
    dplyr::transmute(
      experiment = as_number(METADATA),
      participant_status_code = as_number(Group),
      condition_code = as_number(V6),

      effective = as_number(OUTCOMES1),
      well_liked = as_number(V8),
      clear_expectations_reversed = as_number(V10),
      helpful = as_number(V11),
      challenging_content = as_number(V12),
      learning_environment = as_number(V13),
      prepared_reversed = as_number(V15)
    ) |>
    dplyr::filter(experiment == 1) |>
    dplyr::mutate(
      participant_status = factor(
        participant_status_code,
        levels = c(1, 2),
        labels = c("student", "faculty")
      ),
      rating_magnitude = factor(
        dplyr::case_when(
          condition_code %in% c(1, 3) ~ "high_rating",
          condition_code %in% c(2, 4) ~ "low_rating",
          TRUE ~ NA_character_
        ),
        levels = c("high_rating", "low_rating")
      ),
      quizzing_comment = factor(
        dplyr::case_when(
          condition_code %in% c(1, 2) ~ "quizzes",
          condition_code %in% c(3, 4) ~ "no_quizzes",
          TRUE ~ NA_character_
        ),
        levels = c("quizzes", "no_quizzes")
      )
    ) |>
    dplyr::select(
      participant_status,
      rating_magnitude,
      quizzing_comment,
      effective,
      well_liked,
      clear_expectations_reversed,
      helpful,
      challenging_content,
      learning_environment,
      prepared_reversed
    ) |>
    stats::na.omit()

  if (nrow(dat) != 213) {
    stop(
      "Unexpected complete-case N for Experiment 1. Expected 213, got ",
      nrow(dat),
      ". The script uses the seven original outcome columns, not the partially missing manually derived columns.",
      call. = FALSE
    )
  }

  dependent_variables <- c(
    "effective",
    "well_liked",
    "clear_expectations_reversed",
    "helpful",
    "challenging_content",
    "learning_environment",
    "prepared_reversed"
  )

  contrasts(dat$participant_status) <- contr.sum(2)
  contrasts(dat$rating_magnitude) <- contr.sum(2)
  contrasts(dat$quizzing_comment) <- contr.sum(2)

  full_model <- stats::lm(
    as.matrix(dat[, dependent_variables]) ~
      participant_status * rating_magnitude * quizzing_comment,
    data = dat
  )

  compute_pillai_type3 <- function(model, term) {
    X <- stats::model.matrix(model)
    B <- stats::coef(model)
    E <- crossprod(stats::residuals(model))

    assign_index <- attr(X, "assign")
    term_labels <- attr(stats::terms(model), "term.labels")
    term_number <- match(term, term_labels)

    if (is.na(term_number)) {
      stop("Term not found in model: ", term, call. = FALSE)
    }

    term_columns <- which(assign_index == term_number)
    if (length(term_columns) == 0) {
      stop("No model-matrix columns found for term: ", term, call. = FALSE)
    }

    L <- diag(ncol(X))[term_columns, , drop = FALSE]
    XtX_inv <- solve(crossprod(X))
    LB <- L %*% B
    middle <- solve(L %*% XtX_inv %*% t(L))
    H <- t(LB) %*% middle %*% LB

    pillai <- sum(diag(H %*% solve(H + E)))

    p <- ncol(stats::residuals(model))
    s <- qr(L)$rank
    df_error <- nrow(X) - qr(X)$rank

    m <- (abs(p - s) - 1) / 2
    n <- (df_error - p - 1) / 2

    df1 <- s * (2 * m + s + 1)
    df2 <- s * (2 * n + s + 1)
    f_value <- ((2 * n + s + 1) / (2 * m + s + 1)) * (pillai / (s - pillai))
    p_value <- stats::pf(f_value, df1, df2, lower.tail = FALSE)

    list(
      pillai = unname(pillai),
      f_value = unname(f_value),
      df1 = unname(df1),
      df2 = unname(df2),
      p_value = unname(p_value),
      df_error = unname(df_error),
      hypothesis_df = unname(s)
    )
  }

  rating_magnitude_test <- compute_pillai_type3(full_model, "rating_magnitude")
  quizzing_comment_test <- compute_pillai_type3(full_model, "quizzing_comment")

  n_student <- sum(dat$participant_status == "student")
  n_faculty <- sum(dat$participant_status == "faculty")
  n_high_rating <- sum(dat$rating_magnitude == "high_rating")
  n_low_rating <- sum(dat$rating_magnitude == "low_rating")
  n_quizzes <- sum(dat$quizzing_comment == "quizzes")
  n_no_quizzes <- sum(dat$quizzing_comment == "no_quizzes")

  results <- tibble::tibble(
    id = c(32, 33),
    study_id = "study_44",
    study_DOI = "10.1177/00986283231199454",
    recomputation_status = "recomputed_from_original_experiment1_columns_type3_pillai_manova",
    stat_test = "manova",
    reported_result = c(
      "F(7, 199) = 12.75, p < .001, eta_p2 = .31",
      "F(7, 199) = 0.65, p = .714, eta_p2 = .02"
    ),
    p_value = c(rating_magnitude_test$p_value, quizzing_comment_test$p_value),
    p_operator = c("<", "="),
    p_sidedness = "omnibus",
    t_value = NA_real_,
    t_df = NA_real_,
    f_value = c(rating_magnitude_test$f_value, quizzing_comment_test$f_value),
    f_df1 = c(rating_magnitude_test$df1, quizzing_comment_test$df1),
    f_df2 = c(rating_magnitude_test$df2, quizzing_comment_test$df2),
    z_value = NA_real_,
    chi2_value = NA_real_,
    chi2_df = NA_real_,
    r_value = NA_real_,
    n1 = c(n_high_rating, n_quizzes),
    n2 = c(n_low_rating, n_no_quizzes),
    n_total = nrow(dat),
    n_eff = NA_real_,
    effect_size_type = "eta_p2",
    effect_size_value = c(rating_magnitude_test$pillai, quizzing_comment_test$pillai),
    estimate = NA_real_,
    se_estimate = NA_real_,
    raw_data_file = input_path,
    raw_variable_names = paste(
      c("METADATA", "Group", "V6", "OUTCOMES1", "V8", "V10", "V11", "V12", "V13", "V15"),
      collapse = "; "
    ),
    model_formula = paste(
      "MANOVA cbind(effective, well_liked, clear_expectations_reversed, helpful,",
      "challenging_content, learning_environment, prepared_reversed) ~",
      "participant_status * rating_magnitude * quizzing_comment,",
      "with sum contrasts and Type-III Pillai tests"
    ),
    contrast_direction = c(
      "main effect of rating magnitude: high ratings versus low ratings",
      "main effect of quizzing comments: comments mention daily quizzes versus comments mention no quizzes"
    ),
    extraction_note = c(
      paste0(
        "Experiment 1 only. The original outcome columns were used directly: OUTCOMES1, V8, V10, V11, V12, V13, V15. ",
        "This avoids the partially missing manually derived variables helpful/challenging in the edited SAV. ",
        "Complete-case N = ", nrow(dat), "; students = ", n_student, "; faculty = ", n_faculty,
        "; high-rating n = ", n_high_rating, "; low-rating n = ", n_low_rating,
        ". Type-III Pillai trace = ", signif(rating_magnitude_test$pillai, 8),
        ", F = ", signif(rating_magnitude_test$f_value, 8),
        ", df = ", rating_magnitude_test$df1, ", ", rating_magnitude_test$df2,
        ", p = ", signif(rating_magnitude_test$p_value, 8), "."
      ),
      paste0(
        "Experiment 1 only. The original outcome columns were used directly: OUTCOMES1, V8, V10, V11, V12, V13, V15. ",
        "This is the article's MANOVA effect for whether comments mentioned daily quizzes. ",
        "Complete-case N = ", nrow(dat), "; quizzes n = ", n_quizzes, "; no-quizzes n = ", n_no_quizzes,
        ". Type-III Pillai trace = ", signif(quizzing_comment_test$pillai, 8),
        ", F = ", signif(quizzing_comment_test$f_value, 8),
        ", df = ", quizzing_comment_test$df1, ", ", quizzing_comment_test$df2,
        ", p = ", signif(quizzing_comment_test$p_value, 8), "."
      )
    )
  )

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(results, output_path)
  return(results)
}
