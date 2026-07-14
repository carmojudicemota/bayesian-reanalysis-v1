# Study 35 transparent reconstruction
# Ekuni, Macacare, & Pompeia (2022), DOI: 10.1037/stl0000314
# Target row: id 22
# Reconstructs the delayed-recall repeated-measures GLM for Hypothesis 2.
#
# Important reconstruction note:
# The article reports F(2, 116) = 4.22, p < .02, eta_p2 = .07.
# The shared data reproduce the article's condition means exactly, but the inferential
# statistic obtained from the uploaded author data and syntax variables is larger.
# This script therefore stores the recomputed statistic and explicitly flags the mismatch;
# it does not force the published value.

reproduce_study_35 <- function(
    input_path = "data/raw/study_35/Untitled3.sav",
    output_path = "outputs/reproduced/study_35_recomputed.csv"
) {
  required_packages <- c("haven", "tibble", "readr")
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

  raw_data <- haven::read_sav(input_path, user_na = FALSE)

  required_columns <- c(
    "Participant",
    "Sex",
    "CORRECT_ANSWERS_RR",
    "CORRECT_ANSWERS_MMRP_A",
    "CORRECT_ANSWERS_RP_A"
  )

  missing_columns <- setdiff(required_columns, names(raw_data))

  if (length(missing_columns) > 0) {
    stop(
      "Study_35 is missing required delayed-recall column(s): ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  # Article condition mapping for Hypothesis 2:
  # RR   = multitask + reread, lowest delayed recall
  # MMRP = multitask + retrieval practice
  # RP   = no multitask + retrieval practice
  # The source variables are numbers correct out of 7. The article reports percentages,
  # so we convert to percent. This conversion does not change F or eta_p2.
  dat <- data.frame(
    Participant = as.character(raw_data[["Participant"]]),
    Sex = as.factor(as.character(raw_data[["Sex"]])),
    RR = as.numeric(haven::zap_labels(haven::zap_missing(raw_data[["CORRECT_ANSWERS_RR"]]))) * 100 / 7,
    MMRP = as.numeric(haven::zap_labels(haven::zap_missing(raw_data[["CORRECT_ANSWERS_MMRP_A"]]))) * 100 / 7,
    RP = as.numeric(haven::zap_labels(haven::zap_missing(raw_data[["CORRECT_ANSWERS_RP_A"]]))) * 100 / 7
  )

  dat <- stats::na.omit(dat)

  if (nrow(dat) < 3) {
    stop("Study_35 has too few complete cases for the repeated-measures GLM.", call. = FALSE)
  }

  if (length(unique(dat$Sex)) < 2) {
    stop("Study_35 requires the Sex column to reproduce the article's sex-controlled df.", call. = FALSE)
  }

  # Manual repeated-measures ANOVA with Sex as a between-subject factor.
  # This mirrors the article's description that the 3-level within-participant GLM
  # was controlled for participants' sex, producing df2 = (N - number_of_sex_groups) * 2.
  compute_rm_anova_with_between_factor <- function(wide_scores, between_factor) {
    Y <- as.matrix(wide_scores)
    storage.mode(Y) <- "double"

    n <- nrow(Y)
    k <- ncol(Y)
    groups <- unique(between_factor)
    g <- length(groups)

    grand_mean <- mean(Y)
    condition_means <- colMeans(Y)

    ss_condition <- n * sum((condition_means - grand_mean)^2)
    ss_condition_by_group <- 0
    ss_error_condition <- 0

    for (this_group in groups) {
      group_index <- between_factor == this_group
      Y_group <- Y[group_index, , drop = FALSE]
      n_group <- nrow(Y_group)

      group_mean <- mean(Y_group)
      group_condition_means <- colMeans(Y_group)
      group_subject_means <- rowMeans(Y_group)

      ss_condition_by_group <- ss_condition_by_group +
        n_group * sum((group_condition_means - group_mean - condition_means + grand_mean)^2)

      ss_error_condition <- ss_error_condition +
        sum((Y_group - group_subject_means - matrix(group_condition_means, nrow = n_group, ncol = k, byrow = TRUE) + group_mean)^2)
    }

    df_condition <- k - 1
    df_error_condition <- (n - g) * (k - 1)

    ms_condition <- ss_condition / df_condition
    ms_error_condition <- ss_error_condition / df_error_condition

    f_value <- ms_condition / ms_error_condition
    p_value <- stats::pf(f_value, df_condition, df_error_condition, lower.tail = FALSE)
    eta_p2 <- ss_condition / (ss_condition + ss_error_condition)

    list(
      n = n,
      number_of_groups = g,
      ss_condition = ss_condition,
      ss_condition_by_group = ss_condition_by_group,
      ss_error_condition = ss_error_condition,
      df_condition = df_condition,
      df_error_condition = df_error_condition,
      f_value = f_value,
      p_value = p_value,
      eta_p2 = eta_p2
    )
  }

  # Also compute the unadjusted one-way repeated-measures value because Ali's uploaded
  # syntax does not include Sex in the GLM. This is kept only in the note.
  compute_rm_anova_unadjusted <- function(wide_scores) {
    Y <- as.matrix(wide_scores)
    storage.mode(Y) <- "double"

    n <- nrow(Y)
    k <- ncol(Y)
    grand_mean <- mean(Y)
    condition_means <- colMeans(Y)
    subject_means <- rowMeans(Y)

    ss_condition <- n * sum((condition_means - grand_mean)^2)
    ss_error <- sum((Y - subject_means - matrix(condition_means, nrow = n, ncol = k, byrow = TRUE) + grand_mean)^2)

    df_condition <- k - 1
    df_error <- (n - 1) * (k - 1)
    f_value <- (ss_condition / df_condition) / (ss_error / df_error)
    p_value <- stats::pf(f_value, df_condition, df_error, lower.tail = FALSE)
    eta_p2 <- ss_condition / (ss_condition + ss_error)

    list(
      n = n,
      df_condition = df_condition,
      df_error = df_error,
      f_value = f_value,
      p_value = p_value,
      eta_p2 = eta_p2
    )
  }

  wide_scores <- dat[, c("RR", "MMRP", "RP")]

  sex_controlled <- compute_rm_anova_with_between_factor(
    wide_scores = wide_scores,
    between_factor = dat$Sex
  )

  unadjusted <- compute_rm_anova_unadjusted(wide_scores = wide_scores)

  condition_means <- vapply(wide_scores, mean, numeric(1))
  condition_sds <- vapply(wide_scores, stats::sd, numeric(1))

  # Article target. The means match exactly, but the inferential statistic from the
  # uploaded author data/syntax variables does not. Keep the recomputed value.
  matches_reported_f <- isTRUE(abs(sex_controlled$f_value - 4.22) < 0.03)
  matches_reported_eta <- isTRUE(abs(sex_controlled$eta_p2 - 0.07) < 0.01)

  recomputation_status <- if (matches_reported_f && matches_reported_eta) {
    "recomputed_from_author_delayed_recall_variables"
  } else {
    "recomputed_from_author_delayed_recall_variables_but_does_not_match_article_F"
  }

  results <- tibble::tibble(
    id = 22,
    study_id = "study_35",
    study_DOI = "10.1037/stl0000314",
    recomputation_status = recomputation_status,

    stat_test = "mixed_anova",
    reported_result = "F(2, 116) = 4.22, p < .02, eta_p2 = .07",

    p_value = sex_controlled$p_value,
    p_operator = "=",
    p_sidedness = "omnibus",

    t_value = NA_real_,
    t_df = NA_real_,
    f_value = sex_controlled$f_value,
    f_df1 = sex_controlled$df_condition,
    f_df2 = sex_controlled$df_error_condition,
    z_value = NA_real_,
    chi2_value = NA_real_,
    chi2_df = NA_real_,
    r_value = NA_real_,

    n1 = NA_real_,
    n2 = NA_real_,
    n_total = sex_controlled$n,
    n_eff = sex_controlled$n,

    effect_size_type = "eta_p2",
    effect_size_value = sex_controlled$eta_p2,

    estimate = NA_real_,
    se_estimate = NA_real_,

    raw_data_file = input_path,
    raw_variable_names = "CORRECT_ANSWERS_RR; CORRECT_ANSWERS_MMRP_A; CORRECT_ANSWERS_RP_A; Sex",
    model_formula = "sex-controlled repeated-measures GLM: delayed_recall_percent ~ condition + condition:Sex error term; condition order = RR, MMRP, RP",
    contrast_direction = "delayed recall differs across multitask + reread, multitask + retrieval practice, and no multitask + retrieval practice",
    extraction_note = paste0(
      "The article's Hypothesis 2 delayed-recall condition means are reproduced from the uploaded author data: ",
      "RR M = ", round(condition_means[["RR"]], 6), " (SD = ", round(condition_sds[["RR"]], 6), "), ",
      "MMRP M = ", round(condition_means[["MMRP"]], 6), " (SD = ", round(condition_sds[["MMRP"]], 6), "), ",
      "RP M = ", round(condition_means[["RP"]], 6), " (SD = ", round(condition_sds[["RP"]], 6), "). ",
      "Using all complete cases and controlling for Sex gives F(",
      sex_controlled$df_condition, ", ", sex_controlled$df_error_condition, ") = ",
      round(sex_controlled$f_value, 6), ", p = ", signif(sex_controlled$p_value, 6),
      ", eta_p2 = ", round(sex_controlled$eta_p2, 6), ". ",
      "The unadjusted author-syntax-style repeated-measures GLM gives F(",
      unadjusted$df_condition, ", ", unadjusted$df_error, ") = ",
      round(unadjusted$f_value, 6), ", p = ", signif(unadjusted$p_value, 6),
      ", eta_p2 = ", round(unadjusted$eta_p2, 6), ". ",
      "The published inferential value F(2,116) = 4.22, eta_p2 = .07 is not recovered from the uploaded data/syntax variables. ",
      "The script does not exclude participant 39 and does not force the article value. Variable ordering only changes labels/contrasts, not the omnibus condition F."
    )
  )

  readr::write_csv(results, output_path)
  return(results)
}
