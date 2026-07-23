# Transparent recomputation for Study 40
# Wood & Cross (2024), DOI: 10.1037/stl0000412
# Target index row: id = 27
# Recomputes the nested lavaan chi-square difference test comparing Model 7 and Model 8.

reproduce_study_40 <- function(
    input_path = "data/raw/study_40/Wood_and_Cross_2024_Final_Data_deidentified.csv",
    output_path = "outputs/reproduced/study_40_recomputed.csv"
) {
  required_packages <- c("lavaan")
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

  data <- utils::read.csv(input_path, stringsAsFactors = FALSE)

  required_columns <- c("opt_in", "p_tot1", "p_tot2", "p_tot3", "p_tot4", "p_tot5")
  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0) {
    stop(
      "Missing required column(s) in raw data: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  data$opt_in <- factor(data$opt_in)

  for (col in c("p_tot1", "p_tot2", "p_tot3", "p_tot4", "p_tot5")) {
    data[[col]] <- as.numeric(data[[col]])
  }

  n_total <- nrow(data)
  n_opt_out <- sum(data$opt_in == "0", na.rm = TRUE)
  n_opt_in <- sum(data$opt_in == "1", na.rm = TRUE)

  # Author latent-basis model structure.
  # Exam 1 loading is fixed to 0 and Exam 3 loading is fixed to 1.
  # Exam 2 loading is labelled m in lb_growth_par to enforce the parallel-trends assumption
  # before the intervention while allowing post-intervention loadings to differ by group.
  lb_growth_par <- '
    i =~ 1*p_tot1 + 1*p_tot2 + 1*p_tot3 + 1*p_tot4 + 1*p_tot5
    s =~ 0*p_tot1 + m*p_tot2 + 1*p_tot3 + NA*p_tot4 + NA*p_tot5
  '

  lb_growth <- '
    i =~ 1*p_tot1 + 1*p_tot2 + 1*p_tot3 + 1*p_tot4 + 1*p_tot5
    s =~ 0*p_tot1 + NA*p_tot2 + 1*p_tot3 + NA*p_tot4 + NA*p_tot5
  '

  # Model 7 in the article/code:
  # latent-basis model with parallel pre-intervention loading, group-specific means,
  # and group-specific post-intervention loadings.
  lb_fit3 <- lavaan::growth(
    model = lb_growth_par,
    data = data,
    estimator = "ML",
    missing = "fiml",
    group = "opt_in",
    group.equal = c("residuals", "lv.variances", "lv.covariances"),
    se = "robust"
  )

  # Model 8 in the article/code:
  # alternative explanation model with group-specific mean structure but equal loadings.
  lb_fit4 <- lavaan::growth(
    model = lb_growth,
    data = data,
    estimator = "ML",
    missing = "fiml",
    group = "opt_in",
    group.equal = c("loadings", "residuals", "lv.variances", "lv.covariances"),
    se = "robust"
  )

  comparison <- as.data.frame(lavaan::lavTestLRT(lb_fit3, lb_fit4))

  chi2_diff_col <- grep("Chisq diff", names(comparison), value = TRUE)
  df_diff_col <- grep("Df diff", names(comparison), value = TRUE)
  p_col <- grep("Pr\\(>Chisq\\)", names(comparison), value = TRUE)

  if (length(chi2_diff_col) != 1 || length(df_diff_col) != 1 || length(p_col) != 1) {
    stop("Could not identify chi-square difference columns in lavaan anova output.", call. = FALSE)
  }

  diff_row <- which(!is.na(comparison[[chi2_diff_col]]))[1]
  if (is.na(diff_row)) {
    stop("lavaan anova output did not contain a chi-square difference row.", call. = FALSE)
  }

  chi2_diff <- unname(comparison[[chi2_diff_col]][diff_row])
  df_diff <- unname(comparison[[df_diff_col]][diff_row])
  p_value <- unname(comparison[[p_col]][diff_row])

  if (is.na(p_value)) {
    p_value <- stats::pchisq(chi2_diff, df = df_diff, lower.tail = FALSE)
  }

  fit7 <- lavaan::fitMeasures(lb_fit3)
  fit8 <- lavaan::fitMeasures(lb_fit4)

  status <- if (
    isTRUE(abs(chi2_diff - 10.31) < 0.05) &&
      isTRUE(df_diff == 2) &&
      isTRUE(p_value < 0.01)
  ) {
    "recomputed_from_author_lavaan_models"
  } else {
    "recomputed_but_does_not_match_article"
  }

  extraction_note <- paste0(
    "Recomputed from the author's lavaan latent-basis models using p_tot1-p_tot5 and opt_in. ",
    "Model 7 is lb_fit3: lb_growth_par with group-specific means and post-intervention loadings, ",
    "under the parallel pre-intervention loading assumption. Model 8 is lb_fit4: lb_growth with ",
    "group-specific mean structure but loadings constrained equal across opt-in groups. ",
    "lavaan::lavTestLRT(lb_fit3, lb_fit4) gave Model 7 chi-square = ",
    round(unname(fit7["chisq"]), 6), ", df = ", unname(fit7["df"]),
    "; Model 8 chi-square = ", round(unname(fit8["chisq"]), 6), ", df = ", unname(fit8["df"]),
    "; chi-square difference = ", round(chi2_diff, 6),
    ", df difference = ", df_diff,
    ", p = ", signif(p_value, 6),
    ". n_total = ", n_total,
    "; opt-out n = ", n_opt_out,
    "; opt-in n = ", n_opt_in, "."
  )

  results <- data.frame(
    id = 27,
    study_id = "study_40",
    study_DOI = "10.1037/stl0000412",
    recomputation_status = status,
    stat_test = "chi_square_difference",
    reported_result = "Delta chi-square(2) = 10.31, p < .01",
    p_value = p_value,
    p_operator = "<",
    p_sidedness = "omnibus",
    t_value = NA_real_,
    t_df = NA_real_,
    f_value = NA_real_,
    f_df1 = NA_real_,
    f_df2 = NA_real_,
    z_value = NA_real_,
    chi2_value = chi2_diff,
    chi2_df = df_diff,
    r_value = NA_real_,
    n1 = n_opt_out,
    n2 = n_opt_in,
    n_total = n_total,
    n_eff = NA_real_,
    effect_size_type = "none",
    effect_size_value = NA_real_,
    estimate = NA_real_,
    se_estimate = NA_real_,
    raw_data_file = input_path,
    raw_variable_names = "opt_in; p_tot1; p_tot2; p_tot3; p_tot4; p_tot5",
    model_formula = "lavaan latent-basis growth model: i =~ 1*p_tot1 + ... + 1*p_tot5; s =~ 0*p_tot1 + m/NA*p_tot2 + 1*p_tot3 + NA*p_tot4 + NA*p_tot5",
    contrast_direction = "Model 7 versus Model 8: intervention-related post-intervention trajectory differences beyond group mean-structure differences",
    extraction_note = extraction_note,
    stringsAsFactors = FALSE
  )

  utils::write.csv(results, output_path, row.names = FALSE, na = "NA")

  message("Wrote: ", output_path)
  invisible(results)
}
