reproduce_study_60 <- function(
    input_path = "data/raw/study_60/Syllabus_Safety_Cues_Instructor_Gender_Data_Sp21_OSF.sav",
    output_path = "outputs/reproduced/study_60_recomputed.csv"
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

  raw <- as.data.frame(haven::read_sav(input_path))

  require_columns <- function(cols) {
    missing_cols <- setdiff(cols, names(raw))
    if (length(missing_cols) > 0) {
      stop("Missing required column(s): ", paste(missing_cols, collapse = ", "), call. = FALSE)
    }
  }

  as_numeric <- function(x) {
    if (inherits(x, "haven_labelled")) {
      return(as.numeric(x))
    }
    suppressWarnings(as.numeric(x))
  }

  row_mean_spss <- function(df) {
    out <- rowMeans(df, na.rm = TRUE)
    out[is.nan(out)] <- NA_real_
    out
  }

  winsorise_three_sd_to_nearest <- function(x) {
    x <- as_numeric(x)
    ok <- !is.na(x)
    if (sum(ok) < 2) return(x)

    lower <- mean(x[ok]) - 3 * stats::sd(x[ok])
    upper <- mean(x[ok]) + 3 * stats::sd(x[ok])
    inside <- ok & x >= lower & x <= upper

    if (sum(inside) == 0) return(x)

    lower_replacement <- min(x[inside], na.rm = TRUE)
    upper_replacement <- max(x[inside], na.rm = TRUE)

    x[ok & x < lower] <- lower_replacement
    x[ok & x > upper] <- upper_replacement
    x
  }

  require_columns(c("IV_profgender", "IV_safetycues"))

  # The original SPSS syntax computes expected field belonging from nine items,
  # reverse-coding fieldexp_4 before averaging. If the saved composite is already
  # present, the item-level reconstruction is still preferred because it documents
  # the author scoring rule transparently.
  field_items <- c(
    "fieldexp_1", "fieldexp_2", "fieldexp_3", "fieldexp_4",
    "fieldexp_5", "fieldexp_6", "fieldexp_7", "fieldexp_8", "fieldexp_9"
  )

  if (all(field_items %in% names(raw))) {
    field_dat <- data.frame(
      fieldexp_1 = as_numeric(raw$fieldexp_1),
      fieldexp_2 = as_numeric(raw$fieldexp_2),
      fieldexp_3 = as_numeric(raw$fieldexp_3),
      fieldexp_4r = 6 - as_numeric(raw$fieldexp_4),
      fieldexp_5 = as_numeric(raw$fieldexp_5),
      fieldexp_6 = as_numeric(raw$fieldexp_6),
      fieldexp_7 = as_numeric(raw$fieldexp_7),
      fieldexp_8 = as_numeric(raw$fieldexp_8),
      fieldexp_9 = as_numeric(raw$fieldexp_9)
    )
    raw$fieldbelong_high_recomputed <- row_mean_spss(field_dat)
  } else if ("fieldbelong_high" %in% names(raw)) {
    raw$fieldbelong_high_recomputed <- as_numeric(raw$fieldbelong_high)
  } else {
    stop(
      "Cannot reconstruct expected field belonging: neither the nine fieldexp items nor fieldbelong_high are present.",
      call. = FALSE
    )
  }

  require_columns(c("include_1"))

  # The author syntax reports that outliers beyond 3 SD were winsorized by replacing
  # them with the nearest value inside the 3-SD boundary. Reapplying this to already
  # winsorized data has no effect, but it recovers the SPSS analysis when the shared
  # file contains pre-winsorized values.
  raw$fieldbelong_high_analysis <- winsorise_three_sd_to_nearest(raw$fieldbelong_high_recomputed)
  raw$include_1_analysis <- winsorise_three_sd_to_nearest(raw$include_1)

  type3_safety_effect <- function(data, outcome) {
    dat <- data.frame(
      y = as_numeric(data[[outcome]]),
      professor_gender = factor(as_numeric(data$IV_profgender)),
      syllabus_safety_cues = factor(as_numeric(data$IV_safetycues))
    )

    dat <- dat[stats::complete.cases(dat), , drop = FALSE]

    if (nlevels(dat$professor_gender) != 2 || nlevels(dat$syllabus_safety_cues) != 2) {
      stop("Expected exactly two levels for professor gender and syllabus safety cues.", call. = FALSE)
    }

    old_contrasts <- options("contrasts")[[1]]
    on.exit(options(contrasts = old_contrasts), add = TRUE)
    options(contrasts = c("contr.sum", "contr.poly"))

    fit <- stats::lm(y ~ professor_gender * syllabus_safety_cues, data = dat)
    coef_table <- summary(fit)$coefficients
    safety_row <- grep("^syllabus_safety_cues", rownames(coef_table), value = TRUE)
    safety_row <- safety_row[!grepl(":", safety_row)]

    if (length(safety_row) != 1) {
      stop("Could not identify the syllabus-safety-cues coefficient in the Type-III model.", call. = FALSE)
    }

    t_value <- unname(coef_table[safety_row, "t value"])
    f_value <- t_value^2
    f_df1 <- 1
    f_df2 <- stats::df.residual(fit)
    p_value <- stats::pf(f_value, f_df1, f_df2, lower.tail = FALSE)
    ss_error <- sum(stats::residuals(fit)^2)
    ms_error <- ss_error / f_df2
    ss_effect <- f_value * ms_error
    eta_p2 <- ss_effect / (ss_effect + ss_error)

    group_counts <- stats::xtabs(~ syllabus_safety_cues, data = dat)
    cell_counts <- stats::xtabs(~ professor_gender + syllabus_safety_cues, data = dat)

    list(
      f_value = f_value,
      f_df1 = f_df1,
      f_df2 = f_df2,
      p_value = p_value,
      ss_effect = ss_effect,
      ss_error = ss_error,
      ms_error = ms_error,
      eta_p2 = eta_p2,
      n_total = nrow(dat),
      n_control = unname(group_counts[1]),
      n_safety = unname(group_counts[2]),
      cell_counts = paste(capture.output(print(cell_counts)), collapse = " | "),
      control_mean = mean(dat$y[dat$syllabus_safety_cues == levels(dat$syllabus_safety_cues)[1]], na.rm = TRUE),
      safety_mean = mean(dat$y[dat$syllabus_safety_cues == levels(dat$syllabus_safety_cues)[2]], na.rm = TRUE),
      control_sd = stats::sd(dat$y[dat$syllabus_safety_cues == levels(dat$syllabus_safety_cues)[1]], na.rm = TRUE),
      safety_sd = stats::sd(dat$y[dat$syllabus_safety_cues == levels(dat$syllabus_safety_cues)[2]], na.rm = TRUE)
    )
  }

  field <- type3_safety_effect(raw, "fieldbelong_high_analysis")
  include <- type3_safety_effect(raw, "include_1_analysis")

  output <- data.frame(
    id = c(44, 45),
    study_id = c("study_60", "study_60"),
    study_DOI = c("10.1177/00986283211043779", "10.1177/00986283211043779"),
    recomputation_status = c(
      "recomputed_from_author_spss_unianova",
      "recomputed_from_author_spss_unianova"
    ),
    stat_test = c("factorial_between_anova", "factorial_between_anova"),
    reported_result = c(
      "Expected field belonging: F(1, 322) = 5.32, p = .022, eta_p2 = .016",
      "Inclusive classroom impression: F(1, 322) = 42.95, p < .001, eta_p2 = .118"
    ),
    p_value = c(field$p_value, include$p_value),
    p_operator = c("=", "<"),
    p_sidedness = c("omnibus", "omnibus"),
    t_value = c(NA_real_, NA_real_),
    t_df = c(NA_real_, NA_real_),
    f_value = c(field$f_value, include$f_value),
    f_df1 = c(field$f_df1, include$f_df1),
    f_df2 = c(field$f_df2, include$f_df2),
    z_value = c(NA_real_, NA_real_),
    chi2_value = c(NA_real_, NA_real_),
    chi2_df = c(NA_real_, NA_real_),
    r_value = c(NA_real_, NA_real_),
    n1 = c(field$n_control, include$n_control),
    n2 = c(field$n_safety, include$n_safety),
    n_total = c(field$n_total, include$n_total),
    n_eff = c(NA_real_, NA_real_),
    effect_size_type = c("eta_p2", "eta_p2"),
    effect_size_value = c(field$eta_p2, include$eta_p2),
    estimate = c(field$safety_mean - field$control_mean, include$safety_mean - include$control_mean),
    se_estimate = c(NA_real_, NA_real_),
    raw_data_file = c(input_path, input_path),
    raw_variable_names = c(
      "IV_profgender; IV_safetycues; fieldexp_1-fieldexp_9; fieldexp_4 reverse-coded; fieldbelong_high recomputed/winsorized",
      "IV_profgender; IV_safetycues; include_1; include_1 winsorized"
    ),
    model_formula = c(
      "fieldbelong_high_analysis ~ IV_profgender * IV_safetycues, Type-III SPSS UNIANOVA logic with sum contrasts",
      "include_1_analysis ~ IV_profgender * IV_safetycues, Type-III SPSS UNIANOVA logic with sum contrasts"
    ),
    contrast_direction = c(
      "identity-safety-cue syllabus versus control syllabus on expected social-psychology field belonging",
      "identity-safety-cue syllabus versus control syllabus on perception that the professor is trying to create an inclusive classroom environment"
    ),
    extraction_note = c(
      sprintf(
        "Recomputed from the author SPSS syntax: fieldexp_4 was reverse-coded as 6-fieldexp_4, the nine field belonging items were averaged, 3-SD outliers were winsorized to the nearest non-outlier value, and fieldbelong_high was analysed with UNIANOVA BY IV_profgender IV_safetycues /METHOD=SSTYPE(3) /DESIGN=IV_profgender IV_safetycues IV_profgender*IV_safetycues. Result: F(1, %.0f) = %.6f, p = %.6g, eta_p2 = %.6f. Control n = %s, safety n = %s; control M = %.6f, safety M = %.6f.",
        field$f_df2, field$f_value, field$p_value, field$eta_p2,
        field$n_control, field$n_safety, field$control_mean, field$safety_mean
      ),
      sprintf(
        "Recomputed from the author SPSS syntax: include_1 was winsorized for 3-SD outliers and analysed with UNIANOVA BY IV_profgender IV_safetycues /METHOD=SSTYPE(3) /DESIGN=IV_profgender IV_safetycues IV_profgender*IV_safetycues. Result: F(1, %.0f) = %.6f, p = %.6g, eta_p2 = %.6f. Control n = %s, safety n = %s; control M = %.6f, safety M = %.6f.",
        include$f_df2, include$f_value, include$p_value, include$eta_p2,
        include$n_control, include$n_safety, include$control_mean, include$safety_mean
      )
    ),
    stringsAsFactors = FALSE
  )

  utils::write.csv(output, output_path, row.names = FALSE, na = "")
  message("Wrote ", output_path)
  invisible(output)
}
