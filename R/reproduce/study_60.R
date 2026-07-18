# Study 60: Maimon, Howansky, and Sanchez (2023)
# Reproduction functions for the two retained syllabus-safety-cue effects.

#' Validate the Study 60 author dataset
#'
#' @param dat A data frame imported from the author SPSS .sav file.
#' @return The input data invisibly.
#' @export
validate_study_60_data <- function(dat) {
  required <- c(
    "participantID", "IV_profgender", "IV_safetycues",
    "fieldbelong_high", "include_1",
    paste0("fieldexp_", 1:9)
  )

  missing <- setdiff(required, names(dat))
  if (length(missing) > 0L) {
    stop(
      "Study 60 data are missing required variables: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  if (nrow(dat) != 326L) {
    stop("Study 60 must contain 326 retained participants; found ", nrow(dat), ".", call. = FALSE)
  }

  if (!setequal(stats::na.omit(unique(dat$IV_profgender)), c(0, 1))) {
    stop("IV_profgender must be coded 0 = man and 1 = woman.", call. = FALSE)
  }

  if (!setequal(stats::na.omit(unique(dat$IV_safetycues)), c(0, 1))) {
    stop("IV_safetycues must be coded 0 = control and 1 = safety.", call. = FALSE)
  }

  invisible(dat)
}

#' Reconstruct the un-winsorized expected-field-belonging composite
#'
#' The fourth item is reverse-scored as 6 - item 4. SPSS MEAN() semantics
#' are reproduced by rowMeans(..., na.rm = TRUE).
#'
#' @param dat Validated Study 60 data.
#' @return A numeric vector of composite scores.
#' @export
reconstruct_field_belonging_raw <- function(dat) {
  item_names <- paste0("fieldexp_", 1:9)
  items <- dat[, item_names, drop = FALSE]
  items$fieldexp_4 <- 6 - items$fieldexp_4

  out <- rowMeans(items, na.rm = TRUE)
  all_missing <- rowSums(!is.na(items)) == 0L
  out[all_missing] <- NA_real_
  out
}

#' Winsorize values beyond three standard deviations to the nearest observed
#' non-outlier value
#'
#' This follows the authors' written rule: observations beyond three standard
#' deviations are replaced by the next-closest observed value lying inside the
#' three-standard-deviation interval.
#'
#' @param x Numeric vector.
#' @param z Positive threshold in standard deviations; default 3.
#' @return A list with the winsorized vector and audit information.
#' @export
winsorize_to_nearest_observed <- function(x, z = 3) {
  if (!is.numeric(x)) stop("x must be numeric.", call. = FALSE)
  if (length(z) != 1L || !is.finite(z) || z <= 0) {
    stop("z must be one positive finite number.", call. = FALSE)
  }

  mu <- mean(x, na.rm = TRUE)
  sigma <- stats::sd(x, na.rm = TRUE)
  lower <- mu - z * sigma
  upper <- mu + z * sigma

  inside <- x[!is.na(x) & x >= lower & x <= upper]
  if (length(inside) == 0L) stop("No observed values fall inside the winsorization interval.", call. = FALSE)

  low_replacement <- min(inside)
  high_replacement <- max(inside)
  low_index <- which(!is.na(x) & x < lower)
  high_index <- which(!is.na(x) & x > upper)

  out <- x
  out[low_index] <- low_replacement
  out[high_index] <- high_replacement

  list(
    values = out,
    mean = mu,
    sd = sigma,
    lower = lower,
    upper = upper,
    low_index = low_index,
    high_index = high_index,
    low_replacement = low_replacement,
    high_replacement = high_replacement
  )
}

#' Fit a two-by-two Type-III factorial ANOVA
#'
#' @param dat Data frame containing the outcome and the two design variables.
#' @param outcome Character scalar naming the dependent variable.
#' @return A list containing the fitted model, Type-III ANOVA table, and the
#'   syllabus-safety-cue result.
#' @export
fit_study_60_anova <- function(dat, outcome) {
  if (!requireNamespace("car", quietly = TRUE)) {
    stop("Package 'car' is required. Install it with install.packages('car').", call. = FALSE)
  }
  if (!outcome %in% names(dat)) stop("Outcome not found in data: ", outcome, call. = FALSE)

  analysis_dat <- dat[, c(outcome, "IV_profgender", "IV_safetycues"), drop = FALSE]
  names(analysis_dat)[1] <- "outcome"
  analysis_dat <- analysis_dat[stats::complete.cases(analysis_dat), , drop = FALSE]

  analysis_dat$IV_profgender <- factor(
    analysis_dat$IV_profgender, levels = c(0, 1), labels = c("man", "woman")
  )
  analysis_dat$IV_safetycues <- factor(
    analysis_dat$IV_safetycues, levels = c(0, 1), labels = c("control", "safety")
  )

  old_contrasts <- getOption("contrasts")
  on.exit(options(contrasts = old_contrasts), add = TRUE)
  options(contrasts = c("contr.sum", "contr.poly"))

  model <- stats::lm(outcome ~ IV_profgender * IV_safetycues, data = analysis_dat)
  type3 <- car::Anova(model, type = 3)
  effect_row <- "IV_safetycues"

  ss_effect <- unname(type3[effect_row, "Sum Sq"])
  ss_error <- unname(type3["Residuals", "Sum Sq"])

  result <- list(
    f_value = unname(type3[effect_row, "F value"]),
    df1 = unname(type3[effect_row, "Df"]),
    df2 = unname(type3["Residuals", "Df"]),
    p_value = unname(type3[effect_row, "Pr(>F)"]),
    eta_p2 = ss_effect / (ss_effect + ss_error),
    n_total = nrow(analysis_dat),
    n_control = sum(analysis_dat$IV_safetycues == "control"),
    n_safety = sum(analysis_dat$IV_safetycues == "safety"),
    mean_control = mean(analysis_dat$outcome[analysis_dat$IV_safetycues == "control"]),
    mean_safety = mean(analysis_dat$outcome[analysis_dat$IV_safetycues == "safety"]),
    sd_control = stats::sd(analysis_dat$outcome[analysis_dat$IV_safetycues == "control"]),
    sd_safety = stats::sd(analysis_dat$outcome[analysis_dat$IV_safetycues == "safety"])
  )

  list(model = model, type3 = type3, result = result, data = analysis_dat)
}

#' Reproduce the retained Study 60 results
#'
#' @param sav_path Path to the original author SPSS data file.
#' @return A list with the main results table and an audit table.
#' @export
reproduce_study_60 <- function(
    sav_path = "data/raw/study_60/Syllabus_Safety_Cues_Instructor_Gender_Data_Sp21_OSF.sav",
    output_path = "outputs/reproduced/study_60_recomputed.csv",
    audit_path = "outputs/reproduced/study_60_recomputation_audit.csv") {
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop("Package 'haven' is required. Install it with install.packages('haven').", call. = FALSE)
  }
  if (!file.exists(sav_path)) stop("Study 60 data file not found: ", sav_path, call. = FALSE)

  dat <- as.data.frame(haven::read_sav(sav_path))
  validate_study_60_data(dat)

  dat$fieldbelong_raw_reconstructed <- reconstruct_field_belonging_raw(dat)
  field_win <- winsorize_to_nearest_observed(dat$fieldbelong_raw_reconstructed, z = 3)
  dat$fieldbelong_exact_winsorized <- field_win$values

  affected_ids <- sort(as.numeric(dat$participantID[c(field_win$low_index, field_win$high_index)]))
  if (!identical(affected_ids, c(128, 320))) {
    stop(
      "Unexpected field-belonging outliers. Expected participant IDs 128 and 320; found: ",
      paste(affected_ids, collapse = ", "), call. = FALSE
    )
  }

  field_published <- fit_study_60_anova(dat, "fieldbelong_exact_winsorized")$result
  field_raw <- fit_study_60_anova(dat, "fieldbelong_raw_reconstructed")$result
  field_saved_rounded <- fit_study_60_anova(dat, "fieldbelong_high")$result
  inclusive <- fit_study_60_anova(dat, "include_1")$result

  make_row <- function(id, reported_result, result, p_operator, estimate,
                       raw_variable_names, model_formula, contrast_direction,
                       extraction_note) {
    data.frame(
      id = id,
      study_id = "study_60",
      study_DOI = "10.1177/00986283211043779",
      recomputation_status = "recomputed_from_author_spss_unianova",
      stat_test = "factorial_between_anova",
      reported_result = reported_result,
      p_value = result$p_value,
      p_operator = p_operator,
      p_sidedness = "omnibus",
      t_value = NA_real_, t_df = NA_real_,
      f_value = result$f_value, f_df1 = result$df1, f_df2 = result$df2,
      z_value = NA_real_, chi2_value = NA_real_, chi2_df = NA_real_,
      r_value = NA_real_,
      n1 = result$n_control, n2 = result$n_safety,
      n_total = result$n_total, n_eff = NA_real_,
      effect_size_type = "eta_p2",
      effect_size_value = result$eta_p2,
      estimate = estimate, se_estimate = NA_real_,
      raw_data_file = sav_path,
      raw_variable_names = raw_variable_names,
      model_formula = model_formula,
      contrast_direction = contrast_direction,
      extraction_note = extraction_note,
      stringsAsFactors = FALSE
    )
  }

  results <- rbind(
    make_row(
      44,
      "Expected field belonging: F(1, 322) = 5.17, p = .02, eta_p2 = .02",
      field_published,
      "=",
      field_published$mean_safety - field_published$mean_control,
      paste(
        "IV_profgender; IV_safetycues; fieldexp_1-fieldexp_9;",
        "fieldexp_4 reverse-coded; exact nearest-observed 3-SD winsorization"
      ),
      paste(
        "fieldbelong_exact_winsorized ~ IV_profgender * IV_safetycues;",
        "Type-III SPSS UNIANOVA logic with sum contrasts"
      ),
      paste(
        "identity-safety-cue syllabus versus control syllabus on expected",
        "social-psychology field belonging"
      ),
      sprintf(
        paste0(
          "The article result is reproduced by reconstructing the nine-item composite, ",
          "reverse-scoring fieldexp_4 as 6-fieldexp_4, and replacing the two values beyond ",
          "three SD with the nearest observed non-outlier values (participant 320: 2 to %.12f; ",
          "participant 128: 5 to %.12f). Result: F(1, 322) = %.12f, p = %.12g, ",
          "eta_p2 = %.12f. Control n = %d, safety n = %d; control M = %.12f, safety M = %.12f. ",
          "The separate SPSS screenshot used the un-winsorized composite and therefore shows ",
          "F = %.12f rather than the article's F = 5.17."
        ),
        field_win$low_replacement, field_win$high_replacement,
        field_published$f_value, field_published$p_value, field_published$eta_p2,
        field_published$n_control, field_published$n_safety,
        field_published$mean_control, field_published$mean_safety,
        field_raw$f_value
      )
    ),
    make_row(
      45,
      "Inclusive classroom impression: F(1, 322) = 42.95, p < .001, eta_p2 = .12",
      inclusive,
      "<",
      inclusive$mean_safety - inclusive$mean_control,
      paste(
        "IV_profgender; IV_safetycues; include_1;",
        "author-saved processed inclusive-classroom score"
      ),
      paste(
        "include_1 ~ IV_profgender * IV_safetycues;",
        "Type-III SPSS UNIANOVA logic with sum contrasts"
      ),
      paste(
        "identity-safety-cue syllabus versus control syllabus on perception that",
        "the professor is trying to create an inclusive classroom environment"
      ),
      sprintf(
        paste0(
          "The author-saved include_1 variable reproduces the article and SPSS output: ",
          "F(1, 322) = %.12f, p = %.12g, eta_p2 = %.12f. Control n = %d, safety n = %d; ",
          "control M = %.12f, safety M = %.12f. The distributed SAV contains the processed ",
          "include_1 values, so the original pre-winsorization observation cannot be recovered ",
          "from this file alone."
        ),
        inclusive$f_value, inclusive$p_value, inclusive$eta_p2,
        inclusive$n_control, inclusive$n_safety,
        inclusive$mean_control, inclusive$mean_safety
      )
    )
  )

  audit <- data.frame(
    analysis = c(
      "field_exact_rule_winsorized_article",
      "field_unwinsorized_spss_screenshot",
      "field_author_saved_rounded_winsorized",
      "inclusive_author_saved_processed"
    ),
    f_value = c(
      field_published$f_value, field_raw$f_value,
      field_saved_rounded$f_value, inclusive$f_value
    ),
    df1 = c(field_published$df1, field_raw$df1, field_saved_rounded$df1, inclusive$df1),
    df2 = c(field_published$df2, field_raw$df2, field_saved_rounded$df2, inclusive$df2),
    p_value = c(
      field_published$p_value, field_raw$p_value,
      field_saved_rounded$p_value, inclusive$p_value
    ),
    eta_p2 = c(
      field_published$eta_p2, field_raw$eta_p2,
      field_saved_rounded$eta_p2, inclusive$eta_p2
    ),
    mean_control = c(
      field_published$mean_control, field_raw$mean_control,
      field_saved_rounded$mean_control, inclusive$mean_control
    ),
    mean_safety = c(
      field_published$mean_safety, field_raw$mean_safety,
      field_saved_rounded$mean_safety, inclusive$mean_safety
    ),
    interpretation = c(
      "Correct main reproduction; matches the published F = 5.17 after exact nearest-observed winsorization.",
      "Matches the supplied SPSS screenshot F = 5.321 and its displayed descriptive statistics.",
      "Uses rounded saved values 2.33 and 4.89; still rounds to F = 5.17 but is not the exact preprocessing rule.",
      "Matches both the published article and supplied SPSS screenshot F = 42.952."
    ),
    stringsAsFactors = FALSE
  )

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(results, output_path, na = "")
  readr::write_csv(audit, audit_path, na = "")
  message("Wrote: ", output_path)
  invisible(list(results = results, audit = audit))
}
