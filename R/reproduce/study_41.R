# Study 41: Kelly & Parrish (2025)
# Reproduction of paired-samples t tests from the deidentified content-knowledge data.
#
# Important:
# - The article reports N = 37 students who consented to data use.
# - The target item analyses use pairwise-complete observations, not mean imputation.
# - For "Statistically analyze data", n_complete = 30 and df = 29.
# - For "Interpret data by relating results to the original hypothesis",
#   n_complete = 29 and df = 28.
# - The article reports positive t and d values because change is interpreted as POST - PRE.

#' Check packages needed for Study 41
#'
#' @return Invisibly returns TRUE.
check_study_41_packages <- function() {
  required <- c("readxl", "readr", "dplyr", "tibble")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing) > 0L) {
    stop(
      "Install the required packages before running Study 41: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Read and validate the Study 41 content-knowledge workbook
#'
#' @param path Path to BART Content Knowledge_Deidentified.xlsx.
#'
#' @return A tibble containing the original 37 rows.
read_study_41_data <- function(path) {
  check_study_41_packages()

  if (!file.exists(path)) {
    stop("Study 41 data file does not exist: ", path, call. = FALSE)
  }

  dat <- readxl::read_excel(path, sheet = "Sheet1")
  dat <- tibble::as_tibble(dat)

  required <- c(
    "ID",
    "PRE_Analyze",
    "POST_Analyze",
    "PRE_Intr data",
    "POST_Intr data"
  )

  missing <- setdiff(required, names(dat))
  if (length(missing) > 0L) {
    stop(
      "Study 41 workbook is missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  if (nrow(dat) != 37L) {
    warning(
      "Expected 37 consented students, but the workbook contains ",
      nrow(dat),
      " rows.",
      call. = FALSE
    )
  }

  dat
}

#' Reproduce one paired-samples result for Study 41
#'
#' @param dat Study 41 data frame.
#' @param pre Name of the pre-test variable.
#' @param post Name of the post-test variable.
#' @param analysis_id Stable analysis identifier.
#' @param outcome Human-readable outcome label.
#' @param reported_t Published t value.
#' @param reported_d Published Cohen's d value.
#'
#' @return A one-row tibble with exact recomputed statistics.
reproduce_study_41_pair <- function(dat,
                                    pre,
                                    post,
                                    analysis_id,
                                    outcome,
                                    reported_t,
                                    reported_d) {
  stopifnot(
    is.data.frame(dat),
    length(pre) == 1L,
    length(post) == 1L,
    pre %in% names(dat),
    post %in% names(dat)
  )

  x_pre <- suppressWarnings(as.numeric(dat[[pre]]))
  x_post <- suppressWarnings(as.numeric(dat[[post]]))
  keep <- stats::complete.cases(x_pre, x_post)

  pre_complete <- x_pre[keep]
  post_complete <- x_post[keep]
  difference <- post_complete - pre_complete

  n_complete <- length(difference)
  if (n_complete < 2L) {
    stop("Fewer than two complete pairs for outcome: ", outcome, call. = FALSE)
  }

  difference_sd <- stats::sd(difference)
  if (!is.finite(difference_sd) || difference_sd <= 0) {
    stop("Difference-score SD is not positive for outcome: ", outcome, call. = FALSE)
  }

  test <- stats::t.test(
    post_complete,
    pre_complete,
    paired = TRUE,
    alternative = "two.sided",
    conf.level = 0.95
  )

  t_value <- unname(test$statistic)
  df_value <- unname(test$parameter)
  p_value <- unname(test$p.value)

  # Cohen's dz for paired data: mean(POST - PRE) / SD(POST - PRE).
  cohen_dz <- mean(difference) / difference_sd

  tibble::tibble(
    study_id = "study_41",
    analysis_id = analysis_id,
    outcome = outcome,
    recomputation_status = "recomputed_from_raw_data_pairwise_complete",
    stat_test = "paired_t_test",
    subtraction_direction = "post_minus_pre",
    n_total = nrow(dat),
    n_complete = n_complete,
    n_missing_pair = nrow(dat) - n_complete,
    df = df_value,
    pre_mean = mean(pre_complete),
    pre_sd = stats::sd(pre_complete),
    post_mean = mean(post_complete),
    post_sd = stats::sd(post_complete),
    mean_difference = mean(difference),
    sd_difference = difference_sd,
    t_value = t_value,
    p_value = p_value,
    p_operator = ifelse(p_value < 0.001, "<", "="),
    effect_size_type = "cohen_dz",
    effect_size_value = cohen_dz,
    reported_t = reported_t,
    reported_d = reported_d,
    t_absolute_difference = abs(abs(t_value) - reported_t),
    d_absolute_difference = abs(abs(cohen_dz) - reported_d),
    matches_reported_t = abs(abs(t_value) - reported_t) < 0.01,
    matches_reported_d = abs(abs(cohen_dz) - reported_d) < 0.01,
    notes = paste0(
      "No imputation. Analysis uses pairwise-complete PRE/POST values. ",
      "The article table reports t and d but does not report df; df follows from ",
      "the number of complete pairs. Positive signs reflect POST - PRE."
    )
  )
}

#' Reproduce all target results for Study 41
#'
#' @param path Path to BART Content Knowledge_Deidentified.xlsx.
#'
#' @return A two-row tibble.
reproduce_study_41 <- function(path) {
  dat <- read_study_41_data(path)

  result_1 <- reproduce_study_41_pair(
    dat = dat,
    pre = "PRE_Analyze",
    post = "POST_Analyze",
    analysis_id = "study_41_result_1",
    outcome = "Statistically analyze data",
    reported_t = 8.53,
    reported_d = 1.56
  )

  result_2 <- reproduce_study_41_pair(
    dat = dat,
    pre = "PRE_Intr data",
    post = "POST_Intr data",
    analysis_id = "study_41_result_2",
    outcome = "Interpret data by relating results to the original hypothesis",
    reported_t = 6.15,
    reported_d = 1.14
  )

  dplyr::bind_rows(result_1, result_2)
}

#' Write Study 41 reproduction and audit outputs
#'
#' @param results Output from reproduce_study_41().
#' @param output_path Main CSV path.
#' @param audit_path Audit CSV path.
#'
#' @return Invisibly returns the results.

# ---- write generic-schema recomputed CSV (98_compile-ready) + native audit ----
# reproduce_study_41() still returns the detailed native tibble (used by the test);
# the main recomputed CSV is now the generic schema every other study uses.
write_study_41_outputs <- function(results, output_path, audit_path) {
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(audit_path), recursive = TRUE, showWarnings = FALSE)

  meta <- list(
    study_41_result_1 = list(id = 28L,
      reported_result = "t(36) = 8.53, p < .001, d = 1.56",
      raw_variable_names = "PRE_Analyze; POST_Analyze"),
    study_41_result_2 = list(id = 29L,
      reported_result = "t(36) = 6.15, p < .001, d = 1.14",
      raw_variable_names = "PRE_Intr data; POST_Intr data")
  )

  generic <- do.call(rbind, lapply(seq_len(nrow(results)), function(i) {
    r <- results[i, , drop = FALSE]
    m <- meta[[r$analysis_id]]
    data.frame(
      id = m$id, study_id = "study_41", study_DOI = "10.1177/00986283251313760",
      recomputation_status = "recomputed_from_raw_data", stat_test = "paired_t_test",
      reported_result = m$reported_result,
      p_value = r$p_value, p_operator = r$p_operator, p_sidedness = "two_sided",
      t_value = r$t_value, t_df = r$df,
      f_value = NA_real_, f_df1 = NA_real_, f_df2 = NA_real_,
      z_value = NA_real_, chi2_value = NA_real_, chi2_df = NA_real_, r_value = NA_real_,
      n1 = NA_real_, n2 = NA_real_, n_total = r$n_complete, n_eff = NA_real_,
      effect_size_type = "cohens_dz_paired", effect_size_value = r$effect_size_value,
      estimate = r$mean_difference, se_estimate = r$sd_difference / sqrt(r$n_complete),
      raw_data_file = "data/raw/study_41/BART_Content_Knowledge_Deidentified.xlsx",
      raw_variable_names = m$raw_variable_names,
      model_formula = "paired t-test: POST - PRE (pairwise-complete)",
      contrast_direction = "post minus pre",
      extraction_note = r$notes, stringsAsFactors = FALSE
    )
  }))

  readr::write_csv(generic, output_path, na = "")   # generic schema -> 98_compile
  readr::write_csv(results, audit_path, na = "")    # full native detail
  invisible(results)
}
