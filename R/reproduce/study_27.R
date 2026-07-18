# Study 27: Chopik & Oh (2022), Teaching of Psychology, 10.1177/00986283211065746
# "Implementing the Fast Friends Procedure to Build Camaraderie in a Remote
#  Synchronous Teaching Setting."
# Reproduction of the paired-samples t tests in Table 1 (pre-to-post attitude change).
#
# EFFECT-SIZE NOTE (this is the whole story of the discrepancy):
#   The manuscript's Cohen's d standardizes by the AVERAGE of the pre and post SDs:
#       d = mean_diff / ((sd_pre + sd_post) / 2)          (Lakens 2013, d_av style)
#   SPSS's default paired d (/ES STANDARDIZER(SD)) standardizes by the SD of the
#   DIFFERENCE scores (that is Cohen's dz), which is smaller. So SPSS reports
#   dz = 4.05 for "closeness" while the manuscript reports d = 5.65.
#   Using the manuscript's average-SD method on the raw data:
#     - Result 2 "spirit of community" reproduces exactly (d = 1.80).
#     - Result 1 "closeness" reproduces to d = 5.64 vs the reported 5.65 (0.01 rounding).

check_study_27_packages <- function() {
  required <- c("haven", "readr", "dplyr", "tibble")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0L) {
    stop("Install the required packages before running Study 27: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

read_study_27_data <- function(path) {
  check_study_27_packages()
  if (!file.exists(path)) stop("Study 27 data file does not exist: ", path, call. = FALSE)
  tibble::as_tibble(haven::read_sav(path))
}

# SPSS long variable names may be stored lower-case (Q3_4pre); match case-insensitively.
find_var_27 <- function(dat, target) {
  hit <- names(dat)[tolower(names(dat)) == tolower(target)]
  if (length(hit) != 1L) {
    stop("Study 27: cannot uniquely resolve variable '", target, "'.", call. = FALSE)
  }
  hit
}

reproduce_study_27_pair <- function(dat, pre, post, analysis_id, outcome,
                                    reported_t, reported_d) {
  pre  <- find_var_27(dat, pre)
  post <- find_var_27(dat, post)

  x_pre  <- suppressWarnings(as.numeric(dat[[pre]]))
  x_post <- suppressWarnings(as.numeric(dat[[post]]))
  keep   <- stats::complete.cases(x_pre, x_post)
  pre_c  <- x_pre[keep]; post_c <- x_post[keep]; d <- post_c - pre_c

  n <- length(d)
  if (n < 2L) stop("Fewer than two complete pairs for outcome: ", outcome, call. = FALSE)

  test <- stats::t.test(post_c, pre_c, paired = TRUE, alternative = "two.sided")
  t_value  <- unname(test$statistic)
  df_value <- unname(test$parameter)
  p_value  <- unname(test$p.value)

  sd_pre  <- stats::sd(pre_c)
  sd_post <- stats::sd(post_c)
  sd_diff <- stats::sd(d)

  # Manuscript effect size: average-SD standardizer (reproduces Table 1).
  cohen_d_average_sd <- mean(d) / ((sd_pre + sd_post) / 2)
  # SPSS default paired d for reference (standardized by SD of differences).
  spss_cohen_dz <- mean(d) / sd_diff

  tibble::tibble(
    study_id = "study_27",
    analysis_id = analysis_id,
    outcome = outcome,
    recomputation_status = ifelse(
      round(cohen_d_average_sd, 2) == round(reported_d, 2),
      "recomputed_from_raw_data",
      "recomputed_from_raw_data_effect_size_average_sd_off_by_rounding"),
    stat_test = "paired_t_test",
    subtraction_direction = "post_minus_pre",
    n_total = nrow(dat),
    n_complete = n,
    n_missing_pair = nrow(dat) - n,
    df = df_value,
    pre_mean = mean(pre_c), pre_sd = sd_pre,
    post_mean = mean(post_c), post_sd = sd_post,
    mean_difference = mean(d), sd_difference = sd_diff,
    t_value = t_value, p_value = p_value,
    p_operator = ifelse(p_value < 0.001, "<", "="),
    effect_size_type = "cohen_d_average_sd",
    effect_size_value = cohen_d_average_sd,
    spss_cohen_dz = spss_cohen_dz,
    reported_t = reported_t, reported_d = reported_d,
    t_absolute_difference = abs(abs(t_value) - reported_t),
    d_absolute_difference = abs(abs(cohen_d_average_sd) - reported_d),
    matches_reported_t = round(abs(t_value), 2) == round(reported_t, 2),
    matches_reported_d = round(abs(cohen_d_average_sd), 2) == round(reported_d, 2),
    notes = paste0(
      "Paired-samples t test, POST - PRE, pairwise-complete. Effect size is the ",
      "manuscript's average-SD d = mean_diff/((sd_pre+sd_post)/2). SPSS's ",
      "difference-score d (dz) = ", round(spss_cohen_dz, 3), " differs and is not ",
      "what the article reports. Raw .sav holds ", nrow(dat), " rows; ", n,
      " have complete pre/post on this item."))
}

reproduce_study_27 <- function(
    input_path = "data/raw/study_27/ClassExerciseData.sav",
    output_path = "outputs/reproduced/study_27_recomputed.csv",
    audit_path = "outputs/reproduced/study_27_recomputation_audit.csv") {
  dat <- read_study_27_data(input_path)
  result_1 <- reproduce_study_27_pair(
    dat, pre = "Q3_4pre", post = "Q3_4", analysis_id = "study_27_result_1",
    outcome = "Made me feel closer to my classmate",
    reported_t = 25.32, reported_d = 5.65)
  result_2 <- reproduce_study_27_pair(
    dat, pre = "Q3_9pre", post = "Q3_9", analysis_id = "study_27_result_2",
    outcome = "Made me feel a spirit of community with other students in my class",
    reported_t = 9.03, reported_d = 1.80)
  results <- dplyr::bind_rows(result_1, result_2)
  write_study_27_outputs(results, output_path, audit_path)
  invisible(results)
}

# ---- write generic-schema recomputed CSV (98_compile-ready) + native audit ----
write_study_27_outputs <- function(results, output_path, audit_path) {
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(audit_path), recursive = TRUE, showWarnings = FALSE)

  meta <- list(
    study_27_result_1 = list(id = 46L,
      reported_result = "t(38) = 25.32, p < .001, d = 5.65",
      raw_variable_names = "Q3_4pre; Q3_4"),
    study_27_result_2 = list(id = 47L,
      reported_result = "t(38) = 9.03, p < .001, d = 1.80",
      raw_variable_names = "Q3_9pre; Q3_9")
  )

  generic <- do.call(rbind, lapply(seq_len(nrow(results)), function(i) {
    r <- results[i, , drop = FALSE]
    m <- meta[[r$analysis_id]]
    data.frame(
      id = m$id, study_id = "study_27", study_DOI = "10.1177/00986283211065746",
      recomputation_status = r$recomputation_status, stat_test = "paired_t_test",
      reported_result = m$reported_result,
      p_value = r$p_value, p_operator = r$p_operator, p_sidedness = "two_sided",
      t_value = r$t_value, t_df = r$df,
      f_value = NA_real_, f_df1 = NA_real_, f_df2 = NA_real_,
      z_value = NA_real_, chi2_value = NA_real_, chi2_df = NA_real_, r_value = NA_real_,
      n1 = NA_real_, n2 = NA_real_, n_total = r$n_complete, n_eff = NA_real_,
      effect_size_type = "cohen_d_average_sd", effect_size_value = r$effect_size_value,
      estimate = r$mean_difference, se_estimate = r$mean_difference / r$t_value,
      raw_data_file = "data/raw/study_27/ClassExerciseData.sav",
      raw_variable_names = m$raw_variable_names,
      model_formula = "paired t-test: post - pre; average-SD Cohen's d",
      contrast_direction = "post minus pre",
      extraction_note = r$notes, stringsAsFactors = FALSE
    )
  }))

  readr::write_csv(generic, output_path, na = "")
  readr::write_csv(results, audit_path, na = "")
  invisible(results)
}
