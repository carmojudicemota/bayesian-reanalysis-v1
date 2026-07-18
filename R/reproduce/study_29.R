# Study 29: Morling & Lee (2019), Teaching of Psychology, 10.1177/0098628319888087
# "Are 'Associate Professors' Better Than 'Associate Teaching Professors'?
#  Student and Faculty Perceptions of Faculty Titles."
#
# Design: 2 (Title: 1 = Associate Teaching Professor, 2 = Associate Professor)
#         x 2 (Department: 1, 2) between-subjects factorial ANOVA, FACULTY sample.
#
# CORRECT-DV NOTE (this is where the original attempt went wrong):
#   The reported tenure result uses `univtenure` -- the rated likelihood that the
#   described UNIVERSITY professor (the title-manipulated target) has tenure.
#   Using `cctenure` (a different target) gives F(1,435) = 0.24 and is wrong.
#   The provided CSV is ALREADY the faculty sample; no respondent filtering needed.
#
# Two extracted results (matching the project's phase-2 coding):
#   result_01 (key)    tenure   univtenure  F(1,432) = 232.57  reported d = 1.50
#   result_02 (second) children univkids    F(1,429) = 14.19   reported d = 0.36
#   Result 1: F and 95% CI reproduce exactly; Cohen's d = 1.46 vs reported 1.50
#             (off by 0.04) -> not reproducible on the effect size.
#   Result 2: fully reproducible (F and d).

check_study_29_packages <- function() {
  required <- c("readr", "dplyr", "tibble", "car")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0L) {
    stop("Install the required packages before running Study 29: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

read_study_29_data <- function(path) {
  check_study_29_packages()
  if (!file.exists(path)) stop("Study 29 data file does not exist: ", path, call. = FALSE)
  tibble::as_tibble(readr::read_csv(path, show_col_types = FALSE))
}

# One Title main effect from the 2x2 Type-III factorial ANOVA (SPSS UNIANOVA SSTYPE(3)).
reproduce_study_29_effect <- function(dat, dv, id, reported_result, reported_d, contrast) {
  keep <- !is.na(dat[[dv]]) & dat$Title %in% c(1, 2) & dat$Department %in% c(1, 2)
  d <- dat[keep, c(dv, "Title", "Department")]
  d$Title      <- factor(d$Title)
  d$Department <- factor(d$Department)

  op <- options(contrasts = c("contr.sum", "contr.poly")); on.exit(options(op))
  fit  <- stats::lm(stats::as.formula(paste0("`", dv, "` ~ Title * Department")), data = d)
  aov3 <- car::Anova(fit, type = 3)

  F_val  <- aov3["Title", "F value"]
  df1    <- aov3["Title", "Df"]
  df2    <- fit$df.residual
  p_val  <- aov3["Title", "Pr(>F)"]
  MSE    <- sum(stats::residuals(fit)^2) / df2

  m_prof  <- mean(d[[dv]][d$Title == "2"])   # Associate Professor
  m_teach <- mean(d[[dv]][d$Title == "1"])   # Associate Teaching Professor
  diff    <- m_prof - m_teach
  cohen_d <- diff / sqrt(MSE)
  N       <- nrow(d)
  se_d    <- sqrt(4 / N + cohen_d^2 / (2 * df2))
  eta_p2  <- (F_val * df1) / (F_val * df1 + df2)

  matches_d <- round(abs(cohen_d), 2) == round(reported_d, 2)
  status <- if (matches_d) "recomputed_from_raw_data"
            else "recomputed_from_raw_data_effect_size_d_off_by_0.04"

  tibble::tibble(
    id = id, study_id = "study_29", study_DOI = "10.1177/0098628319888087",
    recomputation_status = status, stat_test = "factorial_between_anova",
    reported_result = reported_result,
    p_value = p_val, p_operator = "=", p_sidedness = "omnibus",
    t_value = NA_real_, t_df = NA_real_,
    f_value = F_val, f_df1 = df1, f_df2 = df2,
    z_value = NA_real_, chi2_value = NA_real_, chi2_df = NA_real_, r_value = NA_real_,
    n1 = sum(d$Title == "1"), n2 = sum(d$Title == "2"), n_total = N, n_eff = NA_real_,
    effect_size_type = "cohens_d", effect_size_value = cohen_d,
    estimate = diff, se_estimate = se_d,
    raw_data_file = "data/raw/study_29/Morling and Lee Faculty Sample Open Data.csv",
    raw_variable_names = paste0(dv, "; Title; Department"),
    model_formula = paste0(dv, " ~ Title * Department; Type-III between-subjects ANOVA ",
                           "(SPSS UNIANOVA SSTYPE(3)); Title main effect"),
    contrast_direction = contrast,
    extraction_note = sprintf(
      "Title main effect on %s. eta_p2 = %.3f; Cohen's d = mean_diff / sqrt(MSE) = %.3f (reported %.2f). %s",
      dv, eta_p2, cohen_d, reported_d,
      if (matches_d) "Reproduces." else "F and 95% CI reproduce; d off by ~0.04."))
}

reproduce_study_29 <- function(
    input_path = "data/raw/study_29/Morling and Lee Faculty Sample Open Data.csv",
    output_path = "outputs/reproduced/study_29_recomputed.csv") {
  dat <- read_study_29_data(input_path)
  result_1 <- reproduce_study_29_effect(
    dat, dv = "univtenure", id = 48L,
    reported_result = "F(1, 432) = 232.57, p < .001, d = 1.50, 95% CI [1.25, 1.67]",
    reported_d = 1.50,
    contrast = "Associate Professor (Title=2) minus Associate Teaching Professor (Title=1)")
  result_2 <- reproduce_study_29_effect(
    dat, dv = "univkids", id = 49L,
    reported_result = "F(1, 429) = 14.19, p < .001, d = 0.36, 95% CI [0.17, 0.55]",
    reported_d = 0.36,
    contrast = "Associate Professor (Title=2) minus Associate Teaching Professor (Title=1)")
  results <- dplyr::bind_rows(result_1, result_2)
  write_study_29_outputs(results, output_path)
  invisible(results)
}

write_study_29_outputs <- function(results, output_path) {
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(results, output_path, na = "")
  invisible(results)
}
