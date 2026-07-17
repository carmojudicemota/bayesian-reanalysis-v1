# Study 37: Gravelle (2023), Scholarship of Teaching and Learning in Psychology, 10.1037/stl0000356
# Mixed-effects binomial logistic regression (lme4::glmer) predicting course withdrawal (Attrition),
# with a random intercept for section. Reproduces the two target fixed-effect coefficients from model4_att:
#   result_01 (key)    HUMAN_family  log-odds =  1.23,  p < .001  (family obligations -> more withdrawal)
#   result_02 (second) synchronous1  log-odds = -1.11,  p = .010  (synchronous -> less withdrawal)

STUDY37_FORMULA <- Attrition ~ h1_demo_age_nontrad1 + h1_demo_gender_male1 + h1_demo_ethn_Lat1 +
  h1_demo_ethn_Afr1 + h1_demo_parents_y_1 + h1_demo_addl_lang_EngOther1 + h1_demo_first_semest1 +
  h1_regents_avg + h1_ESSES_avg + h1_textbook_y_1 + h1_blackboard_y_1 + synchronous1 + HUMAN_attn +
  HUMAN_work + HUMAN_course + HUMAN_mental + HUMAN_sleep + HUMAN_family + (1 | section)

check_study_37_packages <- function() {
  req  <- c("lme4", "readr", "dplyr", "tibble")
  miss <- req[!vapply(req, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss) > 0L)
    stop("Install the required packages before running Study 37: ",
         paste(miss, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

read_study_37_data <- function(path) {
  check_study_37_packages()
  if (!file.exists(path)) stop("Study 37 data file does not exist: ", path, call. = FALSE)
  d <- utils::read.csv(path)
  factor_vars <- c("h1_demo_age_nontrad1", "h1_demo_gender_male1", "h1_demo_ethn_Lat1",
    "h1_demo_ethn_Afr1", "h1_demo_parents_y_1", "h1_demo_addl_lang_EngOther1",
    "h1_demo_first_semest1", "h1_textbook_y_1", "h1_blackboard_y_1", "synchronous1",
    "HUMAN_attn", "HUMAN_work", "HUMAN_course", "HUMAN_mental", "HUMAN_sleep", "HUMAN_family")
  for (v in factor_vars) d[[v]] <- as.factor(d[[v]])
  d$h1_regents_avg <- as.numeric(d$h1_regents_avg)
  d$h1_ESSES_avg   <- as.numeric(d$h1_ESSES_avg)
  d
}

fit_study_37_model <- function(d) {
  lme4::glmer(STUDY37_FORMULA, family = binomial, data = d,
              control = lme4::glmerControl(optimizer = "bobyqa"), nAGQ = 10)
}

reproduce_study_37 <- function(path) {
  d   <- read_study_37_data(path)
  fit <- fit_study_37_model(d)
  cf  <- as.data.frame(summary(fit)$coefficients)   # Estimate, Std. Error, z value, Pr(>|z|)
  names(cf) <- c("estimate", "se", "z", "p")

  targets <- list(
    list(id = 50L, coef = "HUMAN_family1", reported = "Log-odds = 1.23, p < .001",
         contrast = "reporting family obligations (HUMAN_family = 1) vs not, on the log-odds of course withdrawal"),
    list(id = 51L, coef = "synchronous11", reported = "Log-odds = -1.11, p = .010",
         contrast = "synchronous (1) vs asynchronous (0) format, on the log-odds of course withdrawal"))

  form_txt <- paste(trimws(deparse(STUDY37_FORMULA)), collapse = " ")
  rows <- lapply(targets, function(t) {
    if (!t$coef %in% rownames(cf)) stop("Study 37: coefficient not found: ", t$coef, call. = FALSE)
    r <- cf[t$coef, ]
    tibble::tibble(
      id = t$id, study_id = "study_37", study_DOI = "10.1037/stl0000356",
      recomputation_status = "recomputed_from_raw_data",
      stat_test = "mixed_effects_logistic_regression",
      reported_result = t$reported, p_value = r$p, p_operator = "=", p_sidedness = "two_sided",
      t_value = NA_real_, t_df = NA_real_, f_value = NA_real_, f_df1 = NA_real_, f_df2 = NA_real_,
      z_value = r$z, chi2_value = NA_real_, chi2_df = NA_real_, r_value = NA_real_,
      n1 = NA_real_, n2 = NA_real_, n_total = nrow(d), n_eff = NA_real_,
      effect_size_type = "log_odds_coefficient", effect_size_value = r$estimate,
      estimate = r$estimate, se_estimate = r$se,
      raw_data_file = "data/raw/study_37/osf_data.csv",
      raw_variable_names = paste0("Attrition; section; ", t$coef, "; + covariates (see model_formula)"),
      model_formula = paste0(form_txt, "; family = binomial(logit); glmer, bobyqa, nAGQ = 10; Wald z for ", t$coef),
      contrast_direction = t$contrast,
      extraction_note = sprintf(
        "model4_att fixed effect %s: log-odds = %.4f (SE = %.4f), z = %.3f, p = %.6f, OR = %.3f. n = %d in %d sections.",
        t$coef, r$estimate, r$se, r$z, r$p, exp(r$estimate), nrow(d), lme4::ngrps(fit)[["section"]]))
  })
  dplyr::bind_rows(rows)
}

write_study_37_outputs <- function(results, output_path) {
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(results, output_path, na = "")
  invisible(results)
}
