wave2_result_template <- function() {
  tibble::tibble(
    claim_id = character(),
    study_id = character(),
    stat_test = character(),
    bf_family = character(),
    design = character(),
    bf_sidedness = character(),
    bf_direction = character(),
    observed_sign = character(),
    direction_matches_observed = logical(),
    prior_label = character(),
    rscale = double(),
    t_for_bf = double(),
    df_for_bf = double(),
    r_value = double(),
    n1 = double(),
    n2 = double(),
    n_total = double(),
    p_value = double(),
    bf10 = double(),
    log_bf10 = double(),
    log10_bf10 = double(),
    bf_error = double(),
    prior_family = character(),
    model_null = character(),
    model_alt = character(),
    method = character()
  )
}


wave2_row <- function(
    claim,
    prior_label,
    rscale,
    bf10,
    bf_error,
    model_null,
    model_alt,
    bf_family = "anova_cauchy",
    prior_family = "anova_cauchy",
    method = "bayesfactor_model_comparison",
    log_bf10 = NULL,
    log10_bf10 = NULL) {
  
  bf10 <- as.numeric(bf10)
  bf_error <- as.numeric(bf_error)
  
  if (length(bf10) != 1L || is.na(bf10) || bf10 <= 0) {
    stop("Invalid BF10 for ",claim$claim_id,": ",paste(bf10,collapse = ", "),call. = FALSE)
  }
  
  if (is.null(log_bf10)) {
    log_bf10 <- if (is.finite(bf10)) {log(bf10)} else {NA_real_}
  }
  
  log_bf10 <- as.numeric(log_bf10)
  
  if (is.null(log10_bf10)) {
    log10_bf10 <- if (length(log_bf10) == 1L &&is.finite(log_bf10)) {
      log_bf10 /log(10)
    } else if (is.finite(bf10)
    ) {log10(bf10)
    } else {NA_real_}
  }
  
  log10_bf10 <- as.numeric(log10_bf10)
  if (length(log_bf10) != 1L || length(log10_bf10) != 1L) {
    stop("Study ",claim$claim_id," must provide one log Bayes factor.",call. = FALSE)
  }
  
  if (!is.finite(bf10) && (!is.finite(log_bf10) ||!is.finite(log10_bf10))
  ) {
    stop("A non-finite BF10 for ",claim$claim_id," requires finite log_bf10 and log10_bf10.",call. = FALSE)
  }
  
  tibble::tibble(claim_id =claim$claim_id,
                 study_id =claim$study_id,
                 stat_test = claim$frequentist_test,
                 bf_family =bf_family,
                 design ="wave2",
                 bf_sidedness ="two_sided",
                 bf_direction =NA_character_,
                 observed_sign = NA_character_,
                 direction_matches_observed = NA,
                 prior_label = prior_label,
                 rscale = as.numeric(rscale),
                 t_for_bf = NA_real_,
                 df_for_bf = NA_real_,
                 r_value = NA_real_,
                 n1 = as.numeric(claim$n1),
                 n2 = as.numeric(claim$n2),
                 n_total = as.numeric(claim$n_total),
                 p_value = as.numeric(claim$p_value),
                 bf10 = bf10,
                 log_bf10 = log_bf10, 
                 log10_bf10 = log10_bf10,
                 bf_error = bf_error,
                 prior_family = prior_family,
                 model_null = model_null,
                 model_alt =model_alt,   
                 method =method
                 )
}