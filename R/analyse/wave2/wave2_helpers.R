wave2_result_template <- function() {
  tibble::tibble(
    claim_id = character(),
    study_id = character(),
    stat_test = character(),
    bf_family = character(),
    design = character(),
    analysis_role = character(),
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
    bf_error = double(),
    prior_family = character(),
    model_null = character(),
    model_alt = character()
  )
}

wave2_row <- function(
    claim,
    prior_label,
    rscale,
    bf10,
    prior_family,
    model_null,
    model_alt,
    bf_error = NA_real_) {
  
  bf10 <- as.numeric(bf10)
  
  if (
    length(bf10) != 1L ||
    !is.finite(bf10) ||
    bf10 <= 0
  ) {
    stop(
      "Invalid BF10 for ",
      claim$claim_id,
      ": ",
      paste(bf10, collapse = ", "),
      call. = FALSE
    )
  }
  
  tibble::tibble(
    claim_id = claim$claim_id,
    study_id = claim$study_id,
    stat_test = claim$frequentist_test,
    bf_family = prior_family,
    design = "wave2",
    analysis_role = "primary",
    bf_sidedness = claim$bf_sidedness,
    bf_direction = claim$bf_direction,
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
    bf_error = as.numeric(bf_error),
    prior_family = prior_family,
    model_null = model_null,
    model_alt = model_alt
  )
}

prior_grid <- function(
    priors,
    prior_family,
    param) {
  
  grid <- priors[
    priors$prior_family == prior_family &
      priors$param == param,
    c("prior_label", "value"),
    drop = FALSE
  ]
  
  expected <- c(
    "narrow",
    "primary",
    "wide"
  )
  
  missing <- setdiff(
    expected,
    grid$prior_label
  )
  
  if (length(missing) > 0L) {
    stop(
      "Missing ",
      prior_family,
      " prior rows for: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  
  grid[
    match(expected, grid$prior_label),
    ,
    drop = FALSE
  ]
}

prior_value <- function(
    priors,
    prior_family,
    param,
    prior_label = "primary") {
  
  value <- priors$value[
    priors$prior_family == prior_family &
      priors$param == param &
      priors$prior_label == prior_label
  ]
  
  if (length(value) != 1L) {
    stop(
      "Expected one ",
      prior_family,
      " / ",
      param,
      " / ",
      prior_label,
      " prior value.",
      call. = FALSE
    )
  }
  
  as.numeric(value)
}

record_mcmc_diagnostics <- function(
    claim_id,
    prior_label,
    rhat_max,
    ess_min,
    max_mcmc_error = NA_real_,
    max_error_sd = NA_real_,
    messages = character(),
    path = "outputs/tables/wave2_mcmc_diagnostics.csv") {
  
  messages <- as.character(messages)
  
  passed <- (
    is.finite(rhat_max) &&
      rhat_max < 1.01 &&
      is.finite(ess_min) &&
      ess_min > 400 &&
      length(messages) == 0L
  )
  
  row <- tibble::tibble(
    claim_id = claim_id,
    prior_label = prior_label,
    rhat_max = rhat_max,
    ess_min = ess_min,
    max_mcmc_error = max_mcmc_error,
    max_error_sd = max_error_sd,
    diagnostic_messages = paste(
      messages,
      collapse = " | "
    ),
    passed = passed
  )
  
  dir.create(
    dirname(path),
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  if (file.exists(path)) {
    existing <- readr::read_csv(
      path,
      show_col_types = FALSE
    )
    
    same_row <- (
      existing$claim_id == claim_id &
        existing$prior_label == prior_label
    )
    
    same_row[is.na(same_row)] <- FALSE
    
    existing <- existing[
      !same_row,
      ,
      drop = FALSE
    ]
  } else {
    existing <- row[0, ]
  }
  
  readr::write_csv(
    dplyr::bind_rows(
      existing,
      row
    ),
    path
  )
  
  if (!passed) {
    stop(
      "MCMC diagnostics failed for ",
      claim_id,
      " under the ",
      prior_label,
      " prior.",
      if (length(messages) > 0L) {
        paste0(
          " ",
          paste(messages, collapse = "; ")
        )
      } else {
        ""
      },
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}