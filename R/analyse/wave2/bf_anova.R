library(BayesFactor)
library(dplyr)
library(purrr)

extract_named_bf <- function(bfs, model_alt, model_null, claim_id) {
  available <- names(bfs)
  missing <- setdiff(c(model_alt, model_null), available)
  if (length(missing) > 0) {
    stop(
      "Model name(s) not returned for ", claim_id, ": ",
      paste(missing, collapse = ", "),
      ". Available models: ", paste(available, collapse = " | "),
      call. = FALSE
    )
  }
  bfs[model_alt] / bfs[model_null]
}

run_family_A <- function(claim, priors) {
  d <- load_wave2_data(
    study_id = claim$study_id,
    outcome_col = claim$outcome_column
  ) |>
    as.data.frame()
  
  if (!all(c("outcome", "A", "B") %in% names(d))) {
    stop("Family A loader must return outcome, A, and B for ", claim$claim_id,
         call. = FALSE)
  }
  
  grid <- priors |>
    filter(prior_family == "anova_cauchy", param == "rscale_fixed")
  
  pmap_dfr(grid, function(prior_label, value, ...) {
    bfs <- BayesFactor::generalTestBF(
      outcome ~ A * B,
      data = d,
      whichModels = "all",
      rscaleFixed = value,
      progress = FALSE
    )
    
    bf <- extract_named_bf(
      bfs = bfs,
      model_alt = claim$model_alt,
      model_null = claim$model_null,
      claim_id = claim$claim_id
    )
    
    tab <- BayesFactor::extractBF(bf)
    
    wave2_row(
      claim = claim,
      prior_label = prior_label,
      rscale = value,
      bf10 = as.numeric(tab$bf)[1],
      prior_family = "anova_cauchy",
      model_null = claim$model_null,
      model_alt = claim$model_alt,
      bf_error = as.numeric(tab$error)[1]
    )
  })
}

run_family_B <- function(claim, priors) {
  stop(
    "No generic mixed-ANOVA handler is permitted. Register a study-specific ",
    "handler for ", claim$claim_id, " before changing it to ready.",
    call. = FALSE
  )
}
