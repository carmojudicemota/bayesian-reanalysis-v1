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
  grid <- priors |>
    filter(
      prior_family == "anova_cauchy",
      param == "rscale_fixed"
    )
  purrr::pmap_dfr(
    grid,
    function(prior_label, value, ...) {
      bfs <- BayesFactor::generalTestBF(
        outcome ~ A * B,
        data = d,
        whichModels = "all",
        rscaleFixed = value,
        progress = FALSE
      )
      
      bf <- bfs[claim$model_alt] /
        bfs[claim$model_null]
      
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
    }
  )
}

run_family_B <- function(claim, priors) {
  stop(
    "No generic mixed-ANOVA handler is permitted. Register a study-specific ",
    "handler for ", claim$claim_id, " before changing it to ready.",
    call. = FALSE
  )
}
