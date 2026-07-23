library(readr)
library(dplyr)
library(purrr)
library(tidyr)

source("R/analyse/wave2/wave2_helpers.R")
source("R/analyse/wave2/load_wave2_data.R")
source("R/analyse/wave2/bf_anova.R")

compute_wave2_bayes_factors <- function(
    claims_path = "data/derived/claims.csv",
    priors_path = "config/priors_wave2.csv",
    specs_path = "config/wave2_claim_specs.csv",
    output_path = "outputs/tables/bayes_factor_results_wave2.csv") {
  
  claims <- readr::read_csv(claims_path, show_col_types = FALSE)
  priors <- readr::read_csv(priors_path, show_col_types = FALSE)
  specs <- readr::read_csv(specs_path, show_col_types = FALSE)
  
  duplicate_specs <- specs$claim_id[duplicated(specs$claim_id)]
  if (length(duplicate_specs) > 0) {
    stop("Duplicate claim IDs in wave2_claim_specs.csv: ",
         paste(unique(duplicate_specs), collapse = ", "), call. = FALSE)
  }
  
  wave2 <- claims |>
    filter(status == "ready", in_scope) |>
    inner_join(specs, by = "claim_id") |>
    filter(activation_state == "active") |>
    mutate(
      bf_sidedness = if_else(
        !is.na(p_sidedness) & p_sidedness == "one_sided",
        "one_sided",
        "two_sided"
      ),
      bf_direction = if_else(
        bf_sidedness == "one_sided",
        direction,
        NA_character_
      )
    )
  
  if (nrow(wave2) == 0) {
    message("compute_wave2_bayes_factors: no active, ready Wave 2 claims; nothing written.")
    return(invisible(NULL))
  }
  
  results <- purrr::pmap_dfr(wave2, function(...) {
    claim <- list(...)
    
    switch(
      claim$handler,
      factorial_additive = run_family_A(claim, priors),
      welch_model_averaged = {
        source("R/analyse/wave2/bf_welch_t.R", local = TRUE)
        run_family_C(claim, priors)
      },
      stop("No active Wave 2 handler for ", claim$claim_id,
           " (handler = ", claim$handler, ")", call. = FALSE)
    )
  })
  
  key_counts <- results |>
    count(claim_id, prior_label, name = "n") |>
    filter(n != 1)
  if (nrow(key_counts) > 0) {
    stop("Wave 2 output contains duplicate claim/prior rows.", call. = FALSE)
  }
  
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(results, output_path)
  invisible(results)
}
