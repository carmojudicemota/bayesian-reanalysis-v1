library(readr); library(dplyr); library(purrr); library(tidyr)
source("R/analyse/wave2/wave2_helpers.R")
source("R/analyse/wave2/load_wave2_data.R")
source("R/analyse/wave2/bf_anova.R")
source("R/analyse/wave2/bf_glmm.R")

compute_wave2_bayes_factors <- function(
    claims_path = "data/derived/claims.csv",
    priors_path = "config/priors_wave2.csv",
    pairs_path  = "config/wave2_model_pairs.csv",
    output_path = "outputs/tables/bayes_factor_results_wave2.csv") {

  claims <- read_csv(claims_path, show_col_types = FALSE)
  priors <- read_csv(priors_path, show_col_types = FALSE)
  pairs  <- if (file.exists(pairs_path)) read_csv(pairs_path, show_col_types = FALSE)
            else tibble(claim_id = character(), outcome_column = character(),
                        model_null = character(), model_alt = character())

  wave2_families <- c("factorial_between_anova", "mixed_anova",
                      "welch_independent_t_test", "mixed_effects_logistic_regression")
  wave2 <- claims |>
    filter(status == "ready", in_scope, frequentist_test %in% wave2_families) |>
    mutate(
      bf_sidedness = if_else(!is.na(p_sidedness) & p_sidedness == "one_sided",
                             "one_sided", "two_sided"),
      bf_direction = if_else(bf_sidedness == "one_sided", direction, NA_character_)) |>
    left_join(pairs, by = "claim_id")

  needs_pair <- wave2 |>
    filter(frequentist_test %in% c("factorial_between_anova", "mixed_anova"),
           is.na(model_alt) | is.na(model_null))
  if (nrow(needs_pair) > 0)
    stop("No model pair in config/wave2_model_pairs.csv for: ",
         paste(needs_pair$claim_id, collapse = ", "), call. = FALSE)

  if (nrow(wave2) == 0) {
    message("compute_wave2_bayes_factors: no ready Wave 2 claims; nothing written.")
    return(invisible(NULL))
  }

  handler_of <- c(factorial_between_anova = "anova_cauchy", mixed_anova = "anova_cauchy",
                  welch_independent_t_test = "welch_averaged",
                  mixed_effects_logistic_regression = "glmm_normal")
  missing <- setdiff(unique(handler_of[wave2$frequentist_test]), priors$prior_family)
  if (length(missing) > 0)
    stop("No priors in priors_wave2.csv for: ", paste(missing, collapse = ", "), call. = FALSE)

  results <- pmap_dfr(wave2, function(...) {
    claim <- list(...)
    switch(claim$frequentist_test,
      factorial_between_anova = run_family_A(claim, priors),
      mixed_anova = run_family_B(claim, priors),
      welch_independent_t_test = { source("R/analyse/wave2/bf_welch_t.R"); run_family_C(claim, priors) },
      mixed_effects_logistic_regression = run_family_D(claim, priors),
      stop("No Wave 2 handler for ", claim$claim_id, call. = FALSE))
  })

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  write_csv(results, output_path)
  invisible(results)
}
