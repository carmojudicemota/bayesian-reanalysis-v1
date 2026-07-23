library(tibble)
library(readr)

wave2_row <- function(claim, prior_label, rscale, bf10, prior_family,
                      model_null, model_alt, bf_error = NA_real_,
                      analysis_role = "primary") {
  if (!is.finite(bf10) || bf10 <= 0) {
    stop("Invalid BF10 for ", claim$claim_id, ": ", bf10, call. = FALSE)
  }
  
  tibble::tibble(
    claim_id = claim$claim_id,
    study_id = claim$study_id,
    stat_test = claim$frequentist_test,
    bf_family = prior_family,
    design = "wave2",
    analysis_role = analysis_role,
    bf_sidedness = claim$bf_sidedness,
    bf_direction = claim$bf_direction,
    observed_sign = NA_character_,
    direction_matches_observed = NA,
    prior_label = prior_label,
    rscale = rscale,
    t_for_bf = NA_real_,
    df_for_bf = NA_real_,
    r_value = NA_real_,
    n1 = NA_real_,
    n2 = NA_real_,
    n_total = claim$n_total,
    p_value = claim$p_value,
    bf10 = bf10,
    bf_error = bf_error,
    prior_family = prior_family,
    model_null = model_null,
    model_alt = model_alt
  )
}

record_mcmc_diagnostics <- function(claim_id, prior_label, rhat_max, ess_min,
                                    divergences, ppc_ok,
                                    path = "outputs/tables/wave2_mcmc_diagnostics.csv") {
  ok <- is.finite(rhat_max) && rhat_max < 1.01 &&
    is.finite(ess_min) && ess_min > 400 &&
    identical(as.integer(divergences), 0L) &&
    isTRUE(ppc_ok)
  
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  
  row <- tibble::tibble(
    claim_id = claim_id,
    prior_label = prior_label,
    rhat_max = rhat_max,
    ess_min = ess_min,
    divergences = divergences,
    ppc_ok = ppc_ok,
    passed = ok
  )
  
  existing <- if (file.exists(path)) {
    readr::read_csv(path, show_col_types = FALSE) |>
      dplyr::filter(!(claim_id == !!claim_id & prior_label == !!prior_label))
  } else {
    row[0, ]
  }
  
  readr::write_csv(dplyr::bind_rows(existing, row), path)
  
  if (!ok) {
    stop("MCMC checks failed for ", claim_id, " under prior ", prior_label,
         call. = FALSE)
  }
  
  invisible(ok)
}
