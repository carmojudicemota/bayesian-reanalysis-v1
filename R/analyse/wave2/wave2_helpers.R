library(tibble)
library(readr)

wave2_row <- function(claim, prior_label, rscale, bf10, prior_family,
                      model_null, model_alt, bf_error = NA_real_) {
  tibble::tibble(
    claim_id = claim$claim_id,
    study_id = claim$study_id,
    stat_test = claim$frequentist_test,
    bf_family = prior_family,
    design = "wave2",
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

record_mcmc_diagnostics <- function(claim_id, rhat_max, ess_min, divergences, ppc_ok,
                                    path = "outputs/tables/wave2_mcmc_diagnostics.csv") {
  ok <- rhat_max < 1.01 && ess_min > 400 && divergences == 0 && isTRUE(ppc_ok)
  readr::write_csv(
    tibble::tibble(claim_id, rhat_max, ess_min, divergences, ppc_ok, passed = ok),
    path, append = file.exists(path))
  if (!ok) stop("MCMC checks failed for ", claim_id, call. = FALSE)
  invisible(ok)
}

