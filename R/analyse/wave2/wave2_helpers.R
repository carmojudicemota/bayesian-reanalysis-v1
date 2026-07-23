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

