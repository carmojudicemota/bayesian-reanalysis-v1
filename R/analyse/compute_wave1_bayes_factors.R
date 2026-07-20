library(readr)
library(dplyr)
library(purrr)
library(BayesFactor)

compute_wave1_bayes_factors <- function(
    claims_path = "data/derived/claims.csv",
    priors_path = "config/priors.csv",
    output_path = "outputs/tables/bayes_factor_results.csv"
) {
  claims <- read_csv(claims_path, show_col_types = FALSE)
  priors <- read_csv(priors_path, show_col_types = FALSE)

  # SCOPE RULE: only results Almuhanna marked "Fully reproducible" are analysed.
  # `in_scope` is built in build_claims_draft(); older claims tables may lack it,
  # so fall back to the underlying status rather than silently analysing everything.
  if (!"in_scope" %in% names(claims)) {
    claims$in_scope <- !is.na(claims$ali_result_status) &
      trimws(claims$ali_result_status) == "Fully reproducible"
  }

  wave1 <- claims |>
    filter(status == "ready", in_scope) |>
    filter(frequentist_test %in% c(
      "one_sample_t_test", "paired_t_test",
      "independent_t_test", "repeated_measures_anova",
      "pearson_correlation"
    )) |>
    mutate(
      bf_family = if_else(frequentist_test == "pearson_correlation", "correlation", "t_test"),
      t_for_bf = if_else(
        frequentist_test == "repeated_measures_anova" & is.na(t_value) & !is.na(f_value),
        sqrt(f_value), t_value
      ),
      df_for_bf = if_else(
        frequentist_test == "repeated_measures_anova" & is.na(t_df) & !is.na(f_df2),
        f_df2, t_df
      ),
      # Policy: sidedness follows the ORIGINAL PUBLISHED TEST. If the article
      # reported a one-sided p, we keep that p and compute a one-sided (order-
      # restricted) Bayes factor, so p and BF are always on the same footing.
      bf_sidedness = if_else(
        !is.na(p_sidedness) & p_sidedness == "one_sided",
        "one_sided", "two_sided"
      ),
      # Orientation of a one-sided test comes from the RECORDED direction of the
      # original hypothesis (config/claim_map.csv), never from the sign of our own
      # recomputed statistic. Picking the tail from the observed sign would be
      # data-dependent hypothesis selection: it forces the one-sided BF to be at
      # least the two-sided BF and makes a wrong-signed result impossible to
      # penalise, which is exactly what a directional prediction should risk.
      bf_direction = if_else(bf_sidedness == "one_sided", direction, NA_character_),
      observed_sign = case_when(
        bf_family == "correlation" & !is.na(r_value) & r_value < 0 ~ "negative",
        bf_family == "correlation" & !is.na(r_value)               ~ "positive",
        !is.na(t_for_bf) & t_for_bf < 0                            ~ "negative",
        !is.na(t_for_bf)                                           ~ "positive",
        TRUE                                                       ~ NA_character_
      ),
      direction_matches_observed = if_else(
        bf_sidedness == "one_sided" & !is.na(bf_direction) & !is.na(observed_sign),
        bf_direction == observed_sign,
        NA
      )
    )

  # A one-sided Bayes factor needs an explicitly recorded direction. If the
  # reproduced result is one-sided but claim_map does not say which tail the
  # original study predicted, stop: that gap must be resolved from the article,
  # not inferred from our own data.
  undirected <- wave1$bf_sidedness == "one_sided" &
    !(wave1$bf_direction %in% c("positive", "negative"))
  if (any(undirected)) {
    stop(
      "One-sided claims with no recorded direction. Set `direction` to ",
      "'positive' or 'negative' in config/claim_map.csv for: ",
      paste(wave1$claim_id[undirected], collapse = ", "),
      call. = FALSE
    )
  }

  # Faithfulness check. If the recorded direction disagrees with the sign we
  # recomputed, that is a finding ABOUT the source study, not a reason to switch
  # tails. Report it and carry it in the output; the recorded direction is used.
  mismatched <- which(!is.na(wave1$direction_matches_observed) &
                        !wave1$direction_matches_observed)
  if (length(mismatched) > 0) {
    warning(
      "Recorded hypothesis direction disagrees with the recomputed sign for: ",
      paste(wave1$claim_id[mismatched], collapse = ", "),
      ". The recorded direction is used; record the discrepancy in the claim note.",
      call. = FALSE
    )
  }
  bad_rm <- wave1$frequentist_test == "repeated_measures_anova" &
    !is.na(wave1$f_df1) & wave1$f_df1 != 1
  if (any(bad_rm)) {
    stop(
      "repeated_measures_anova rows with f_df1 != 1 cannot be converted via sqrt(F): ",
      paste(wave1$claim_id[bad_rm], collapse = ", "),
      call. = FALSE
    )
  }
  # direction: NA = two-sided; "positive" = c(0, Inf); "negative" = c(-Inf, 0).
  bf_for_t_row <- function(t_val, df_val, n1, n2, rscale, direction = NA_character_) {
    null_interval <- if (is.na(direction)) NULL else
      if (identical(direction, "positive")) c(0, Inf) else c(-Inf, 0)
    if (is.na(n2)) {
      bf <- meta.ttestBF(t = t_val, n1 = df_val + 1, rscale = rscale,
                         nullInterval = null_interval)
    } else {
      bf <- meta.ttestBF(t = t_val, n1 = n1, n2 = n2, rscale = rscale,
                         nullInterval = null_interval)
    }
    as.numeric(extractBF(bf)$bf)[1]
  }

  bf_for_r_row <- function(r_val, n_val, rscale, direction = NA_character_) {
    # Deterministic construction: with x and z_orth orthonormal, cor(x, y) equals
    # r_val exactly, so the Bayes factor depends only on (r_val, n_val) and not on
    # the draw. The seed makes that reproducible rather than merely provable.
    set.seed(20260720L)
    x <- rnorm(n_val); x <- (x - mean(x)) / sd(x)
    z <- rnorm(n_val); z <- (z - mean(z)) / sd(z)
    z_orth <- residuals(lm(z ~ x)); z_orth <- (z_orth - mean(z_orth)) / sd(z_orth)
    y <- r_val * x + sqrt(1 - r_val^2) * z_orth
    achieved <- stats::cor(x, y)
    if (!isTRUE(all.equal(achieved, r_val, tolerance = 1e-8))) {
      stop("Synthetic correlation construction failed: requested r = ", r_val,
           " but achieved r = ", achieved, call. = FALSE)
    }
    null_interval <- if (is.na(direction)) NULL else
      if (identical(direction, "positive")) c(0, 1) else c(-1, 0)
    bf <- correlationBF(x, y, rscale = rscale, nullInterval = null_interval)
    as.numeric(extractBF(bf)$bf)[1]
  }

  results <- pmap_dfr(wave1, function(...) {
    row <- list(...)
    map_dfr(seq_len(nrow(priors)), function(i) {
      p <- priors[i, ]
      if (p$family != row$bf_family) return(NULL)
      dir_for_bf <- if (identical(row$bf_sidedness, "one_sided")) {
        row$bf_direction
      } else {
        NA_character_
      }
      bf10 <- if (row$bf_family == "correlation") {
        bf_for_r_row(row$r_value, row$n_total, p$rscale, direction = dir_for_bf)
      } else {
        bf_for_t_row(row$t_for_bf, row$df_for_bf, row$n1, row$n2, p$rscale,
                     direction = dir_for_bf)
      }
      tibble(
        claim_id = row$claim_id,
        study_id = row$study_id,
        stat_test = row$frequentist_test,
        bf_family = row$bf_family,
        bf_sidedness = row$bf_sidedness,
        bf_direction = row$bf_direction,
        direction_matches_observed = row$direction_matches_observed,
        prior_label = p$prior_label,
        rscale = p$rscale,
        p_value = row$p_value,
        bf10 = bf10
      )
    })
  })

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  write_csv(results, output_path)
  invisible(results)
}
