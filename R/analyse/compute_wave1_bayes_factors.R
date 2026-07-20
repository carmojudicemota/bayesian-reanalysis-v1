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

  wave1 <- claims |>
    filter(status == "ready") |>
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
      )
    )

  # Guard (#2): sqrt(F) -> t is only valid when the F numerator has 1 df
  # (a 2-level repeated-measures ANOVA, F(1, nu) = t^2). A multi-df F must never
  # be silently square-rooted into a t. Stop loudly if any such row slipped in.
  bad_rm <- wave1$frequentist_test == "repeated_measures_anova" &
    !is.na(wave1$f_df1) & wave1$f_df1 != 1
  if (any(bad_rm)) {
    stop(
      "repeated_measures_anova rows with f_df1 != 1 cannot be converted via sqrt(F): ",
      paste(wave1$claim_id[bad_rm], collapse = ", "),
      call. = FALSE
    )
  }

  # One-sided BFs are order-restricted to the side the observed effect falls on,
  # which is the side the article's one-sided test was run on (verifiable: a
  # published one-sided p equals the two-sided p / 2). extractBF()$bf[1] is the
  # restricted hypothesis; [2] would be its complement.
  bf_for_t_row <- function(t_val, df_val, n1, n2, rscale, one_sided = FALSE) {
    null_interval <- if (!one_sided) NULL else
      if (!is.na(t_val) && t_val >= 0) c(0, Inf) else c(-Inf, 0)
    if (is.na(n2)) {
      bf <- meta.ttestBF(t = t_val, n1 = df_val + 1, rscale = rscale,
                         nullInterval = null_interval)
    } else {
      bf <- meta.ttestBF(t = t_val, n1 = n1, n2 = n2, rscale = rscale,
                         nullInterval = null_interval)
    }
    as.numeric(extractBF(bf)$bf)[1]
  }

  bf_for_r_row <- function(r_val, n_val, rscale, one_sided = FALSE) {
    x <- rnorm(n_val); x <- (x - mean(x)) / sd(x)
    z <- rnorm(n_val); z <- (z - mean(z)) / sd(z)
    z_orth <- residuals(lm(z ~ x)); z_orth <- (z_orth - mean(z_orth)) / sd(z_orth)
    y <- r_val * x + sqrt(1 - r_val^2) * z_orth
    null_interval <- if (!one_sided) NULL else
      if (!is.na(r_val) && r_val >= 0) c(0, 1) else c(-1, 0)
    bf <- correlationBF(x, y, rscale = rscale, nullInterval = null_interval)
    as.numeric(extractBF(bf)$bf)[1]
  }

  results <- pmap_dfr(wave1, function(...) {
    row <- list(...)
    map_dfr(seq_len(nrow(priors)), function(i) {
      p <- priors[i, ]
      if (p$family != row$bf_family) return(NULL)
      one_sided <- identical(row$bf_sidedness, "one_sided")
      bf10 <- if (row$bf_family == "correlation") {
        bf_for_r_row(row$r_value, row$n_total, p$rscale, one_sided = one_sided)
      } else {
        bf_for_t_row(row$t_for_bf, row$df_for_bf, row$n1, row$n2, p$rscale,
                     one_sided = one_sided)
      }
      tibble(
        claim_id = row$claim_id,
        study_id = row$study_id,
        stat_test = row$frequentist_test,
        bf_family = row$bf_family,
        bf_sidedness = row$bf_sidedness,
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
