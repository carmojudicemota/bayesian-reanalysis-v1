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

  bf_for_t_row <- function(t_val, df_val, n1, n2, rscale) {
    if (is.na(n2)) {
      bf <- meta.ttestBF(t = t_val, n1 = df_val + 1, rscale = rscale)
    } else {
      bf <- meta.ttestBF(t = t_val, n1 = n1, n2 = n2, rscale = rscale)
    }
    as.numeric(extractBF(bf)$bf)
  }

  bf_for_r_row <- function(r_val, n_val, rscale) {
    x <- rnorm(n_val); x <- (x - mean(x)) / sd(x)
    z <- rnorm(n_val); z <- (z - mean(z)) / sd(z)
    z_orth <- residuals(lm(z ~ x)); z_orth <- (z_orth - mean(z_orth)) / sd(z_orth)
    y <- r_val * x + sqrt(1 - r_val^2) * z_orth
    bf <- correlationBF(x, y, rscale = rscale)
    as.numeric(extractBF(bf)$bf)
  }

  results <- pmap_dfr(wave1, function(...) {
    row <- list(...)
    map_dfr(seq_len(nrow(priors)), function(i) {
      p <- priors[i, ]
      if (p$family != row$bf_family) return(NULL)
      bf10 <- if (row$bf_family == "correlation") {
        bf_for_r_row(row$r_value, row$n_total, p$rscale)
      } else {
        bf_for_t_row(row$t_for_bf, row$df_for_bf, row$n1, row$n2, p$rscale)
      }
      tibble(
        claim_id = row$claim_id,
        study_id = row$study_id,
        stat_test = row$frequentist_test,   
        bf_family = row$bf_family,
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
