# R/prepare/resolve_claim_directionality.R
#
# The reproduction layer (R/reproduce/*.R) stores whatever p-value/sidedness
# matches what the source article reported -- that's correct, it's a fidelity
# record, not an analysis decision. Bayesian reanalysis, though, must test the
# direction actually supported by the study's own pre-specified hypothesis
# (checked against Phase 1's "Claim trace: Hypothesis" column), which can
# differ from the article's own reporting choice.
#
# This is a general rule, not a per-claim patch: whenever a claim's resolved
# `direction` is "two_sided" but its source result was recomputed as
# "one_sided", recompute the exact two-sided p from the same t-statistic
# (2 * pt(-abs(t), df) is mathematically exact, not an approximation) rather
# than silently reusing the one-sided p in a two-sided analysis.

resolve_claim_directionality <- function(
    claims_path = "data/derived/claims_draft.csv",
    output_path = "data/derived/claims_draft.csv"
) {
  claims <- readr::read_csv(claims_path, show_col_types = FALSE)

  needs_columns <- c("direction", "p_sidedness", "p_value", "t_value", "t_df", "note")
  missing <- setdiff(needs_columns, names(claims))
  if (length(missing) > 0) {
    stop("resolve_claim_directionality: claims table is missing columns: ",
         paste(missing, collapse = ", "))
  }

  claims <- claims |>
    dplyr::mutate(
      p_value_original = p_value,
      needs_correction = direction == "two_sided" &
        p_sidedness == "one_sided" &
        !is.na(t_value) & !is.na(t_df),
      p_value = dplyr::if_else(
        needs_correction,
        2 * stats::pt(-abs(t_value), t_df),
        p_value
      ),
      note = dplyr::if_else(
        needs_correction,
        paste0(
          note, " | p_value corrected from one-sided (",
          signif(p_value_original, 4), ") to two-sided (",
          signif(p_value, 4), ") to match this claim's resolved direction."
        ),
        note
      )
    )

  n_corrected <- sum(claims$needs_correction, na.rm = TRUE)
  message("resolve_claim_directionality: corrected ", n_corrected, " claim(s).")

  claims <- dplyr::select(claims, -needs_correction)
  readr::write_csv(claims, output_path)
  invisible(claims)
}
