library(readr)
library(dplyr)
library(ggplot2)

classify_concordance <- function(
    bf_path = "outputs/tables/bayes_factor_results.csv",
    alpha = 0.05,
    k = 3,
    output_table_path = "outputs/tables/concordance_claim_level.csv",
    output_plot_path = "outputs/figures/evidence_plane.png"
) {
  bf <- read_csv(bf_path, show_col_types = FALSE)

  primary <- bf |>
    filter(prior_label == "primary") |>
    mutate(
      sig_freq = p_value < alpha,
      bf_band = case_when(
        bf10 > k     ~ "supports_H1",
        bf10 < 1 / k ~ "supports_H0",
        TRUE         ~ "inconclusive"
      ),
      cell = paste0(
        if_else(sig_freq, "p<alpha", "p>=alpha"), " x ", bf_band
      ),
      concordant = (sig_freq & bf_band == "supports_H1") |
                   (!sig_freq & bf_band == "supports_H0"),
      x_c = -log10(p_value),
      y_c = log10(bf10)
    )

  # Prior sensitivity span: range of log10(BF10) across narrow/primary/wide
  # for each claim, per the diary's continuous-evidence-plane definition.
  span <- bf |>
    group_by(claim_id) |>
    summarise(prior_sensitivity_span = max(log10(bf10)) - min(log10(bf10)), .groups = "drop")

  claim_level <- primary |>
    left_join(span, by = "claim_id") |>
    select(claim_id, study_id, stat_test, p_value, bf10, cell, bf_band,
           concordant, x_c, y_c, prior_sensitivity_span)

  dir.create(dirname(output_table_path), recursive = TRUE, showWarnings = FALSE)
  write_csv(claim_level, output_table_path)

  ref_x <- -log10(alpha)
  ref_y <- log10(k)

  plot <- ggplot(claim_level, aes(x = x_c, y = y_c, colour = cell)) +
    geom_point(size = 2.5, alpha = 0.85) +
    geom_hline(yintercept = c(-ref_y, ref_y), linetype = "dashed", colour = "grey40") +
    geom_vline(xintercept = ref_x, linetype = "dashed", colour = "grey40") +
    labs(
      x = expression(-log[10](p)),
      y = expression(log[10](BF[10])),
      colour = "Cell",
      title = "Continuous evidence-concordance plane (primary prior)"
    ) +
    theme_minimal()

  dir.create(dirname(output_plot_path), recursive = TRUE, showWarnings = FALSE)
  ggsave(output_plot_path, plot, width = 7, height = 5, dpi = 300)

  invisible(claim_level)
}
