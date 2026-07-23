library(ggplot2)
build_detailed_rank_table <- function(claim_level) {
  claim_level |>
    select(
      claim_id, study_id, stat_test, p_value, p_band, frequentist_result, bf10, bf_strength,
      favoured_side, concordance_cell, concordance_status, negative_log10_p, log10_bf10,
      prior_sensitivity_span
    ) |>
    arrange(desc(log10_bf10))
}
plot_concordace_squares <- function(
    in_path = "outputs/tables/concordance_summary.csv",
    out_path = "outputs/figures/concordance_squares.png") {
  library(dplyr); library(tidyr)
  d <- readr::read_csv(in_path, show_col_types = FALSE) |>
    tidyr::separate(concordance_cell, c("sig", "bf"), sep = " \\+ ", remove = FALSE) |>
    mutate(bf  = recode(bf, "inconclusive" = "Inconclusive"),
           sig = factor(sig, c("Nonsignificant", "Significant")),
           bf  = factor(bf,  c("H0", "Inconclusive", "H1")),
           status = case_when(
             concordance_cell %in% c("Significant + H1", "Nonsignificant + H0") ~ "Concordant",
             grepl("inconclusive", concordance_cell)                            ~ "Inconclusive",
             TRUE ~ "Discordant"))
  p <- ggplot(d, aes(bf, sig, fill = status, alpha = claim_proportion)) +
    geom_tile(colour = "white", linewidth = 3) +
    geom_text(aes(label = ifelse(claim_count > 0, paste0(claim_count, "\n", round(claim_proportion*100), "%"),"")),
              fontface = "bold", size = 5, alpha = 1)+
    scale_fill_manual(values = concordance_colours) +
    labs(x = "Bayes Factor Conclusion", y = NULL , fill = NULL,
         title = "3x2 Concordance Scheme") +
    guides(alpha = "none") +
    theme_reanalysis() + theme(panel.grid = element_blank(),
                               plot.title = element_text(hjust = 0.5))
  save_fig(p,out_path,w = 8, h = 4.5)
}
plot_evidence_plan <- function(
    in_path = "outputs/tables/concordance_claim_level.csv",
    out_path = "outputs/figures/evidence_plan.png",
    alpha = alpha_default, k = k_default) {
  library(dplyr)
  cl <- readr::read_csv(in_path, show_col_types = FALSE)
  sbb <- function(x) log10(-1 / (exp(1) * 10^(-x) * log(10^(-x))))
  p <- ggplot(cl, aes(negative_log10_p, log10_bf10, colour = concordance_status)) +
    annotate("rect", xmin=-Inf, xmax =Inf, ymin=log10(k), ymax=Inf,fill=tint_blue)+
    annotate("rect", xmin=-Inf, xmax =Inf, ymin=-Inf, ymax=-log10(k),fill=tint_red)+
    stat_function(fun = sbb, colour = blue_med, linetype = "dashed",
                  linewidth = 0.8, xlim= c(0.44,max(cl$negative_log10_p))) +
    geom_hline(yintercept = c(-log10(k),log10(k)), linetype = "dashed", colour = "grey40")+
    geom_vline(xintercept = -log10(alpha), linetype = "dashed", colour = "grey40")+
    geom_hline(yintercept = 0, linewidth = 0.3)+
    geom_point(size = 3) +
    scale_colour_manual(values = concordance_colours) +
    coord_cartesian(ylim = c(-1.3, 7.3)) +
    labs(x = expression(-log[10](p)), y = expression(log[10](BF[10])),
         colour = NULL, title = "Frequentist-Bayesian Evidence Plane") +
    theme_reanalysis()
  save_fig(p,out_path,w=8,h=5.5)
}
plot_detailed_rank <- function(
    in_path = "outputs/tables/concordance_claim_level.csv",
    out_path = "outputs/figures/detailed_rank.png") {
  library(dplyr); library(forcats)
  d <- readr::read_csv(in_path, show_col_types = FALSE) |>
    mutate(claim_id = fct_reorder(claim_id, log10_bf10))
  p <- ggplot(d, aes(log10_bf10, claim_id)) +
    geom_vline(xintercept = log10(jeffreys_bf), colour = "grey88", linewidth = 0.3) +
    geom_vline(xintercept = 0, colour = "black", linewidth = 0.4) +
    geom_point(aes(colour = concordance_status), size = 3) +
    geom_text(aes(label = p_band), hjust = -0.15, size = 2.6, colour = "grey30") +
    scale_colour_manual(values = concordance_colours) +
    labs(x = expression(log[10](BF[10])~"(primary prior)"), y = NULL,
         colour = NULL, title = "Detailed Evidence Rank") +
    theme_reanalysis() + theme(panel.grid = element_blank())
  save_fig(p, out_path, w = 9, h = max(4, 0.35 * nrow(d) + 1.5))
}
plot_prior_sensitivity <-function(
    in_path = "outputs/tables/bayes_factor_results.csv",
    out_path = "outputs/figures/prior_sensitivity.png") {
  library(dplyr); library(tidyr); library(forcats)
  d <- readr::read_csv(in_path,show_col_types = FALSE) |>
    mutate(log10_bf10 = log10(bf10)) |>
    group_by(claim_id) |>
    mutate(primary_lbf = log10_bf10[prior_label == "primary"]) |>
    ungroup() |>
    mutate(claim_id = fct_reorder(claim_id, primary_lbf))
  spans <- d |> group_by(claim_id) |>
    summarise(lo = min(log10_bf10), hi = max(log10_bf10), .groups = "drop")
  p <- ggplot() +
    geom_vline(xintercept = c(log10(3), log10(1/3)), linetype = "dashed", colour = red_main) +
    geom_vline(xintercept = 0, linewidth = 0.4) +
    geom_segment(data = spans, aes(x = lo, xend = hi, y = claim_id, yend = claim_id),
                 colour = "grey40", linewidth = 1.6) +
    geom_point(data = d, aes(log10_bf10, claim_id, colour = prior_label), size = 2.4) +
    scale_colour_manual(values = c(narrow = blue_light, primary = blue_deep, wide = red_main)) +
    labs(x = expression(log[10](BF[10])), y = NULL, colour = "Prior",
         title = "Prior Sensitivity (Narrow / Primary / Wide)") +
    coord_cartesian(xlim = c(-1.2, 8)) +
    theme_reanalysis()
  save_fig(p, out_path, w = 8, h = max(4, 0.35 * n_distinct(d$claim_id) + 1.5))
}
#inspired by the wetzels article
plot_evidence_grid <- function(
    in_path = "outputs/tables/concordance_claim_level.csv",
    out_path = "outputs/figures/evidence_grid_p.png") {
  library(dplyr)
  cl <- readr::read_csv(in_path, show_col_types = FALSE)
  p <- ggplot(cl, aes(p_value, bf10, colour = concordance_status)) +
    geom_hline(yintercept = jeffreys_bf, colour = "grey90", linewidth = 0.3) +
    geom_vline(xintercept = c(.001, .01, .05), colour = "grey90", linewidth = 0.3) +
    geom_point(size = 2.6) +
    scale_x_log10(breaks = c(1e-5, 1e-3, 1e-2, .05, 1),
                  limits = c(1e-6, 1), oob = scales::squish) +
    scale_y_log10(breaks = c(1/10, 1/3, 1, 3, 10, 30, 100),
                  labels = c("1/10","1/3","1","3","10","30","100"),
                  limits = c(0.1, 1e5), oob = scales::squish) + scale_colour_manual(values = concordance_colours) +
    labs(x = "p-value", y = "Bayes Factor", colour = NULL,
         title = "Evidence Grid: Bayes Factor vs P",
         subtitle = "Jeffreys categories; after Wetzels et al. (2011)") +
    theme_reanalysis()
  save_fig(p, out_path, w = 8, h = 5.5)
}
add_margin_pct <- function(p, cl) {
  bands <- cut(cl$p_value, c(-1,.001,.01,.05,1),
               labels = c("<.001",".001-.01",".01-.05",">.05"))
  tab <- round(100 * prop.table(table(bands)))
  p + annotate("text", x = c(3e-4,3e-3,.022,.35), y = 1.4e5,
               label = paste0(tab, "%"), size = 3, colour = "grey35")
  
}
build_all_figures <- function() {
  plot_evidence_plan()
  plot_concordace_squares()
  plot_prior_sensitivity()
  plot_evidence_grid()
}