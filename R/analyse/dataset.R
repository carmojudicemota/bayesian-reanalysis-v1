library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

source("R/visualise/plot_theme.R")

family_label_lookup <- c(
  one_sample_t_test = "One-sample t-test",
  paired_t_test = "Paired t-test",
  independent_t_test = "Independent t-test",
  welch_independent_t_test = "Welch t-test",
  repeated_measures_anova = "One-df repeated-measures ANOVA",
  factorial_between_anova = "Factorial between-subjects ANOVA",
  mixed_anova = "Mixed ANOVA",
  welch_anova = "Welch ANOVA",
  manova = "MANOVA",
  pearson_correlation = "Pearson correlation",
  spearman_correlation = "Spearman correlation",
  wilcoxon_signed_rank = "Wilcoxon signed-rank",
  friedman_test = "Friedman test",
  mixed_effects_logistic_regression = "Mixed-effects logistic regression",
  sem_contrast = "SEM contrast",
  chi_square_difference = "Chi-square difference",
  unclassified = "Unclassified / held"
)

label_family <- function(x) {
  key <- ifelse(is.na(x) | trimws(x) == "", "unclassified", x)
  labels <- unname(family_label_lookup[key])
  labels[is.na(labels)] <- key[is.na(labels)]
  labels
}

claims_dictionary <- tibble::tribble(
  ~column_name, ~description,
  "claim_id", "Stable project identifier for the claim-level target.",
  "study_id", "Stable project identifier for the source study.",
  "doi", "Digital Object Identifier of the source article.",
  "claim", "Text of the scientific claim Ali selected for reanalysis.",
  "role", "Whether the claim is primary or supplementary within the project registry (As defined by Ali).",
  "direction", "Prospectively recorded direction of a directional hypothesis.",
  "status", "Current status: ready, pending, or held.",
  "note", "Methodological or reconstruction note.",
  "result_position", "Position or importance of the result within the source study.",
  "source_result_id", "Identifier linking the claim to the source result record.",
  "article_result_from_phase1", "Result as extracted from Ali's Phase 1.",
  "ali_test_name", "Name of the statistical procedure used in Ali's reproduction file.",
  "ali_reported_result", "Published result recorded in Ali's source files.",
  "ali_reproduced_result", "Result obtained in Ali's computational reproduction.",
  "ali_result_status", "Reproducibility status assigned in Ali's project.",
  "in_scope", "Whether the result satisfies this project's scope rules.",
  "scope_exclusion_reason", "Reason a claim is excluded from scope.",
  "project_verified_result", "Result recomputed and verified in the present project.",
  "project_result_status", "Method and status of the present project's verification.",
  "frequentist_test", "Canonical frequentist model or test family assigned by us to the result.",
  "p_value", "Exact or best available p-value.",
  "p_operator", "Reported comparison operator attached to the p-value, such as = or <.",
  "p_sidedness", "Whether the published test was one-sided, two-sided, or omnibus.",
  "t_value", "Reconstructed t-statistic, when applicable.",
  "t_df", "Degrees of freedom associated with the t-statistic.",
  "f_value", "Reconstructed F-statistic, when applicable.",
  "f_df1", "Numerator degrees of freedom of the F-statistic.",
  "f_df2", "Denominator degrees of freedom of the F-statistic.",
  "z_value", "Reconstructed z-statistic, when applicable.",
  "chi2_value", "Reconstructed chi-square statistic, when applicable.",
  "chi2_df", "Degrees of freedom of the chi-square statistic.",
  "r_value", "Reconstructed Pearson or rank correlation coefficient, when applicable.",
  "n1", "Analysis sample size for group 1, where the design has two independent groups.",
  "n2", "Analysis sample size for group 2, where the design has two independent groups.",
  "n_total", "Total analysis sample size for the target result.",
  "n_eff", "Effective sample size used or derived for the target analysis.",
  "effect_size_type", "Definition of the reported or reconstructed effect-size measure.",
  "effect_size_value", "Numerical value of the reported or reconstructed effect size.",
  "estimate", "Model coefficient, contrast estimate, or other estimate.",
  "se_estimate", "Standard error associated with the estimate.",
  "raw_data_file", "Source raw-data file used for recomputation.",
  "raw_variable_names", "Raw variables used to reconstruct the result.",
  "model_formula", "Model formula used in recomputation",
  "contrast_direction", "Definition and orientation of the contrast.",
  "extraction_note", "Detailed notes on extraction, reconstruction, or interpretation.",
  "source_file", "Project file from which the verified result was compiled.",
  "source_row_id", "Row identifier within the project source file.",
  "source_study_id", "Study identifier retained from the upstream source files."
)

build_dataset_outputs <- function(
    claims_path = "data/derived/claims.csv",
    table_dir = "outputs/tables",
    figure_dir = "outputs/figures"
) {
  claims <- read_csv(claims_path, show_col_types = FALSE) |>
    mutate(
      family_key = if_else(
        is.na(frequentist_test) | trimws(frequentist_test) == "",
        "unclassified",
        frequentist_test
      ),
      family_label = label_family(family_key),
      raw_data_available = !is.na(raw_data_file) & trimws(raw_data_file) != "",
      sample_size_available = !is.na(n_total),
      p_value_available = !is.na(p_value),
      model_formula_available = !is.na(model_formula) & trimws(model_formula) != ""
    )
  
  required_statuses <- c("ready", "pending", "held")
  invalid_status <- setdiff(unique(na.omit(claims$status)), required_statuses)
  if (length(invalid_status) > 0) {
    stop(
      "Unexpected claim status values: ",
      paste(invalid_status, collapse = ", "),
      call. = FALSE
    )
  }
  
  if (anyDuplicated(claims$claim_id)) {
    stop("Duplicate claim_id values found in ", claims_path, call. = FALSE)
  }
  
  generated_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  
  overview <- tibble::tribble(
    ~metric, ~value, ~definition,
    "total_claims", nrow(claims), "Number of claim-level targets in the full claims table.",
    "total_studies", n_distinct(claims$study_id), "Number of distinct source studies represented.",
    "in_scope_claims", sum(claims$in_scope %in% TRUE, na.rm = TRUE), "Claims satisfying the project's scope rule.",
    "out_of_scope_claims", sum(claims$in_scope %in% FALSE, na.rm = TRUE), "Claims excluded by the project's scope rule.",
    "ready_claims", sum(claims$status == "ready", na.rm = TRUE), "Claims currently ready for Bayesian analysis.",
    "pending_claims", sum(claims$status == "pending", na.rm = TRUE), "Claims awaiting a family-specific Bayesian model or further specification.",
    "held_claims", sum(claims$status == "held", na.rm = TRUE), "Claims retained in the registry but not entering the current analysis.",
    "primary_claims", sum(claims$role == "primary", na.rm = TRUE), "Claims classified as primary targets.",
    "supplementary_claims", sum(claims$role == "supplementary", na.rm = TRUE), "Claims classified as supplementary targets.",
    "claims_with_raw_data", sum(claims$raw_data_available), "Claims linked to an identified raw-data file.",
    "claims_with_p_value", sum(claims$p_value_available), "Claims with an exact or best available p-value.",
    "claims_with_sample_size", sum(claims$sample_size_available), "Claims with a recorded total analysis sample size."
  ) |>
    mutate(
      source_file = claims_path,
      generated_by = "R/analyse/dataset.R::build_dataset_outputs",
      generated_at_utc = generated_at
    )
  
  family_summary <- claims |>
    group_by(family_key, family_label) |>
    summarise(
      claim_count = n(),
      study_count = n_distinct(study_id),
      primary_claims = sum(role == "primary", na.rm = TRUE),
      supplementary_claims = sum(role == "supplementary", na.rm = TRUE),
      ready_claims = sum(status == "ready", na.rm = TRUE),
      pending_claims = sum(status == "pending", na.rm = TRUE),
      held_claims = sum(status == "held", na.rm = TRUE),
      in_scope_claims = sum(in_scope %in% TRUE, na.rm = TRUE),
      raw_data_available_claims = sum(raw_data_available),
      sample_size_available_claims = sum(sample_size_available),
      minimum_n = if (all(is.na(n_total))) NA_real_ else min(n_total, na.rm = TRUE),
      median_n = if (all(is.na(n_total))) NA_real_ else median(n_total, na.rm = TRUE),
      maximum_n = if (all(is.na(n_total))) NA_real_ else max(n_total, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(desc(claim_count), family_label) |>
    mutate(
      source_file = claims_path,
      generated_by = "R/analyse/dataset.R::build_dataset_outputs",
      generated_at_utc = generated_at
    )
  
  study_summary <- claims |>
    group_by(study_id) |>
    summarise(
      doi = dplyr::first(na.omit(doi), default = NA_character_),
      claim_count = n(),
      primary_claims = sum(role == "primary", na.rm = TRUE),
      supplementary_claims = sum(role == "supplementary", na.rm = TRUE),
      ready_claims = sum(status == "ready", na.rm = TRUE),
      pending_claims = sum(status == "pending", na.rm = TRUE),
      held_claims = sum(status == "held", na.rm = TRUE),
      in_scope_claims = sum(in_scope %in% TRUE, na.rm = TRUE),
      statistical_families = paste(sort(unique(family_label)), collapse = "; "),
      raw_data_available = any(raw_data_available),
      minimum_n = if (all(is.na(n_total))) NA_real_ else min(n_total, na.rm = TRUE),
      maximum_n = if (all(is.na(n_total))) NA_real_ else max(n_total, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(study_id) |>
    mutate(
      source_file = claims_path,
      generated_by = "R/analyse/dataset.R::build_dataset_outputs",
      generated_at_utc = generated_at
    )
  
  column_profile <- tibble(
    column_name = names(claims)[!names(claims) %in% c(
      "family_key", "family_label", "raw_data_available",
      "sample_size_available", "p_value_available", "model_formula_available"
    )]
  ) |>
    mutate(
      storage_type = vapply(
        claims[column_name],
        function(x) paste(class(x), collapse = "/"),
        character(1)
      ),
      non_missing_n = vapply(claims[column_name], function(x) sum(!is.na(x)), integer(1)),
      missing_n = nrow(claims) - non_missing_n,
      completeness_percent = round(100 * non_missing_n / nrow(claims), 1)
    ) |>
    left_join(claims_dictionary, by = "column_name") |>
    mutate(
      description = coalesce(description, "No project description has yet been supplied."),
      source_file = claims_path,
      generated_by = "R/analyse/dataset.R::build_dataset_outputs",
      generated_at_utc = generated_at
    )
  
  manifest <- tibble::tribble(
    ~file_name, ~unit_of_observation, ~description,
    "dataset_overview.csv", "One row per result", "High-level counts describing the complete claims dataset.",
    "dataset_family_summary.csv", "One row per statistical family", "Counts, status composition, data availability, and sample-size summaries by statistical family.",
    "dataset_study_summary.csv", "One row per study", "Claim composition, status, model families, and sample-size range for each source study.",
    "dataset_data_dictionary.csv", "One row per claims-table column", "Column type, completeness, and a human-readable definition for the authoritative claims table."
  ) |>
    mutate(
      source_file = claims_path,
      generated_by = "R/analyse/dataset.R::build_dataset_outputs",
      generated_at_utc = generated_at
    )
  
  dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
  
  write_csv(overview, file.path(table_dir, "dataset_overview.csv"), na = "")
  write_csv(family_summary, file.path(table_dir, "dataset_family_summary.csv"), na = "")
  write_csv(study_summary, file.path(table_dir, "dataset_study_summary.csv"), na = "")
  write_csv(column_profile, file.path(table_dir, "dataset_data_dictionary.csv"), na = "")
  write_csv(manifest, file.path(table_dir, "dataset_manifest.csv"), na = "")
  
  family_order <- family_summary$family_label
  plot_claims <- family_summary |>
    select(family_label, claim_count, study_count) |>
    pivot_longer(
      cols = c(claim_count, study_count),
      names_to = "measure",
      values_to = "count"
    ) |>
    mutate(
      family_label = factor(family_label, levels = rev(family_order)),
      measure = recode(
        measure,
        claim_count = "Claims",
        study_count = "Studies"
      )
    ) |>
    ggplot(aes(count, family_label, fill = measure)) +
    geom_col(position = position_dodge(width = 0.78), width = 0.68) +
    geom_text(
      aes(label = count),
      position = position_dodge(width = 0.78),
      hjust = -0.15,
      size = 3.1
    ) +
    scale_fill_manual(values = c(Claims = blue_deep, Studies = blue_light)) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(
      x = "Count",
      y = NULL,
      fill = NULL,
      title = "Claims and Studies by Statistical Family",
      subtitle = "Claims are analytical targets; several claims can originate from the same study."
    ) +
    theme_reanalysis() +
    theme(panel.grid.major.y = element_blank())
  
  status_data <- claims |>
    count(family_label, status, name = "claim_count") |>
    complete(
      family_label = family_summary$family_label,
      status = required_statuses,
      fill = list(claim_count = 0L)
    ) |>
    mutate(
      family_label = factor(family_label, levels = rev(family_order)),
      status = factor(status, levels = required_statuses, labels = c("Ready", "Pending", "Held"))
    )
  
  plot_status <- ggplot(status_data, aes(claim_count, family_label, fill = status)) +
    geom_col(width = 0.72) +
    scale_fill_manual(values = c(Ready = blue_med, Pending = slate, Held = red_main)) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(
      x = "Claims",
      y = NULL,
      fill = "Analytical status",
      title = "Analytical Status by Statistical Family",
      subtitle = "Ready claims have a complete registered analysis; pending claims await family-specific modelling."
    ) +
    theme_reanalysis() +
    theme(panel.grid.major.y = element_blank())
  
  role_data <- claims |>
    count(family_label, role, name = "claim_count") |>
    complete(
      family_label = family_summary$family_label,
      role = c("primary", "supplementary"),
      fill = list(claim_count = 0L)
    ) |>
    mutate(
      family_label = factor(family_label, levels = rev(family_order)),
      role = factor(role, levels = c("primary", "supplementary"), labels = c("Primary", "Supplementary"))
    )
  
  plot_role <- ggplot(role_data, aes(claim_count, family_label, fill = role)) +
    geom_col(width = 0.72) +
    scale_fill_manual(values = c(Primary = blue_deep, Supplementary = blue_light)) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(
      x = "Claims",
      y = NULL,
      fill = "Claim role",
      title = "Primary and Supplementary Claims by Statistical Family"
    ) +
    theme_reanalysis() +
    theme(panel.grid.major.y = element_blank())
  
  sample_data <- claims |>
    filter(!is.na(n_total), n_total > 0) |>
    mutate(
      family_label = fct_reorder(family_label, n_total, .fun = median, .desc = FALSE)
    )
  
  plot_sample_size <- ggplot(sample_data, aes(n_total, family_label)) +
    geom_jitter(height = 0.12, width = 0, alpha = 0.55, size = 2.2, colour = blue_med) +
    stat_summary(fun = median, geom = "point", shape = 18, size = 3.8, colour = red_deep) +
    scale_x_log10(
      breaks = c(10, 20, 50, 100, 200, 500, 1000),
      labels = scales::label_number(big.mark = ",")
    ) +
    labs(
      x = "Analysis sample size (log scale)",
      y = NULL,
      title = "Analysis Sample Sizes by Statistical Family",
      subtitle = "Each blue point is one claim; red diamonds show family medians."
    ) +
    theme_reanalysis() +
    theme(panel.grid.major.y = element_blank())
  
  save_fig(plot_claims, file.path(figure_dir, "dataset_family_counts.png"), w = 9, h = 7.5)
  save_fig(plot_status, file.path(figure_dir, "dataset_status_by_family.png"), w = 9, h = 7.5)
  save_fig(plot_role, file.path(figure_dir, "dataset_role_by_family.png"), w = 9, h = 7.5)
  save_fig(plot_sample_size, file.path(figure_dir, "dataset_sample_size_by_family.png"), w = 9, h = 7.5)
  
  message(
    "Created dataset outputs for ", nrow(claims), " claims from ",
    n_distinct(claims$study_id), " studies."
  )
  
  invisible(list(
    overview = overview,
    family_summary = family_summary,
    study_summary = study_summary,
    data_dictionary = column_profile,
    manifest = manifest
  ))
}

if (sys.nframe() == 0L) {
  source("R/visualise/plot_theme.R")
  build_dataset_outputs()
}
