# R/reproduce/study_39.R
# Study 39: Biddle & Clinton-Lisell (2023), DOI 10.1037/stl0000385.
# Targeted reconstruction of two Spearman correlations from the student
# perceptions/open pedagogy dataset. The script recovers both the recomputed
# correlation evidence and the analysis-ready fields needed downstream.

reproduce_study_39 <- function(
    input_path = NULL,
    output_path = "outputs/reproduced/study_39_recomputed.csv"
) {
  required_packages <- c("haven", "dplyr", "tibble", "readr")
  missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_packages) > 0) {
    stop(
      "Install required package(s) before running this script: ",
      paste(missing_packages, collapse = ", "),
      "\nUse install.packages(c(",
      paste(sprintf('"%s"', missing_packages), collapse = ", "),
      "))",
      call. = FALSE
    )
  }

  if (is.null(input_path)) {
    candidate_paths <- c(
      "data/raw/study_39/Open_Pedagogy_Student_Perceptions.sav",
      "data/raw/study_39/Untitled3.sav"
    )
    existing <- candidate_paths[file.exists(candidate_paths)]
    if (length(existing) == 0) {
      stop(
        "Could not find Study 39 raw data. Tried:\n  ",
        paste(candidate_paths, collapse = "\n  "),
        call. = FALSE
      )
    }
    input_path <- existing[[1]]
  }

  raw <- haven::read_sav(input_path)

  labels <- vapply(
    raw,
    function(x) {
      lab <- attr(x, "label")
      if (is.null(lab)) "" else as.character(lab)
    },
    character(1)
  )

  find_column <- function(name_pattern = NULL, label_pattern = NULL, label_text = "variable") {
    matches <- character(0)

    if (!is.null(name_pattern)) {
      matches <- c(matches, grep(name_pattern, names(raw), value = TRUE, ignore.case = TRUE))
    }

    if (!is.null(label_pattern)) {
      label_matches <- names(raw)[grepl(label_pattern, labels, ignore.case = TRUE)]
      matches <- c(matches, label_matches)
    }

    matches <- unique(matches)

    if (length(matches) < 1) {
      stop(
        "Could not identify ", label_text,
        ". Available variables include:\n  ",
        paste(names(raw), collapse = ", "),
        call. = FALSE
      )
    }

    matches[[1]]
  }

  motivating_col <- find_column(
    name_pattern = "^HOWMOTIV",
    label_pattern = "motivating.*final product.*openly available",
    label_text = "motivation/open availability item"
  )

  diversity_col <- find_column(
    name_pattern = "^IBELIEV",
    label_pattern = "photographs add diversity",
    label_text = "photographs add diversity item"
  )

  engaging_col <- find_column(
    name_pattern = "^DIDTHISC",
    label_pattern = "course seem more engaging.*project",
    label_text = "course engagement comparison item"
  )

  flickr_col <- find_column(
    name_pattern = "^(V41|V41_A)$",
    label_pattern = "sharing your photos.*world.*Flickr",
    label_text = "liked sharing photos with the world/Flickr item"
  )

  as_numeric_survey <- function(x) {
    x <- haven::zap_labels(x)
    if (is.factor(x)) x <- as.character(x)
    if (inherits(x, "Date") || inherits(x, "POSIXt")) {
      return(rep(NA_real_, length(x)))
    }
    suppressWarnings(as.numeric(x))
  }

  compute_spearman <- function(x, y) {
    x_num <- as_numeric_survey(x)
    y_num <- as_numeric_survey(y)
    keep <- stats::complete.cases(x_num, y_num)

    if (sum(keep) < 4) {
      stop("Too few complete paired observations for Spearman correlation.", call. = FALSE)
    }

    test <- suppressWarnings(stats::cor.test(
      x = x_num[keep],
      y = y_num[keep],
      method = "spearman",
      exact = FALSE,
      alternative = "two.sided"
    ))

    list(
      test = test,
      n_pairwise = sum(keep)
    )
  }

  motivating_diversity <- compute_spearman(
    raw[[motivating_col]],
    raw[[diversity_col]]
  )

  engaging_flickr <- compute_spearman(
    raw[[engaging_col]],
    raw[[flickr_col]]
  )

  row25 <- tibble::tibble(
    id = 25,
    study_id = "study_39",
    study_DOI = "10.1037/stl0000385",
    recomputation_status = "recomputed_from_raw_data",
    stat_test = "spearman_correlation",
    reported_result = "rho = .468, p = .008",
    reported_p_value = 0.008,
    reported_p_operator = "=",
    reported_p_sidedness = "two_sided",
    reported_effect_size_type = "spearman_rho",
    reported_effect_size_value = 0.468,
    p_value = unname(motivating_diversity$test$p.value),
    p_operator = "=",
    p_sidedness = "two_sided",
    t_value = NA_real_,
    t_df = NA_real_,
    f_value = NA_real_,
    f_df1 = NA_real_,
    f_df2 = NA_real_,
    z_value = NA_real_,
    chi2_value = NA_real_,
    chi2_df = NA_real_,
    r_value = unname(motivating_diversity$test$estimate),
    n1 = NA_real_,
    n2 = NA_real_,
    n_total = motivating_diversity$n_pairwise,
    n_eff = motivating_diversity$n_pairwise,
    effect_size_type = "spearman_rho",
    effect_size_value = unname(motivating_diversity$test$estimate),
    estimate = unname(motivating_diversity$test$estimate),
    se_estimate = NA_real_,
    raw_data_file = basename(input_path),
    raw_variable_names = paste(motivating_col, diversity_col, sep = "; "),
    model_formula = "stats::cor.test(motivation_open_availability, photographs_add_diversity, method = 'spearman', exact = FALSE)",
    contrast_direction = "More motivation about the openly available final product is associated with stronger belief that photographs add diversity.",
    analysis_label = "Motivation for open availability correlated with perceived contribution to diversity",
    statistic_source = "stats::cor.test Spearman rho with pairwise complete observations",
    bayesian_input_status = "ready_correlation_diagnostic",
    extraction_note = paste0(
      "Recomputes Table 1 Spearman correlation. Pairwise N = ",
      motivating_diversity$n_pairwise,
      ". R uses the large-sample Spearman p-value because Likert items contain ties."
    )
  )

  row26 <- tibble::tibble(
    id = 26,
    study_id = "study_39",
    study_DOI = "10.1037/stl0000385",
    recomputation_status = "recomputed_from_raw_data_extraction_typo_corrected",
    stat_test = "spearman_correlation",
    reported_result = "rho = .394, p = .031",
    reported_p_value = 0.031,
    reported_p_operator = "=",
    reported_p_sidedness = "two_sided",
    reported_effect_size_type = "spearman_rho",
    reported_effect_size_value = 0.394,
    p_value = unname(engaging_flickr$test$p.value),
    p_operator = "=",
    p_sidedness = "two_sided",
    t_value = NA_real_,
    t_df = NA_real_,
    f_value = NA_real_,
    f_df1 = NA_real_,
    f_df2 = NA_real_,
    z_value = NA_real_,
    chi2_value = NA_real_,
    chi2_df = NA_real_,
    r_value = unname(engaging_flickr$test$estimate),
    n1 = NA_real_,
    n2 = NA_real_,
    n_total = engaging_flickr$n_pairwise,
    n_eff = engaging_flickr$n_pairwise,
    effect_size_type = "spearman_rho",
    effect_size_value = unname(engaging_flickr$test$estimate),
    estimate = unname(engaging_flickr$test$estimate),
    se_estimate = NA_real_,
    raw_data_file = basename(input_path),
    raw_variable_names = paste(engaging_col, flickr_col, sep = "; "),
    model_formula = "stats::cor.test(course_engagement_comparison, liked_sharing_with_world_flickr, method = 'spearman', exact = FALSE)",
    contrast_direction = "Courses perceived as more engaging are associated with greater liking of sharing photographs with the world via Flickr.",
    analysis_label = "Course engagement comparison correlated with liking Flickr/world sharing",
    statistic_source = "stats::cor.test Spearman rho with pairwise complete observations",
    bayesian_input_status = "ready_correlation_diagnostic",
    extraction_note = paste0(
      "Recomputes Table 1 Spearman correlation. Pairwise N = ",
      engaging_flickr$n_pairwise,
      ". The original index row appears to contain a typo ('p = .394'); the article reports rho = .394, p = .031."
    )
  )

  results <- dplyr::bind_rows(row25, row26)

  if (exists("standardise_recomputed_output", mode = "function")) {
    results <- standardise_recomputed_output(results)
  }

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(results, output_path)

  message("Wrote Study 39 recomputed results to: ", output_path)
  invisible(results)
}
