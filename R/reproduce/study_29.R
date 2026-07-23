# R/reproduce/study_29.R
# Study 29 transparent reconstruction
# Morling & Lee (2020), DOI: 10.1177/0098628319888087
#
# Faithful R translation of the final SPSS reproduction syntax.
#
# Primary target:
#   UNIANOVA univtenure BY Title
#
# Secondary/extra target:
#   UNIANOVA univkids BY Title
#
# Important:
# 1. This script reproduces the supplied reproduction code as written. It does
#    not add Department or reconstruct a different 2 x 2 factorial model.
# 2. In the SPSS syntax, SELECT IF on nonmissing univtenure permanently reduced
#    the active working dataset before the univkids analysis. The secondary
#    analysis below intentionally preserves that inherited filter.
# 3. With a single two-level factor, the Title test from lm()/anova() is
#    equivalent to the SPSS one-factor UNIANOVA Type-III test.

reproduce_study_29 <- function(
    primary_id,
    secondary_id = NULL,
    input_path = NULL,
    output_path = "outputs/reproduced/study_29_recomputed.csv"
) {
  required_packages <- c("readr", "dplyr", "tibble")
  missing_packages <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]

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

  if (missing(primary_id) || length(primary_id) != 1 || is.na(primary_id)) {
    stop(
      "Provide the registry id for the primary univtenure target as primary_id.",
      call. = FALSE
    )
  }

  if (is.null(input_path)) {
    candidate_paths <- c(
      "data/raw/study_29/Morling and Lee Faculty Sample Open Data.csv",
      "data/raw/study_29/Morling and Lee Faculty Sample Open Data(1).csv",
      "data/raw/study_29/Morling and Lee Faculty Sample Open Data(2).csv"
    )

    existing_paths <- candidate_paths[file.exists(candidate_paths)]

    if (length(existing_paths) == 0) {
      stop(
        "Raw data file not found. Tried:\n  ",
        paste(candidate_paths, collapse = "\n  "),
        call. = FALSE
      )
    }

    input_path <- existing_paths[[1]]
  }

  if (!file.exists(input_path)) {
    stop("Raw data file not found: ", input_path, call. = FALSE)
  }

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

  raw_data <- readr::read_csv(
    input_path,
    show_col_types = FALSE
  )

  required_columns <- c("Title", "univtenure", "univkids")
  missing_columns <- setdiff(required_columns, names(raw_data))

  if (length(missing_columns) > 0) {
    stop(
      "The following required columns are missing from the raw data: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  as_numeric_clean <- function(x) {
    if (is.factor(x)) {
      x <- as.character(x)
    }
    suppressWarnings(as.numeric(x))
  }

  cleaned_data <- tibble::tibble(
    title_numeric = as_numeric_clean(raw_data$Title),
    univtenure = as_numeric_clean(raw_data$univtenure),
    univkids = as_numeric_clean(raw_data$univkids)
  )

  invalid_title_values <- sort(unique(
    cleaned_data$title_numeric[
      !is.na(cleaned_data$title_numeric) &
        !cleaned_data$title_numeric %in% c(1, 2)
    ]
  ))

  if (length(invalid_title_values) > 0) {
    stop(
      "Unexpected nonmissing Title value(s): ",
      paste(invalid_title_values, collapse = ", "),
      ". Expected 1 = Teaching Associate and 2 = Associate Professor.",
      call. = FALSE
    )
  }

  fit_one_way_title_anova <- function(data, outcome, label) {
    analysis_data <- tibble::tibble(
      outcome = data[[outcome]],
      title_numeric = data$title_numeric
    ) |>
      dplyr::filter(
        title_numeric %in% c(1, 2),
        !is.na(outcome)
      ) |>
      dplyr::mutate(
        title = factor(
          title_numeric,
          levels = c(1, 2),
          labels = c(
            "Teaching Associate Professor",
            "Associate Professor"
          )
        )
      )

    if (nrow(analysis_data) == 0) {
      stop("No observations remain for ", label, ".", call. = FALSE)
    }

    observed_levels <- levels(droplevels(analysis_data$title))

    if (length(observed_levels) != 2) {
      stop(
        "Expected two Title groups for ", label,
        ", but observed: ",
        paste(observed_levels, collapse = ", "),
        call. = FALSE
      )
    }

    # This is the direct R equivalent of:
    # UNIANOVA outcome BY Title /METHOD=SSTYPE(3) /DESIGN=Title.
    #
    # Because the model has only one factor, the Title sum of squares is the
    # same under Type I and Type III definitions.
    model <- stats::lm(
      outcome ~ title,
      data = analysis_data
    )

    anova_table <- as.data.frame(stats::anova(model))
    anova_table$term <- trimws(rownames(anova_table))

    title_row <- anova_table |>
      dplyr::filter(term == "title")

    residual_row <- anova_table |>
      dplyr::filter(term == "Residuals")

    if (nrow(title_row) != 1 || nrow(residual_row) != 1) {
      stop(
        "Could not extract the Title and residual ANOVA rows for ",
        label,
        ".",
        call. = FALSE
      )
    }

    f_value <- as.numeric(title_row[["F value"]])
    f_df1 <- as.numeric(title_row[["Df"]])
    f_df2 <- as.numeric(residual_row[["Df"]])
    p_value <- as.numeric(title_row[["Pr(>F)"]])
    residual_mse <- as.numeric(residual_row[["Mean Sq"]])

    group_summary <- analysis_data |>
      dplyr::group_by(title) |>
      dplyr::summarise(
        n = dplyr::n(),
        mean = mean(outcome),
        sd = stats::sd(outcome),
        .groups = "drop"
      )

    teaching_row <- group_summary |>
      dplyr::filter(title == "Teaching Associate Professor")

    associate_row <- group_summary |>
      dplyr::filter(title == "Associate Professor")

    if (nrow(teaching_row) != 1 || nrow(associate_row) != 1) {
      stop(
        "Could not recover both Title-group summaries for ",
        label,
        ".",
        call. = FALSE
      )
    }

    n_teaching <- as.numeric(teaching_row$n)
    n_associate <- as.numeric(associate_row$n)
    mean_teaching <- as.numeric(teaching_row$mean)
    mean_associate <- as.numeric(associate_row$mean)
    sd_teaching <- as.numeric(teaching_row$sd)
    sd_associate <- as.numeric(associate_row$sd)

    pooled_sd <- sqrt(residual_mse)

    associate_minus_teaching <- mean_associate - mean_teaching
    se_difference <- pooled_sd * sqrt(
      1 / n_teaching + 1 / n_associate
    )

    d_associate_minus_teaching <-
      associate_minus_teaching / pooled_sd

    eta_p2 <- (
      f_value * f_df1
    ) / (
      f_value * f_df1 + f_df2
    )

    group_summary_text <- paste0(
      "Teaching Associate Professor n=", n_teaching,
      ", M=", round(mean_teaching, 6),
      ", SD=", round(sd_teaching, 6),
      "; Associate Professor n=", n_associate,
      ", M=", round(mean_associate, 6),
      ", SD=", round(sd_associate, 6)
    )

    list(
      model = model,
      analysis_data = analysis_data,
      f_value = f_value,
      f_df1 = f_df1,
      f_df2 = f_df2,
      p_value = p_value,
      residual_mse = residual_mse,
      eta_p2 = eta_p2,
      n_teaching = n_teaching,
      n_associate = n_associate,
      mean_teaching = mean_teaching,
      mean_associate = mean_associate,
      pooled_sd = pooled_sd,
      associate_minus_teaching = associate_minus_teaching,
      se_difference = se_difference,
      d_associate_minus_teaching = d_associate_minus_teaching,
      group_summary_text = group_summary_text
    )
  }

  # -------------------------------------------------------------------------
  # Primary target
  #
  # SPSS:
  # USE ALL.
  # SELECT IF (Title = 1 OR Title = 2) AND NOT MISSING(univtenure).
  # UNIANOVA univtenure BY Title /METHOD=SSTYPE(3) /DESIGN=Title.
  # -------------------------------------------------------------------------

  primary_working_data <- cleaned_data |>
    dplyr::filter(
      title_numeric %in% c(1, 2),
      !is.na(univtenure)
    )

  primary_fit <- fit_one_way_title_anova(
    data = primary_working_data,
    outcome = "univtenure",
    label = "perceived tenure likelihood"
  )

  primary_row <- tibble::tibble(
    id = as.numeric(primary_id),
    study_id = "study_29",
    study_DOI = "10.1177/0098628319888087",
    recomputation_status =
      "recomputed_from_raw_data_translated_spss_syntax_near_match",

    stat_test = "one_way_between_anova",
    reported_result = paste0(
      "F(1, 432) = 232.57, p < .001, d = 1.50, ",
      "95% CI [1.25, 1.67]"
    ),
    reported_p_value = 0.001,
    reported_p_operator = "<",
    reported_p_sidedness = "omnibus",
    reported_effect_size_type = "cohens_d",
    reported_effect_size_value = 1.50,

    p_value = primary_fit$p_value,
    p_operator = "=",
    p_sidedness = "omnibus",

    t_value = NA_real_,
    t_df = NA_real_,
    f_value = primary_fit$f_value,
    f_df1 = primary_fit$f_df1,
    f_df2 = primary_fit$f_df2,
    z_value = NA_real_,
    chi2_value = NA_real_,
    chi2_df = NA_real_,
    r_value = NA_real_,

    n1 = primary_fit$n_teaching,
    n2 = primary_fit$n_associate,
    n_total =
      primary_fit$n_teaching + primary_fit$n_associate,
    n_eff = NA_real_,

    effect_size_type = "cohens_d_pooled_sd",
    effect_size_value =
      primary_fit$d_associate_minus_teaching,

    estimate =
      primary_fit$associate_minus_teaching,
    se_estimate =
      primary_fit$se_difference,

    raw_data_file = basename(input_path),
    raw_variable_names = "Title; univtenure",
    model_formula =
      "univtenure ~ Title; one-way between-subjects ANOVA",

    contrast_direction = paste0(
      "Associate Professor minus Teaching Associate Professor"
    ),

    analysis_label =
      "title_difference_in_perceived_tenure_likelihood",

    statistic_source = paste0(
      "Direct R translation of SPSS UNIANOVA univtenure BY Title; ",
      "stats::lm followed by stats::anova"
    ),

    bayesian_input_status =
      "ready_jzs_ttest_equivalent_two_group_anova",

    extraction_note = paste0(
      "Faithful translation of the supplied final SPSS reproduction code. ",
      "No Department term was added. Complete-case N = ",
      primary_fit$n_teaching + primary_fit$n_associate,
      ". ", primary_fit$group_summary_text,
      ". Exact translated result: F(",
      primary_fit$f_df1, ", ", primary_fit$f_df2,
      ") = ", round(primary_fit$f_value, 6),
      ", p = ", format(primary_fit$p_value, scientific = TRUE),
      ", pooled-SD d = ",
      round(primary_fit$d_associate_minus_teaching, 6),
      ", partial eta squared = ",
      round(primary_fit$eta_p2, 6),
      ". The translated reproduction gives denominator df = ",
      primary_fit$f_df2,
      ", rather than the reported 432; this discrepancy is retained ",
      "rather than repaired by introducing an unreported model term."
    ),

    eta_p2 = primary_fit$eta_p2
  )

  results <- primary_row

  # -------------------------------------------------------------------------
  # Secondary/extra target
  #
  # The supplied SPSS syntax ran univkids after SELECT IF on univtenure.
  # SPSS SELECT IF permanently changed the active working dataset. Therefore
  # this reconstruction deliberately starts from primary_working_data and then
  # applies outcome-specific missing-data removal for univkids.
  # -------------------------------------------------------------------------

  if (!is.null(secondary_id)) {
    if (length(secondary_id) != 1 || is.na(secondary_id)) {
      stop(
        "secondary_id must be NULL or one nonmissing registry id.",
        call. = FALSE
      )
    }

    secondary_fit <- fit_one_way_title_anova(
      data = primary_working_data,
      outcome = "univkids",
      label = "perceived likelihood of having children"
    )

    # The reported claim is Teaching Associate Professor > Associate
    # Professor, so reverse the generic Associate-minus-Teaching contrast.
    teaching_minus_associate <-
      -secondary_fit$associate_minus_teaching

    d_teaching_minus_associate <-
      -secondary_fit$d_associate_minus_teaching

    secondary_row <- tibble::tibble(
      id = as.numeric(secondary_id),
      study_id = "study_29",
      study_DOI = "10.1177/0098628319888087",
      recomputation_status =
        "recomputed_from_raw_data_translated_spss_syntax_near_match",

      stat_test = "one_way_between_anova",
      reported_result =
        "F(1, 429) = 14.19, p < .001, d = 0.36",
      reported_p_value = 0.001,
      reported_p_operator = "<",
      reported_p_sidedness = "omnibus",
      reported_effect_size_type = "cohens_d",
      reported_effect_size_value = 0.36,

      p_value = secondary_fit$p_value,
      p_operator = "=",
      p_sidedness = "omnibus",

      t_value = NA_real_,
      t_df = NA_real_,
      f_value = secondary_fit$f_value,
      f_df1 = secondary_fit$f_df1,
      f_df2 = secondary_fit$f_df2,
      z_value = NA_real_,
      chi2_value = NA_real_,
      chi2_df = NA_real_,
      r_value = NA_real_,

      n1 = secondary_fit$n_teaching,
      n2 = secondary_fit$n_associate,
      n_total =
        secondary_fit$n_teaching + secondary_fit$n_associate,
      n_eff = NA_real_,

      effect_size_type = "cohens_d_pooled_sd",
      effect_size_value = d_teaching_minus_associate,

      estimate = teaching_minus_associate,
      se_estimate = secondary_fit$se_difference,

      raw_data_file = basename(input_path),
      raw_variable_names = "Title; univtenure; univkids",
      model_formula =
        "univkids ~ Title; one-way between-subjects ANOVA after inherited univtenure complete-case filter",

      contrast_direction = paste0(
        "Teaching Associate Professor minus Associate Professor"
      ),

      analysis_label =
        "title_difference_in_perceived_likelihood_of_having_children",

      statistic_source = paste0(
        "Direct R translation of SPSS UNIANOVA univkids BY Title, ",
        "preserving the previously applied univtenure SELECT IF filter; ",
        "stats::lm followed by stats::anova"
      ),

      bayesian_input_status =
        "ready_jzs_ttest_equivalent_two_group_anova",

      extraction_note = paste0(
        "Faithful translation of the supplied SPSS command sequence. ",
        "Because SPSS SELECT IF had already restricted the working data to ",
        "nonmissing univtenure cases, the univkids analysis also requires ",
        "nonmissing univtenure. Analysis N = ",
        secondary_fit$n_teaching + secondary_fit$n_associate,
        ". ", secondary_fit$group_summary_text,
        ". Exact translated result: F(",
        secondary_fit$f_df1, ", ", secondary_fit$f_df2,
        ") = ", round(secondary_fit$f_value, 6),
        ", p = ", format(secondary_fit$p_value, scientific = TRUE),
        ", pooled-SD d in the reported direction = ",
        round(d_teaching_minus_associate, 6),
        ", partial eta squared = ",
        round(secondary_fit$eta_p2, 6),
        ". The translated reproduction does not exactly match the reported ",
        "F(1, 429) = 14.19; the inherited SPSS filter and resulting ",
        "denominator df are recorded explicitly."
      ),

      eta_p2 = secondary_fit$eta_p2
    )

    results <- dplyr::bind_rows(
      primary_row,
      secondary_row
    )
  }

  readr::write_csv(results, output_path)

  message(
    "Wrote Study 29 recomputed results to: ",
    output_path
  )

  return(results)
}
