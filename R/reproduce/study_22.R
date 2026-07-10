# Study 22 transparent reconstruction
# Udvardi-Lakos et al. (2023), DOI: 10.1177/14757257231163482
# Target row: id 16
# Reconstructs Table 2 EB declarative knowledge repeated-measures ANOVA.
# The article/SPSS syntax uses the author aggregate variables Vor_EU_All and PT_EU_All.

reproduce_study_22 <- function(
    input_path = "data/raw/study_22/Data_CombinedApproach_220523.sav",
    output_path = "outputs/reproduced/study_22_recomputed.csv"
) {
  required_packages <- c("haven", "dplyr", "tibble", "readr")
  missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing_packages) > 0) {
    stop(
      "Install required package(s) before running this script: ",
      paste(missing_packages, collapse = ", "),
      call. = FALSE
    )
  }

  if (!file.exists(input_path)) {
    stop("Raw data file not found: ", input_path, call. = FALSE)
  }

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

  raw_data <- haven::read_sav(input_path, user_na = FALSE)

  required_columns <- c("Vor_EU_All", "PT_EU_All")
  missing_columns <- setdiff(required_columns, names(raw_data))

  if (length(missing_columns) > 0) {
    stop(
      "The required author aggregate columns are missing: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  pre_values <- as.numeric(haven::zap_labels(haven::zap_missing(raw_data$Vor_EU_All)))
  post_values <- as.numeric(haven::zap_labels(haven::zap_missing(raw_data$PT_EU_All)))

  analysis_data <- tibble::tibble(
    pre_eb_knowledge = pre_values,
    post_eb_knowledge = post_values
  ) |>
    dplyr::filter(!is.na(pre_eb_knowledge), !is.na(post_eb_knowledge))

  n_complete <- nrow(analysis_data)

  if (n_complete != 24) {
    stop(
      paste0(
        "Study_22 raw-data check failed. The article result requires 24 complete cases on ",
        "Vor_EU_All and PT_EU_All, but this local file gives ", n_complete, ". ",
        "This usually means the wrong local .sav file is being used, or the aggregate variables ",
        "were overwritten/recomputed. Recopy the original OSF/article file to data/raw/study_22/ ",
        "and rerun. This script intentionally stops rather than writing a wrong recomputation."
      ),
      call. = FALSE
    )
  }

  paired_test <- stats::t.test(
    x = analysis_data$post_eb_knowledge,
    y = analysis_data$pre_eb_knowledge,
    paired = TRUE,
    alternative = "two.sided"
  )

  t_value <- as.numeric(paired_test$statistic)
  t_df <- as.numeric(paired_test$parameter)
  f_value <- t_value^2
  f_df1 <- 1
  f_df2 <- t_df
  p_value <- as.numeric(paired_test$p.value)
  eta_p2 <- (f_value * f_df1) / ((f_value * f_df1) + f_df2)

  difference_scores <- analysis_data$post_eb_knowledge - analysis_data$pre_eb_knowledge
  mean_pre <- mean(analysis_data$pre_eb_knowledge)
  sd_pre <- stats::sd(analysis_data$pre_eb_knowledge)
  mean_post <- mean(analysis_data$post_eb_knowledge)
  sd_post <- stats::sd(analysis_data$post_eb_knowledge)
  mean_difference <- mean(difference_scores)
  se_difference <- stats::sd(difference_scores) / sqrt(n_complete)

  article_match <- isTRUE(
    abs(f_value - 50.76) < 0.02 &&
      abs(f_df2 - 23) < 0.001 &&
      abs(eta_p2 - 0.688) < 0.002
  )

  if (!article_match) {
    stop(
      paste0(
        "Study_22 recomputation did not match the article after the n=24 check. Got F(1, ",
        round(f_df2, 6), ") = ", round(f_value, 6), ", eta_p2 = ", round(eta_p2, 6),
        ". The script stopped to avoid saving an incorrect output."
      ),
      call. = FALSE
    )
  }

  results <- tibble::tibble(
    id = 16,
    study_id = "study_22",
    study_DOI = "10.1177/14757257231163482",
    recomputation_status = "recomputed_from_original_author_aggregate_variables",

    stat_test = "repeated_measures_anova",
    reported_result = "F(1, 23) = 50.76, p < .001, eta_p2 = .688",

    p_value = p_value,
    p_operator = "<",
    p_sidedness = "omnibus",

    t_value = t_value,
    t_df = t_df,
    f_value = f_value,
    f_df1 = f_df1,
    f_df2 = f_df2,
    z_value = NA_real_,
    chi2_value = NA_real_,
    chi2_df = NA_real_,
    r_value = NA_real_,

    n1 = NA_real_,
    n2 = NA_real_,
    n_total = n_complete,
    n_eff = n_complete,

    effect_size_type = "eta_p2",
    effect_size_value = eta_p2,

    estimate = mean_difference,
    se_estimate = se_difference,

    raw_data_file = input_path,
    raw_variable_names = "Vor_EU_All; PT_EU_All",
    model_formula = "SPSS GLM Vor_EU_All PT_EU_All /WSFACTOR = Zeitpunkt 2 Polynomial /WSDESIGN = Zeitpunkt",
    contrast_direction = "post-training epistemic-beliefs declarative knowledge minus pre-training epistemic-beliefs declarative knowledge",
    extraction_note = paste0(
      "The original SPSS syntax runs GLM on the saved author aggregate variables Vor_EU_All and PT_EU_All. ",
      "Listwise deletion on these variables gives n = ", n_complete,
      ", pre M = ", round(mean_pre, 6), " (SD = ", round(sd_pre, 6), "), post M = ", round(mean_post, 6),
      " (SD = ", round(sd_post, 6), "), F(1, ", round(f_df2, 6), ") = ", round(f_value, 6),
      ", p = ", signif(p_value, 6), ", eta_p2 = ", round(eta_p2, 6), "."
    )
  )

  readr::write_csv(results, output_path)
  return(results)
}
