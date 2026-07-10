# Study 26 transparent reconstruction
# Kelly, Laurin, & Clinton-Lisell (2022), DOI: 10.1177/00986283221108129
# Target rows: id 17 and id 18
# Reconstructs SPSS legacy NPAR TESTS Wilcoxon signed-rank results.

reproduce_study_26 <- function(
    input_path = "data/raw/study_26/Final_Data_Set_Spring_2022.sav",
    output_path = "outputs/reproduced/study_26_recomputed.csv"
) {
  required_packages <- c("haven", "tibble", "readr")
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

  required_columns <- c("womenrec1", "womenrec2", "menrec1")
  missing_columns <- setdiff(required_columns, names(raw_data))

  if (length(missing_columns) > 0) {
    stop(
      "Study_26 is missing required author composite column(s): ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  women_initial <- as.numeric(haven::zap_labels(haven::zap_missing(raw_data$womenrec1)))
  women_followup <- as.numeric(haven::zap_labels(haven::zap_missing(raw_data$womenrec2)))
  men_initial <- as.numeric(haven::zap_labels(haven::zap_missing(raw_data$menrec1)))

  compute_spss_wilcoxon <- function(first_variable, second_variable) {
    paired_data <- stats::na.omit(
      data.frame(
        first_variable = first_variable,
        second_variable = second_variable
      )
    )

    differences <- paired_data$first_variable - paired_data$second_variable
    nonzero_differences <- differences[differences != 0]
    n_nonzero <- length(nonzero_differences)

    if (n_nonzero == 0) {
      stop("Wilcoxon reconstruction failed because all paired differences are zero.", call. = FALSE)
    }

    absolute_differences <- abs(nonzero_differences)
    ranks <- rank(absolute_differences, ties.method = "average")

    positive_rank_sum <- sum(ranks[nonzero_differences > 0])
    negative_rank_sum <- sum(ranks[nonzero_differences < 0])

    expected_rank_sum <- n_nonzero * (n_nonzero + 1) / 4

    tie_table <- table(absolute_differences)
    tie_correction <- sum((as.numeric(tie_table)^3) - as.numeric(tie_table)) / 48
    rank_sum_variance <- n_nonzero * (n_nonzero + 1) * (2 * n_nonzero + 1) / 24 - tie_correction

    z_value <- (positive_rank_sum - expected_rank_sum) / sqrt(rank_sum_variance)
    p_value <- 2 * stats::pnorm(abs(z_value), lower.tail = FALSE)
    effect_size_r <- z_value / sqrt(n_nonzero)

    list(
      n_complete_pairs = nrow(paired_data),
      n_nonzero_pairs = n_nonzero,
      n_zero_pairs = sum(differences == 0),
      positive_rank_sum = positive_rank_sum,
      negative_rank_sum = negative_rank_sum,
      z_value = z_value,
      p_value = p_value,
      effect_size_r = effect_size_r,
      first_mean = mean(paired_data$first_variable),
      first_sd = stats::sd(paired_data$first_variable),
      first_median = stats::median(paired_data$first_variable),
      second_mean = mean(paired_data$second_variable),
      second_sd = stats::sd(paired_data$second_variable),
      second_median = stats::median(paired_data$second_variable)
    )
  }

  # id 17: article change score test for women's pioneer recognition.
  # SPSS syntax: NPAR TESTS /WILCOXON=womenrec1 WITH womenrec2 (PAIRED).
  women_change <- compute_spss_wilcoxon(
    first_variable = women_initial,
    second_variable = women_followup
  )

  # id 18: article initial-recognition comparison between women and men.
  # SPSS syntax: NPAR TESTS /WILCOXON=womenrec1 WITH menrec1 (PAIRED).
  initial_women_men <- compute_spss_wilcoxon(
    first_variable = women_initial,
    second_variable = men_initial
  )

  women_change_matches_article <- isTRUE(
    women_change$n_nonzero_pairs == 50 &&
      abs(women_change$z_value - (-6.155)) < 0.002 &&
      abs(women_change$effect_size_r - (-0.87)) < 0.005
  )

  initial_women_men_matches_article <- isTRUE(
    initial_women_men$n_nonzero_pairs == 50 &&
      abs(initial_women_men$z_value - (-6.126)) < 0.002 &&
      abs(initial_women_men$effect_size_r - (-0.87)) < 0.005
  )

  if (!women_change_matches_article || !initial_women_men_matches_article) {
    stop(
      paste0(
        "Study_26 recomputation did not match the target article values. ",
        "Women change gave z = ", round(women_change$z_value, 6),
        ", r = ", round(women_change$effect_size_r, 6),
        ", n = ", women_change$n_nonzero_pairs,
        "; initial women-vs-men gave z = ", round(initial_women_men$z_value, 6),
        ", r = ", round(initial_women_men$effect_size_r, 6),
        ", n = ", initial_women_men$n_nonzero_pairs,
        ". The script stopped to avoid saving an incorrect output."
      ),
      call. = FALSE
    )
  }

  results <- tibble::tibble(
    id = c(17, 18),
    study_id = c("study_26", "study_26"),
    study_DOI = c("10.1177/00986283221108129", "10.1177/00986283221108129"),
    recomputation_status = c(
      "recomputed_from_author_composite_variables",
      "recomputed_from_author_composite_variables"
    ),

    stat_test = c("wilcoxon_signed_rank", "wilcoxon_signed_rank"),
    reported_result = c(
      "women: z = -6.155, p < .001, r = -.87",
      "initial women versus men: z = -6.126, p < .001, r = -.87"
    ),

    p_value = c(women_change$p_value, initial_women_men$p_value),
    p_operator = c("<", "<"),
    p_sidedness = c("two_sided", "two_sided"),

    t_value = c(NA_real_, NA_real_),
    t_df = c(NA_real_, NA_real_),
    f_value = c(NA_real_, NA_real_),
    f_df1 = c(NA_real_, NA_real_),
    f_df2 = c(NA_real_, NA_real_),
    z_value = c(women_change$z_value, initial_women_men$z_value),
    chi2_value = c(NA_real_, NA_real_),
    chi2_df = c(NA_real_, NA_real_),
    r_value = c(NA_real_, NA_real_),

    n1 = c(NA_real_, NA_real_),
    n2 = c(NA_real_, NA_real_),
    n_total = c(women_change$n_nonzero_pairs, initial_women_men$n_nonzero_pairs),
    n_eff = c(women_change$n_nonzero_pairs, initial_women_men$n_nonzero_pairs),

    effect_size_type = c("wilcoxon_r", "wilcoxon_r"),
    effect_size_value = c(women_change$effect_size_r, initial_women_men$effect_size_r),

    estimate = c(
      women_change$second_mean - women_change$first_mean,
      initial_women_men$first_mean - initial_women_men$second_mean
    ),
    se_estimate = c(NA_real_, NA_real_),

    raw_data_file = c(input_path, input_path),
    raw_variable_names = c("womenrec1; womenrec2", "womenrec1; menrec1"),
    model_formula = c(
      "SPSS NPAR TESTS /WILCOXON=womenrec1 WITH womenrec2 (PAIRED)",
      "SPSS NPAR TESTS /WILCOXON=womenrec1 WITH menrec1 (PAIRED)"
    ),
    contrast_direction = c(
      "follow-up recognition for women pioneers versus initial recognition for women pioneers",
      "initial recognition for women pioneers versus initial recognition for men pioneers"
    ),
    extraction_note = c(
      paste0(
        "The uploaded SPSS syntax identifies this as the main/chosen result. It uses the author composite variables womenrec1 and womenrec2. ",
        "The script reconstructs the SPSS legacy Wilcoxon signed-rank normal approximation with tie correction and no continuity correction: ",
        "n = ", women_change$n_nonzero_pairs,
        ", W+ = ", round(women_change$positive_rank_sum, 6),
        ", W- = ", round(women_change$negative_rank_sum, 6),
        ", initial M = ", round(women_change$first_mean, 6), " (SD = ", round(women_change$first_sd, 6), ", Mdn = ", round(women_change$first_median, 6), "), ",
        "follow-up M = ", round(women_change$second_mean, 6), " (SD = ", round(women_change$second_sd, 6), ", Mdn = ", round(women_change$second_median, 6), "), ",
        "z = ", round(women_change$z_value, 6), ", p = ", signif(women_change$p_value, 6), ", r = ", round(women_change$effect_size_r, 6), "."
      ),
      paste0(
        "The uploaded SPSS syntax identifies this as an extra result. It uses the author composite variables womenrec1 and menrec1. ",
        "The script reconstructs the SPSS legacy Wilcoxon signed-rank normal approximation with tie correction and no continuity correction: ",
        "n = ", initial_women_men$n_nonzero_pairs,
        ", W+ = ", round(initial_women_men$positive_rank_sum, 6),
        ", W- = ", round(initial_women_men$negative_rank_sum, 6),
        ", women M = ", round(initial_women_men$first_mean, 6), " (SD = ", round(initial_women_men$first_sd, 6), ", Mdn = ", round(initial_women_men$first_median, 6), "), ",
        "men M = ", round(initial_women_men$second_mean, 6), " (SD = ", round(initial_women_men$second_sd, 6), ", Mdn = ", round(initial_women_men$second_median, 6), "), ",
        "z = ", round(initial_women_men$z_value, 6), ", p = ", signif(initial_women_men$p_value, 6), ", r = ", round(initial_women_men$effect_size_r, 6), "."
      )
    )
  )

  readr::write_csv(results, output_path)
  return(results)
}
