# R/reproduce/study_10.R
# Study 10: Smith et al. DOI: 10.1037/stl0000338
# Reconstructs rows id 6 and id 7 from master.anonymizedOSF.csv.
# Important: the index targets the replicate/semester main effects, not the interaction.

if (!exists("make_recomputed_row")) source("R/reproduce/00_reproduction_helpers.R")

reproduce_study_10 <- function(
    input_path = "data/raw/study_10/master.anonymizedOSF.csv",
    output_path = "outputs/reproduced/study_10_recomputed.csv"
) {
  check_required_packages(c("readr", "dplyr", "tibble"))
  input_path <- resolve_existing_file(c(input_path, "data/raw/study_10/master.anonymizedOSF.csv"), "study_10 CSV")

  dat <- readr::read_csv(input_path, show_col_types = FALSE)
  check_required_columns(dat, c("university", "replicate", "QHQb.total", "GHQc.total"), "study_10")

  # Article excludes University of Ottawa; in the shared file, McMaster and Waterloo are university codes 1 and 3.
  data2 <- dat[dat$university %in% c(1, 3), ]
  data2$replicate <- factor(data2$replicate)
  data2$university_factor <- factor(data2$university)

  # id 6: GHQ-c social dysfunction, main effect of data-collection semester/replicate.
  ghqc_model <- stats::aov(GHQc.total ~ replicate * university_factor, data = data2)
  ghqc_extract <- extract_aov_row(ghqc_model, "replicate", "study_10 GHQc.total")
  ghqc_row <- ghqc_extract$effect
  ghqc_residual <- ghqc_extract$residual
  ghqc_f <- as.numeric(ghqc_row$`F value`)
  ghqc_df1 <- as.numeric(ghqc_row$Df)
  ghqc_df2 <- as.numeric(ghqc_residual$Df)

  # id 7: GHQ-b anxiety/insomnia, main effect of data-collection semester/replicate.
  # The raw CSV column is misspelled QHQb.total in the public file; this script preserves that exact name.
  ghqb_model <- stats::aov(QHQb.total ~ replicate * university_factor, data = data2)
  ghqb_extract <- extract_aov_row(ghqb_model, "replicate", "study_10 QHQb.total")
  ghqb_row <- ghqb_extract$effect
  ghqb_residual <- ghqb_extract$residual
  ghqb_f <- as.numeric(ghqb_row$`F value`)
  ghqb_df1 <- as.numeric(ghqb_row$Df)
  ghqb_df2 <- as.numeric(ghqb_residual$Df)

  rows <- dplyr::bind_rows(
    make_recomputed_row(
      id = 6,
      study_id = "study_10",
      study_DOI = "10.1037/stl0000338",
      recomputation_status = "recomputed_from_raw_data",
      stat_test = "factorial_between_anova",
      reported_result = "GHQ-c social dysfunction: main effect of semester, F(2, 1043) = 103.50, p < .001",
      reported_p_value = 0.001,
      reported_p_operator = "<",
      reported_p_sidedness = "omnibus",
      reported_effect_size_type = "eta_p2_not_reported_recovered_from_F",
      reported_effect_size_value = NA_real_,
      p_value = as.numeric(ghqc_row$`Pr(>F)`),
      p_operator = "=",
      p_sidedness = "omnibus",
      f_value = ghqc_f,
      f_df1 = ghqc_df1,
      f_df2 = ghqc_df2,
      n_total = nrow(data2),
      effect_size_type = "eta_p2_from_F",
      effect_size_value = eta_p2_from_f(ghqc_f, ghqc_df1, ghqc_df2),
      raw_data_file = input_path,
      raw_variable_names = "GHQc.total; replicate; university",
      model_formula = "GHQc.total ~ replicate * factor(university), after filtering university codes 1 and 3",
      contrast_direction = "omnibus main effect of replicate/semester on GHQ-c social dysfunction",
      analysis_label = "ghqc_social_dysfunction_semester_main_effect",
      statistic_source = "stats::aov() ANOVA table",
      bayesian_input_status = bayes_status_from_test("factorial_between_anova"),
      extraction_note = paste0(
        "Recovered Table 2 main effect of data collection semester for GHQ-c. ",
        "Filtered N = ", nrow(data2), ". This corrects the earlier script, which targeted the wrong ANOVA row."
      )
    ),
    make_recomputed_row(
      id = 7,
      study_id = "study_10",
      study_DOI = "10.1037/stl0000338",
      recomputation_status = "recomputed_from_raw_data",
      stat_test = "factorial_between_anova",
      reported_result = "GHQ-b anxiety/insomnia: main effect of semester, F(2, 1043) = 25.51, p < .001",
      reported_p_value = 0.001,
      reported_p_operator = "<",
      reported_p_sidedness = "omnibus",
      reported_effect_size_type = "eta_p2_not_reported_recovered_from_F",
      reported_effect_size_value = NA_real_,
      p_value = as.numeric(ghqb_row$`Pr(>F)`),
      p_operator = "=",
      p_sidedness = "omnibus",
      f_value = ghqb_f,
      f_df1 = ghqb_df1,
      f_df2 = ghqb_df2,
      n_total = nrow(data2),
      effect_size_type = "eta_p2_from_F",
      effect_size_value = eta_p2_from_f(ghqb_f, ghqb_df1, ghqb_df2),
      raw_data_file = input_path,
      raw_variable_names = "QHQb.total [raw-file spelling]; replicate; university",
      model_formula = "QHQb.total ~ replicate * factor(university), after filtering university codes 1 and 3",
      contrast_direction = "omnibus main effect of replicate/semester on GHQ-b anxiety/insomnia",
      analysis_label = "ghqb_anxiety_insomnia_semester_main_effect",
      statistic_source = "stats::aov() ANOVA table",
      bayesian_input_status = bayes_status_from_test("factorial_between_anova"),
      extraction_note = paste0(
        "Recovered Table 2 main effect of data collection semester for GHQ-b. ",
        "The public CSV uses the column name QHQb.total although the article labels the subscale GHQ-b. Filtered N = ",
        nrow(data2), "."
      )
    )
  )

  write_recomputed_results(rows, output_path)
}
