# R/reproduce/study_20.R
# Study 20: Rosman & Kerwer. DOI: 10.1177/14757257221098860
# Reconstructs rows id 13 and id 14 from the raw CSV using the supplied lavaan latent-difference model.

if (!exists("make_recomputed_row")) source("R/reproduce/00_reproduction_helpers.R")

reproduce_study_20 <- function(
    input_path = "data/raw/study_20/PA_sj-csv-2-plj-10.1177_14757257221098860.csv",
    output_path = "outputs/reproduced/study_20_recomputed.csv"
) {
  check_required_packages(c("lavaan", "dplyr", "tibble", "readr"))
  input_path <- resolve_existing_file(c(input_path, "data/raw/study_20/PA_sj-csv-2-plj-10.1177_14757257221098860.csv"), "study_20 CSV")

  dat <- utils::read.csv(input_path, sep = ";", header = TRUE,
                         na.strings = c("-999", "", "NA"), stringsAsFactors = FALSE)

  frg_cols <- c(
    "FRG_A_01_t1", "FRG_A_02_t1", "FRG_A_03_t1", "FRG_A_04_t1", "FRG_A_05_t1",
    "FRG_M_01_t1", "FRG_M_02_t1", "FRG_M_03_t1", "FRG_M_04_t1", "FRG_M_05_t1",
    "FRG_E_01_t1", "FRG_E_02_t1", "FRG_E_03_t1", "FRG_E_04_t1", "FRG_E_05_t1",
    "FRG_A_01_t2", "FRG_A_02_t2", "FRG_A_03_t2", "FRG_A_04_t2", "FRG_A_05_t2",
    "FRG_M_01_t2", "FRG_M_02_t2", "FRG_M_03_t2", "FRG_M_04_t2", "FRG_M_05_t2",
    "FRG_E_01_t2", "FRG_E_02_t2", "FRG_E_03_t2", "FRG_E_04_t2", "FRG_E_05_t2"
  )
  check_required_columns(dat, c("group_t2", frg_cols), "study_20")

  dat$FRG_A_t1 <- rowMeans(dat[, c("FRG_A_01_t1", "FRG_A_02_t1", "FRG_A_03_t1", "FRG_A_04_t1", "FRG_A_05_t1")])
  dat$FRG_M_t1 <- rowMeans(dat[, c("FRG_M_01_t1", "FRG_M_02_t1", "FRG_M_03_t1", "FRG_M_04_t1", "FRG_M_05_t1")])
  dat$FRG_E_t1 <- rowMeans(dat[, c("FRG_E_01_t1", "FRG_E_02_t1", "FRG_E_03_t1", "FRG_E_04_t1", "FRG_E_05_t1")])
  dat$FRG_A_t2 <- rowMeans(dat[, c("FRG_A_01_t2", "FRG_A_02_t2", "FRG_A_03_t2", "FRG_A_04_t2", "FRG_A_05_t2")])
  dat$FRG_M_t2 <- rowMeans(dat[, c("FRG_M_01_t2", "FRG_M_02_t2", "FRG_M_03_t2", "FRG_M_04_t2", "FRG_M_05_t2")])
  dat$FRG_E_t2 <- rowMeans(dat[, c("FRG_E_01_t2", "FRG_E_02_t2", "FRG_E_03_t2", "FRG_E_04_t2", "FRG_E_05_t2")])

  dat$D_FRG_Index_t1 <- dat$FRG_E_t1 - 0.5 * (dat$FRG_A_t1 + dat$FRG_M_t1)
  dat$D_FRG_Index_t2 <- dat$FRG_E_t2 - 0.5 * (dat$FRG_A_t2 + dat$FRG_M_t2)

  pre_mean <- mean(dat$D_FRG_Index_t1, na.rm = TRUE)
  pre_sd <- stats::sd(dat$D_FRG_Index_t1, na.rm = TRUE)
  dat$D_FRG_Index_t2 <- (dat$D_FRG_Index_t2 - pre_mean) / pre_sd
  dat$D_FRG_Index_t1 <- (dat$D_FRG_Index_t1 - pre_mean) / pre_sd

  dat$DV <- as.numeric(dat$group_t2 != "Kontrolle")
  dat$R <- as.numeric(!(dat$group_t2 %in% c("Kontrolle", "Volition")))
  dat$Social_Interaction <- as.numeric(dat$group_t2 == "Soziale Interaktion")

  latent_difference_model <- '
    delta_FRG =~ 1*D_FRG_Index_t2
    D_FRG_Index_t2 ~ 1*D_FRG_Index_t1
    delta_FRG ~ b0*1
    D_FRG_Index_t1 ~ 1
    D_FRG_Index_t2 ~ 0
    delta_FRG ~ D_FRG_Index_t1
    delta_FRG ~~ delta_FRG
    D_FRG_Index_t1 ~~ D_FRG_Index_t1
    D_FRG_Index_t2 ~~ 0*D_FRG_Index_t2
    delta_FRG ~ b1*DV + b2*R + b3*Social_Interaction
    D_FRG_Index_t1 ~ DV + R + Social_Interaction
    m_C := b0
    m_DV := b0 + b1
    m_R := b0 + b1 + b2
    m_SI := b0 + b1 + b2 + b3
    Test_H1a := (m_DV + m_R + m_SI)/3 - b0
    Test_H1b := (m_R + m_SI)/2 - m_DV
    Test_H1c := m_SI - m_R
    DVvsCONTROL := b1
    RvsCONTROL := b1 + b2
    SIvsCONTROL := b1 + b2 + b3
    RvsDV := b2
    SIvsDV := b2 + b3
    SIvsR := b3
  '

  fit <- lavaan::sem(
    model = latent_difference_model,
    data = dat,
    estimator = "ML",
    std.lv = FALSE,
    fixed.x = FALSE,
    missing = "FIML"
  )

  estimates <- lavaan::parameterEstimates(fit)
  estimates <- estimates[estimates$op == ":=" & estimates$lhs %in% c("Test_H1a", "Test_H1c"), ]
  if (nrow(estimates) != 2) stop("Could not extract Test_H1a and Test_H1c from lavaan parameterEstimates().", call. = FALSE)
  estimates$z_value <- estimates$est / estimates$se
  estimates$p_one_sided <- stats::pnorm(estimates$z_value, lower.tail = FALSE)
  estimates$p_two_sided <- 2 * stats::pnorm(abs(estimates$z_value), lower.tail = FALSE)

  h1a <- estimates[estimates$lhs == "Test_H1a", ]
  h1c <- estimates[estimates$lhs == "Test_H1c", ]

  rows <- dplyr::bind_rows(
    make_recomputed_row(
      id = 13,
      study_id = "study_20",
      study_DOI = "10.1177/14757257221098860",
      recomputation_status = "recomputed_from_raw_data_supplied_sem_model",
      stat_test = "sem_contrast",
      reported_result = "((MDV + MR + MRSI)/3 - MC = .372, SE = .183, p = .021; one-sided test)",
      reported_p_value = 0.021,
      reported_p_operator = "=",
      reported_p_sidedness = "one_sided",
      p_value = as.numeric(h1a$p_one_sided),
      p_operator = "=",
      p_sidedness = "one_sided",
      z_value = as.numeric(h1a$z_value),
      n_total = nrow(dat),
      effect_size_type = "none",
      estimate = as.numeric(h1a$est),
      se_estimate = as.numeric(h1a$se),
      raw_data_file = input_path,
      raw_variable_names = paste(c("group_t2", frg_cols, "DV", "R", "Social_Interaction", "D_FRG_Index_t1", "D_FRG_Index_t2"), collapse = "; "),
      model_formula = "lavaan latent difference score model; Test_H1a := (m_DV + m_R + m_SI)/3 - b0",
      contrast_direction = "average of intervention latent-change means minus control latent-change mean",
      analysis_label = "h1a_average_intervention_vs_control",
      statistic_source = "lavaan::sem() parameterEstimates for defined parameter Test_H1a",
      bayesian_input_status = bayes_status_from_test("sem_contrast"),
      extraction_note = paste0("Recovered estimate, SE, z and one-sided directional p from supplied lavaan model. Two-sided p = ", signif(h1a$p_two_sided, 6), ".")
    ),
    make_recomputed_row(
      id = 14,
      study_id = "study_20",
      study_DOI = "10.1177/14757257221098860",
      recomputation_status = "recomputed_from_raw_data_supplied_sem_model",
      stat_test = "sem_contrast",
      reported_result = "(MRSI - MR = .081, SE = .227, p = .360; one-sided test)",
      reported_p_value = 0.360,
      reported_p_operator = "=",
      reported_p_sidedness = "one_sided",
      p_value = as.numeric(h1c$p_one_sided),
      p_operator = "=",
      p_sidedness = "one_sided",
      z_value = as.numeric(h1c$z_value),
      n_total = nrow(dat),
      effect_size_type = "none",
      estimate = as.numeric(h1c$est),
      se_estimate = as.numeric(h1c$se),
      raw_data_file = input_path,
      raw_variable_names = paste(c("group_t2", frg_cols, "DV", "R", "Social_Interaction", "D_FRG_Index_t1", "D_FRG_Index_t2"), collapse = "; "),
      model_formula = "lavaan latent difference score model; Test_H1c := m_SI - m_R",
      contrast_direction = "reflection plus social interaction latent-change mean minus reflection-only latent-change mean",
      analysis_label = "h1c_social_interaction_vs_reflection",
      statistic_source = "lavaan::sem() parameterEstimates for defined parameter Test_H1c",
      bayesian_input_status = bayes_status_from_test("sem_contrast"),
      extraction_note = paste0("Recovered estimate, SE, z and one-sided directional p from supplied lavaan model. Two-sided p = ", signif(h1c$p_two_sided, 6), ".")
    )
  )

  write_recomputed_results(rows, output_path)
}
