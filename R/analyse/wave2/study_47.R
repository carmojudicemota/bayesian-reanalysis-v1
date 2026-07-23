load_study_47_wave2_data <- function(
    path = paste0("data/raw/study_47/","Outside_Assistance_Dataset.sav")) {
  
  if (!file.exists(path)) {
    stop("Study 47 data file does not exist: ",path,call. = FALSE)
  }
  
  raw <- as.data.frame(haven::read_sav(path))
  required_columns <- c(
    "Condition",
    "PercentRember",
    "PercentCorrectMC",
    "PercentCorrectOE"
    )
  
  missing_columns <- setdiff(required_columns,names(raw))
  
  if (length(missing_columns) > 0L) {
    stop(
      "Study 47 is missing required columns: ",
      paste(
        missing_columns,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  prediction_columns <- c(
    "PercentRember",
    "PercentCorrectMC",
    "PercentCorrectOE"
  )
  
  wide <- raw[
    stats::complete.cases(
      raw[c("Condition",prediction_columns)]
    ),
    c("Condition",prediction_columns
    ),
    drop = FALSE
  ]
  
  for (column in prediction_columns) {
    wide[[column]] <- as.numeric(wide[[column]])
  }
  
  if (inherits(wide$Condition,"haven_labelled")) {
    wide$Condition <- haven::as_factor(wide$Condition,levels = "values")
  } else {wide$Condition <- factor(wide$Condition)}
  wide$Condition <- droplevels(wide$Condition)
  
  if (nlevels(wide$Condition) != 2L) {
    stop("Study 47 Condition must have exactly two levels.", call. = FALSE)
    }
  
  wide$participant <- factor(seq_len(nrow(wide)))
  long <- tidyr::pivot_longer(tibble::as_tibble(wide),
                              cols = dplyr::all_of(prediction_columns),
                              names_to = "format",
                              values_to = "score"
                              )
  
  long$format <- factor(long$format,
                        levels = prediction_columns,
                        labels = c("remembered_percentage",
                                   "multiple_choice_accuracy",
                                   "open_ended_accuracy")
                        )
  
  long <- long |>
    dplyr::select(participant,Condition,format,score) |> as.data.frame()
  
  participant_counts <- table(long$participant)
  
  if (nrow(wide) != 121L || nrow(long) != 363L ||any(participant_counts != 3L)
  ) {
    stop(
      "Unexpected Study 47 analysis dimensions. ",
      "Expected 121 participants, 363 rows, ",
      "and three observations per participant.",
      call. = FALSE
    )
  }
  
  long
}


compute_study_47_bayes_factors <- function(claim,priors) {
  
  data <- load_study_47_wave2_data()
  fixed_priors <- priors |>
    dplyr::filter(
      .data$prior_family ==
        "anova_cauchy",
      .data$param ==
        "rscale_fixed"
    ) |>
    dplyr::mutate(
      prior_order = match(
        .data$prior_label,
        c(
          "narrow",
          "primary",
          "wide"
        )
      )
    ) |>
    dplyr::arrange(
      .data$prior_order
    )
  
  if (
    nrow(fixed_priors) != 3L ||
    anyNA(fixed_priors$prior_order)
  ) {
    stop(
      "Study 47 requires narrow, primary and wide ",
      "fixed-effect ANOVA priors.",
      call. = FALSE
    )
  }
  
  random_scale <- priors |>
    dplyr::filter(
      .data$prior_family ==
        "anova_cauchy",
      .data$param ==
        "rscale_random",
      .data$prior_label ==
        "primary"
    ) |>
    dplyr::pull(
      .data$value
    )
  
  if (length(random_scale) != 1L) {
    stop(
      "Study 47 requires exactly one primary ",
      "rscale_random prior.",
      call. = FALSE
    )
  }
  
  model_null <-
    score ~
    Condition +
    format +
    participant
  
  model_alt <-
    score ~
    Condition +
    format +
    Condition:format +
    participant
  
  purrr::map_dfr(
    seq_len(nrow(fixed_priors)),
    function(i) {
      prior_label <- fixed_priors$prior_label[i]
      fixed_scale <- as.numeric(fixed_priors$value[i])
      null_bf <- BayesFactor::lmBF(formula = model_null,
                                   data = data,
                                   whichRandom = "participant",
                                   rscaleFixed = fixed_scale,
                                   rscaleRandom = random_scale,
                                   progress = FALSE
                                   )
      
      alternative_bf <- BayesFactor::lmBF(formula = model_alt,
                                          data = data,
                                          whichRandom = "participant",
                                          rscaleFixed = fixed_scale,
                                          rscaleRandom = random_scale,
                                          progress = FALSE
                                          )
      
      comparison <- alternative_bf / null_bf
      extracted <- BayesFactor::extractBF(comparison)
      wave2_row(
        claim = claim,
        prior_label = prior_label,
        rscale = fixed_scale,
        bf10 = extracted$bf[1],
        bf_error = extracted$error[1],
        model_null = paste(
          deparse(model_null),
          collapse = ""
        ),
        model_alt = paste(
          deparse(model_alt),
          collapse = ""
        )
      )
    }
  )
}