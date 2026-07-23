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
  
  compute_mixed_anova_model_pair(claim = claim,
                                 data = data,
                                 model_null = model_null,
                                 model_alt = model_alt,
                                 priors = priors,
                                 participant_column = "participant"
                                 )
}