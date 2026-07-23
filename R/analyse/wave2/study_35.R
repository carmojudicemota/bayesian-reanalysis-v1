clean_study_35_score <- function(x) {
  as.numeric(haven::zap_labels(haven::zap_missing(x))) * 100 / 7
}

load_study_35_wave2_data <- function(
    path ="data/raw/study_35/Untitled3.sav") {
  if (!file.exists(path)) {
    stop("Study 35 data file does not exist: ", path, call. = FALSE)
  }
  
  raw <- haven::read_sav(path,user_na = FALSE)
  
  required_columns <- c("Participant",
                        "Sex",
                        "CORRECT_ANSWERS_RR",
                        "CORRECT_ANSWERS_MMRP_A",
                        "CORRECT_ANSWERS_RP_A"
                        )
  
  missing_columns <- setdiff(required_columns, names(raw))
  
  if (length(missing_columns) > 0L) {
    stop(
      "Study 35 is missing required columns: ",
      paste(
        missing_columns,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  wide <- tibble::tibble(
    participant = factor(as.character(raw$Participant)),
    Sex = factor(as.character(raw$Sex)),
    RR = clean_study_35_score(raw$CORRECT_ANSWERS_RR),
    MMRP = clean_study_35_score(raw$CORRECT_ANSWERS_MMRP_A),
    RP = clean_study_35_score(raw$CORRECT_ANSWERS_RP_A)
  ) |>
    tidyr::drop_na()
  
  if (nrow(wide) != 60L) {
    stop(
      "Unexpected Study 35 complete-case sample: ",
      nrow(wide),
      ". Expected 60.",
      call. = FALSE
    )
  }
  
  sex_counts <- table(wide$Sex)
  if (
    length(sex_counts) != 2L ||
    !identical(
      sort(as.integer(sex_counts)),
      c(15L, 45L)
    )
  ) {
    stop(
      "Unexpected Study 35 Sex-group counts: ",
      paste(
        names(sex_counts),
        as.integer(sex_counts),
        collapse = "; "
      ),
      call. = FALSE
    )
  }
  
  long <- wide |>
    tidyr::pivot_longer(
      cols = c("RR","MMRP","RP"),
      names_to = "condition",
      values_to = "score"
    ) |>
    dplyr::mutate(
      condition = factor(
        .data$condition,
        levels = c("RR","MMRP","RP"),
        labels = c("multitask_reread","multitask_retrieval","retrieval_only")
      )
    ) |>
    dplyr::select(participant,Sex,condition,score) |> as.data.frame()
  
  participant_counts <- table(long$participant)
  
  if (nrow(long) != 180L || any(participant_counts != 3L)) {
    stop(
      "Study 35 should contain 180 long-format rows ",
      "and three observations per participant.",
      call. = FALSE
    )
  }
  
  long
}


compute_study_35_bayes_factors <- function(claim,priors) {
  
  data <- load_study_35_wave2_data()
  
  model_null <-
    score ~
    Sex +
    Sex:condition +
    participant
  
  model_alt <-
    score ~
    Sex +
    condition +
    Sex:condition +
    participant
  
  compute_mixed_anova_model_pair(claim = claim,
                                 data = data,
                                 model_null = model_null,
                                 model_alt = model_alt,
                                 priors = priors,
                                 participant_column = "participant"
                                 )
}