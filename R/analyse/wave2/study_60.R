source("R/reproduce/study_60.R")

study_60_outcome <- function(claim_id) {
  switch(
    claim_id,
    study_60_claim_01 ="fieldbelong_exact_winsorized",
    study_60_claim_02 ="include_1",
    stop("Unknown Study 60 claim: ",claim_id,call. = FALSE)
  )
}

load_study_60_wave2_data <- function(outcome_column,path = paste0("data/raw/study_60/","Syllabus_Safety_Cues_Instructor_", "Gender_Data_Sp21_OSF.sav")) {
  if (!file.exists(path)) {stop("Study 60 data file does not exist: ", path,call. = FALSE)}
  
  allowed_outcomes <- c("fieldbelong_exact_winsorized","include_1")
  
  if (!outcome_column %in% allowed_outcomes) {
    stop(
      "Unsupported Study 60 outcome: ",
      outcome_column,
      ". Expected one of: ",
      paste(
        allowed_outcomes,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  raw <- as.data.frame(haven::read_sav(path))
  validate_study_60_data(raw)
  outcome <- if (identical(outcome_column,"fieldbelong_exact_winsorized")
  ) {
    field_belonging_raw <- reconstruct_field_belonging_raw(raw)
    field_belonging_winsorized <- winsorize_to_nearest_observed(field_belonging_raw,z = 3)
    
    affected_ids <- sort(
      as.numeric(
        raw$participantID[
          c(field_belonging_winsorized$low_index, field_belonging_winsorized$high_index)
        ]
      )
    )
    
    if (!identical(affected_ids, c(128, 320))) {
      stop(
        "Unexpected Study 60 winsorized participants: ",
        paste(
          affected_ids,
          collapse = ", "
        ),
        call. = FALSE
      )
    }
    
    field_belonging_winsorized$values
  } else {
    as.numeric(raw$include_1)
  }
  
  data <- data.frame(outcome = as.numeric(outcome),
    
    A = factor(
      raw$IV_safetycues,
      levels = c(0, 1),
      labels = c("control","safety")
    ),
    
    B = factor(
      raw$IV_profgender,
      levels = c(0, 1),
      labels = c("man","woman")
    )
  )
  
  data <- data[
    stats::complete.cases(data),
    ,
    drop = FALSE
  ]
  
  if (nrow(data) == 0L) {
    stop(
      "Study 60 has no complete observations for ",
      outcome_column,
      ".",
      call. = FALSE
    )
  }
  
  data
}


compute_study_60_bayes_factors <- function(claim,priors) {
  
  outcome_column <- study_60_outcome(claim$claim_id)
  data <- load_study_60_wave2_data(outcome_column = outcome_column)
  compute_factorial_main_effect_bfs(claim = claim, data = data, priors = priors)
}