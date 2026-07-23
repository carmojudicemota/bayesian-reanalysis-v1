load_study_10_wave2_data <- function(outcome_column,
                                     path = paste0("data/raw/study_10/","master.anonymizedOSF.csv")
                                     ) {

  if (!file.exists(path)) {
    stop("Study 10 data file does not exist: ",path,call. = FALSE)
  }
  
  allowed_outcomes <- c("GHQc.total","QHQb.total")
  
  if (!outcome_column %in% allowed_outcomes) {
    stop("Unsupported Study 10 outcome: ",outcome_column,". Expected one of: ",
      paste(allowed_outcomes,collapse = ", "),call. = FALSE)
  }
  
  raw <- readr::read_csv(path,show_col_types = FALSE)
  required_columns <- c("university","replicate",outcome_column)
  missing_columns <- setdiff(required_columns,names(raw))
  
  if (length(missing_columns) > 0L) {
    stop("Study 10 is missing required columns: ",
         paste(missing_columns,collapse = ", "),
         call. = FALSE
         )
  }
  
  data <- raw |>
    dplyr::filter(
      .data$university %in%
        c(1, 3)
    ) |>
    dplyr::transmute(
      outcome = as.numeric(
        .data[[outcome_column]]
      ),
      
      A = factor(
        .data$replicate
      ),
      
      B = factor(
        .data$university,
        levels = c(1, 3),
        labels = c("university_1","university_3")
      )
    ) |>
    tidyr::drop_na() |>
    as.data.frame()
  
  if (nrow(data) == 0L) {
    stop("Study 10 has no complete observations ","for ",outcome_column,".",call. = FALSE)
  }
  
  data
}


study_10_outcome <- function(claim_id) {
  switch(claim_id, study_10_claim_01 = "GHQc.total", study_10_claim_02 = "QHQb.total",
         stop("Unknown Study 10 claim: ", claim_id, call. = FALSE)
         )
}

compute_study_10_bayes_factors <- function(claim,priors) {
  outcome_column <- study_10_outcome(claim$claim_id)
  data <- load_study_10_wave2_data(outcome_column =outcome_column)
  compute_factorial_main_effect_bfs(claim = claim, data = data, priors = priors)
}