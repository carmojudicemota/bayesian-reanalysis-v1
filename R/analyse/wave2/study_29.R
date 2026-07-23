load_study_29_wave2_data <- function(
    path = paste0("data/raw/study_29/",
                  "Morling and Lee Faculty Sample Open Data.csv"
    )) {
  if (!file.exists(path)) {
    stop(
      "Study 29 data file does not exist: ",
      path,
      call. = FALSE
    )
  }
  raw <- readr::read_csv(
    path,
    show_col_types = FALSE
  )
  required_columns <- c("univkids","Title","Department")
  missing_columns <- setdiff(required_columns,names(raw))
  if (length(missing_columns) > 0L) {
    stop(
      "Study 29 is missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }
  data <- raw |>
    dplyr::filter(
      .data$Title %in% c(1, 2),
      .data$Department %in% c(1, 2)
    ) |>
    dplyr::transmute(
      outcome = as.numeric(.data$univkids),
      
      A = factor(
        .data$Title,
        levels = c(1, 2),
        labels = c("associate_teaching_professor","associate_professor")
      ),
      
      B = factor(
        .data$Department,
        levels = c(1, 2),
        labels = c("department_1","department_2")
      )
    ) |>
    tidyr::drop_na()
  
  if (nrow(data) == 0L) {
    stop(
      "Study 29 has no complete observations after filtering.",
      call. = FALSE
    )
  }
  
  data
}

compute_study_29_bayes_factors <- function(claim,priors) {
  data <- load_study_29_wave2_data()
  compute_factorial_main_effect_bfs(claim = claim,data = data,priors = priors)
}