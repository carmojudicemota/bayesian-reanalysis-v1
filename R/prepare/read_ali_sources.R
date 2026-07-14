normalise_ali_study_id <- function(x) {
  number <- suppressWarnings(as.integer(x))
  ifelse(
    is.na(number),
    NA_character_,
    sprintf("study_%02d", number)
  )
}


read_ali_primary_claims <- function(
    path = "data/source/ali/Phase 1.xlsx"
) {
  readxl::read_excel(
    path,
    sheet = "Sheet1"
  ) |>
    dplyr::transmute(
      study_id = normalise_ali_study_id(ID),
      doi = trimws(DOI),
      ali_claim = trimws(`Final claim`),
      article_result_from_phase1 =
        trimws(`Final result`)
    )
}


read_ali_results <- function(
    path = "data/source/ali/Phase 2.xlsx"
) {
  source <- readxl::read_excel(
    path,
    sheet = "Sheet1"
  )
  
  key_results <- source |>
    dplyr::transmute(
      study_id = normalise_ali_study_id(ID),
      doi = trimws(DOI),
      result_position = "key",
      ali_test_name = trimws(`Stat test`),
      ali_reported_result =
        trimws(`Reported key result`),
      ali_reproduced_result =
        trimws(`Reproduced key result`),
      ali_result_status =
        trimws(`Reproducibility (Key result)`)
    )
  
  second_results <- source |>
    dplyr::filter(
      tolower(trimws(`Second result used?`)) == "yes"
    ) |>
    dplyr::transmute(
      study_id = normalise_ali_study_id(ID),
      doi = trimws(DOI),
      result_position = "second",
      ali_test_name = trimws(`Stat test`),
      ali_reported_result =
        trimws(`Reported second result`),
      ali_reproduced_result =
        trimws(`Reproduced second Result`),
      ali_result_status =
        trimws(`Reproducibility (second result)`)
    )
  
  dplyr::bind_rows(
    key_results,
    second_results
  )
}