source("R/prepare/read_ali_sources.R")

build_claim_mapping_candidates <- function(
    verified_path =
      "data/derived/verified_results_draft.csv",
    candidates_path =
      "outputs/phase1/claim_mapping_candidates.csv",
    draft_map_path =
      "config/claim_map_draft.csv"
) {
  verified <- readr::read_csv(
    verified_path,
    show_col_types = FALSE,
    na = c("", "NA")
  )
  
  ali_results <- read_ali_results()
  
  project_studies <- unique(
    verified$study_id
  )
  
  ali_targets <- ali_results |>
    dplyr::filter(
      study_id %in% project_studies
    ) |>
    dplyr::mutate(
      position_order = dplyr::case_when(
        result_position == "key" ~ 1L,
        result_position == "second" ~ 2L,
        TRUE ~ 99L
      )
    ) |>
    dplyr::arrange(
      study_id,
      position_order
    )
  
  verified_candidates <- verified |>
    dplyr::transmute(
      study_id,
      candidate_source_result_id = result_id,
      candidate_source_row_id = source_row_id,
      candidate_project_result = reported_result,
      candidate_test = stat_test,
      candidate_p_value = p_value,
      candidate_sidedness = p_sidedness,
      candidate_direction = contrast_direction,
      candidate_status = recomputation_status,
      candidate_n_total = n_total,
      candidate_t_df = t_df,
      candidate_f_df1 = f_df1,
      candidate_f_df2 = f_df2,
      candidate_note = extraction_note
    )
  
  candidates <- ali_targets |>
    dplyr::left_join(
      verified_candidates,
      by = "study_id"
    ) |>
    dplyr::select(
      study_id,
      result_position,
      ali_test_name,
      ali_reported_result,
      ali_reproduced_result,
      ali_result_status,
      candidate_source_result_id,
      candidate_source_row_id,
      candidate_project_result,
      candidate_test,
      candidate_p_value,
      candidate_sidedness,
      candidate_direction,
      candidate_status,
      candidate_n_total,
      candidate_t_df,
      candidate_f_df1,
      candidate_f_df2,
      candidate_note
    ) |>
    dplyr::arrange(
      study_id,
      result_position,
      candidate_source_result_id
    )
  
  claim_map_draft <- ali_targets |>
    dplyr::group_by(study_id) |>
    dplyr::arrange(
      position_order,
      .by_group = TRUE
    ) |>
    dplyr::mutate(
      claim_number = dplyr::row_number(),
      claim_id = sprintf(
        "%s_claim_%02d",
        study_id,
        claim_number
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      claim_id,
      study_id,
      result_position,
      source_result_id = NA_character_,
      role = dplyr::if_else(
        result_position == "key",
        "primary",
        "supplementary"
      ),
      direction = "unclear",
      status = "pending",
      note = paste0(
        "Select and verify a project result row for the ",
        result_position,
        " result."
      )
    )
  
  dir.create(
    dirname(candidates_path),
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  readr::write_csv(
    candidates,
    candidates_path,
    na = ""
  )
  
  readr::write_csv(
    claim_map_draft,
    draft_map_path,
    na = ""
  )
  
  message(
    "Created ",
    candidates_path,
    " with ",
    nrow(candidates),
    " candidate pairings."
  )
  
  message(
    "Created ",
    draft_map_path,
    " with ",
    nrow(claim_map_draft),
    " inherited target rows."
  )
  
  invisible(
    list(
      candidates = candidates,
      claim_map_draft = claim_map_draft
    )
  )
}