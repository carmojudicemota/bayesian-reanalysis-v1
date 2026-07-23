source("R/prepare/read_ali_sources.R")


is_blank_value <- function(x) {
  is.na(x) | trimws(as.character(x)) == ""
}

assert_columns_exist <- function(data, required, object_name) {
  missing_columns <- setdiff(required, names(data))
  if (length(missing_columns) > 0) {
    stop(
      object_name,
      " is missing the followung required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }
}


build_claims_draft <- function(
    claim_map_path = "config/claim_map.csv",
    verified_path = "data/derived/verified_results_draft.csv",
    output_path = "data/derived/claims_draft.csv"
) {
  claim_map <- readr::read_csv(
    claim_map_path,
    col_types = readr::cols(
      .default = readr::col_character()
    ),
    na = c("", "NA")
  ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ stringr::str_squish(.x)
      )
    )
  
  assert_columns_exist(
    claim_map,
    c(
      "claim_id",
      "study_id",
      "result_position",
      "source_result_id",
      "role",
      "direction",
      "status",
      "note"
    ),
    "claim_map"
  )
  
  if (nrow(claim_map) == 0) {
    stop(
      "config/claim_map.csv has no claim rows. "
      )
  }
  
  if (anyDuplicated(claim_map$claim_id)) {
    duplicated_ids <- unique(
      claim_map$claim_id[
        duplicated(claim_map$claim_id)
      ]
    )
    stop(
      "The following claim_id values are duplicated: ",
      paste(duplicated_ids, collapse = ", ")
    )
  }
  
  duplicated_sources <- claim_map |>
    dplyr::filter(
      status == "ready",
      !is_blank_value(source_result_id)
    ) |>
    dplyr::count(source_result_id) |>
    dplyr::filter(n > 1)
  
  if (nrow(duplicated_sources) > 0) {
    stop(
      "Ready claims reuse source_result_id values: ",
      paste(
        duplicated_sources$source_result_id,
        collapse = ", "
      )
    )
  }
  
  verified <- readr::read_csv(
    verified_path,
    show_col_types = FALSE,
    na = c("", "NA")
  )
  
  assert_columns_exist(
    verified,
    c(
      "result_id",
      "source_row_id",
      "source_study_id",
      "source_file",
      "study_id",
      "study_DOI",
      "recomputation_status",
      "stat_test",
      "reported_result",
      "p_value",
      "p_operator",
      "p_sidedness",
      "t_value",
      "t_df",
      "f_value",
      "f_df1",
      "f_df2",
      "z_value",
      "chi2_value",
      "chi2_df",
      "r_value",
      "n1",
      "n2",
      "n_total",
      "n_eff",
      "effect_size_type",
      "effect_size_value",
      "estimate",
      "se_estimate",
      "raw_data_file",
      "raw_variable_names",
      "model_formula",
      "contrast_direction",
      "extraction_note"
    ),
    "verified_results_draft"
  )
  
  ali_claims <- read_ali_primary_claims() |>
    dplyr::rename(
      doi_phase1 = doi
    )
  
  ali_results <- read_ali_results() |>
    dplyr::rename(
      doi_phase2 = doi
    )
  
  verified_for_join <- verified |>
    dplyr::transmute(
      source_result_id = result_id,
      verified_study_id = study_id,
      source_row_id,
      source_study_id,
      source_file,
      project_doi = study_DOI,
      project_result_status = recomputation_status,
      project_verified_result = reported_result,
      frequentist_test = stat_test,
      p_value,
      p_operator,
      p_sidedness,
      t_value,
      t_df,
      f_value,
      f_df1,
      f_df2,
      z_value,
      chi2_value,
      chi2_df,
      r_value,
      n1,
      n2,
      n_total,
      n_eff,
      effect_size_type,
      effect_size_value,
      estimate,
      se_estimate,
      raw_data_file,
      raw_variable_names,
      model_formula,
      contrast_direction,
      extraction_note
    )
  
  joined <- claim_map |>
    dplyr::left_join(
      ali_claims,
      by = "study_id"
    ) |>
    dplyr::left_join(
      ali_results,
      by = c(
        "study_id",
        "result_position"
      )
    ) |>
    dplyr::left_join(
      verified_for_join,
      by = "source_result_id"
    )
  
  unmatched_sources <- joined |>
    dplyr::filter(
      !is_blank_value(source_result_id),
      is.na(verified_study_id)
    )
  
  if (nrow(unmatched_sources) > 0) {
    stop(
      "These source_result_id values were not found: ",
      paste(
        unmatched_sources$source_result_id,
        collapse = ", "
      )
    )
  }
  
  # Orphan check: claim_map drives this join, so any study that reproduced but has
  # no claim_map row is dropped silently and never becomes a claim. That is how
  # study_27, study_29 and study_37 went missing. Report it loudly instead.
  reproduced_studies <- sort(unique(verified$study_id))
  mapped_studies <- sort(unique(claim_map$study_id))
  unmapped_studies <- setdiff(reproduced_studies, mapped_studies)

  if (length(unmapped_studies) > 0) {
    warning(
      "These studies have reproduced results but NO rows in config/claim_map.csv, ",
      "so they are excluded from claims.csv: ",
      paste(unmapped_studies, collapse = ", "),
      ". Add claim_map rows for them (or record why they are out of scope).",
      call. = FALSE
    )
  }

  inconsistent_studies <- joined |>
    dplyr::filter(
      !is.na(verified_study_id),
      study_id != verified_study_id
    )
  
  if (nrow(inconsistent_studies) > 0) {
    stop(
      "The study_id does not agree with the verified result for: ",
      paste(
        inconsistent_studies$claim_id,
        collapse = ", "
      )
    )
  }
  
  claims <- joined |>
    dplyr::transmute(
      claim_id,
      study_id,
      doi = dplyr::coalesce(
        doi_phase1,
        doi_phase2,
        project_doi
      ),
      claim = ali_claim,
      role,
      direction,
      status,
      note,
      result_position,
      source_result_id,
      article_result_from_phase1,
      ali_test_name,
      ali_reported_result,
      ali_reproduced_result,
      ali_result_status,
      # PROJECT SCOPE RULE: only results Almuhanna's Phase 2 marked "Fully
      # reproducible" are analysed. Everything else stays in the registry with a
      # written reason (pre-registration item 9, Sampling Plan) but is never fed
      # to a Bayesian model. Anything we discover about an out-of-scope result --
      # a rounding-level effect-size mismatch, a transcription error in the
      # source table -- is recorded in `note` only; it does not change eligibility.
      in_scope = !is.na(ali_result_status) &
        trimws(ali_result_status) == "Fully reproducible",
      scope_exclusion_reason = dplyr::if_else(
        in_scope,
        NA_character_,
        paste0(
          "Out of scope: Almuhanna Phase 2 status is '",
          dplyr::coalesce(ali_result_status, "missing"),
          "', not 'Fully reproducible'."
        )
      ),
      project_verified_result,
      project_result_status,
      frequentist_test,
      p_value,
      p_operator,
      p_sidedness,
      t_value,
      t_df,
      f_value,
      f_df1,
      f_df2,
      z_value,
      chi2_value,
      chi2_df,
      r_value,
      n1,
      n2,
      n_total,
      n_eff,
      effect_size_type,
      effect_size_value,
      estimate,
      se_estimate,
      raw_data_file,
      raw_variable_names,
      model_formula,
      contrast_direction,
      extraction_note,
      source_file,
      source_row_id,
      source_study_id
    ) |>
    dplyr::arrange(
      study_id,
      claim_id
    )
  
  dir.create(
    dirname(output_path),
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  readr::write_csv(
    claims,
    output_path,
    na = ""
  )
  
  message(
    "Created ",
    output_path,
    " with ",
    nrow(claims),
    " claims from ",
    dplyr::n_distinct(claims$study_id),
    " studies."
  )
  
  invisible(claims)
}