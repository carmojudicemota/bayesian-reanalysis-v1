library(tidyverse)

# File paths
input_path <- "/Users/xiaoyixu/Documents/文稿 - Xiaoyi的MacBook Pro/UBristol 2026 Summer/Wave1_Validation/Data/claims.csv"
selected_output_path <- "/Users/xiaoyixu/Documents/文稿 - Xiaoyi的MacBook Pro/UBristol 2026 Summer/Wave1_Validation/Data/Outputs/wave1_selected_claims.csv"


# Claims shown in the table
candidate_claim_ids <- c(
  "study_05_claim_01",
  "study_05_claim_02",
  "study_14_claim_01",
  "study_14_claim_02",
  "study_21_claim_01",
  "study_27_claim_02",
  "study_30_claim_01",
  "study_33_claim_01",
  "study_33_claim_02",
  "study_41_claim_01",
  "study_41_claim_02",
  "study_49_claim_01",
  "study_49_claim_02",
  "study_51_claim_01",
  "study_51_claim_02",
  "study_53_claim_01",
  "study_53_claim_02"
)

# Number selected from each test type
sample_plan <- tibble(
  frequentist_test = c(
    "independent_t_test",
    "paired_t_test",
    "pearson_correlation",
    "one_sample_t_test"
  ),
  n_select = c(3, 3, 1, 1)
)

# Read the full dataset
claims <- read_csv(
  input_path,
  show_col_types = FALSE
)

# Check required columns
required_columns <- c("claim_id", "frequentist_test")
missing_columns <- setdiff(required_columns, names(claims))

if (length(missing_columns) > 0) {
  stop(
    "Missing required columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

# Keep only the candidate claims
candidates <- claims %>%
  filter(claim_id %in% candidate_claim_ids)

# Check that all candidate claims were found
missing_claims <- setdiff(
  candidate_claim_ids,
  candidates$claim_id
)

if (length(missing_claims) > 0) {
  stop(
    "Candidate claims not found: ",
    paste(missing_claims, collapse = ", ")
  )
}

# Check the number available in each test type
availability <- candidates %>%
  count(frequentist_test, name = "available_n") %>%
  right_join(sample_plan, by = "frequentist_test") %>%
  mutate(available_n = replace_na(available_n, 0L))

if (any(availability$available_n < availability$n_select)) {
  stop("Not enough candidates for the sampling plan.")
}

# Stratified random sampling without replacement
set.seed(20260723)

selected <- purrr::map2_dfr(
  sample_plan$frequentist_test,
  sample_plan$n_select,
  function(test_type, sample_size) {
    candidates %>%
      filter(frequentist_test == test_type) %>%
      slice_sample(
        n = sample_size,
        replace = FALSE
      )
  }
) %>%
  arrange(frequentist_test, claim_id)

# Check the selected sample
selection_check <- selected %>%
  count(frequentist_test, name = "selected_n") %>%
  right_join(sample_plan, by = "frequentist_test")

if (
  nrow(selected) != 8 ||
  any(selection_check$selected_n != selection_check$n_select)
) {
  stop("The selected sample does not match the sampling plan.")
}

# Display selected claims
selected %>%
  select(claim_id, frequentist_test) %>%
  print(n = Inf)

# Save the selected claims
write_csv(
  selected,
  selected_output_path,
  na = ""
)
