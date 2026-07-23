source("R/prepare/build_claims.R")

build_claims_draft(
  claim_map_path = "config/claim_map.csv",
  verified_path = "data/derived/verified_results_draft.csv",
  output_path = "data/derived/claims_draft.csv"
)

file.copy(
  from = "data/derived/claims_draft.csv",
  to = "data/derived/claims.csv",
  overwrite = TRUE
)

message("Refreshed claims.csv from the existing verified results.")