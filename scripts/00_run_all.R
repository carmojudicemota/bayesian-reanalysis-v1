# scripts/00_run_all.R  — full pipeline, run from the repo root.
# 1 reproductions + compile -> 2 verified draft -> 3 claims draft ->
# 4 directionality -> 5 promote to master -> 6 Bayes factors + concordance -> 7 figures

# 1. reproductions + 98_compile
source("scripts/run_all_reproductions.R")

# 2. verified results draft (feeds build_claims_draft)
source("R/prepare/build_verified_results.R")
build_verified_results_draft()                 # -> data/derived/verified_results_draft.csv

# 3. claims draft (from claim_map.csv + verified draft)
source("R/prepare/build_claims.R")
build_claims_draft()                           # -> data/derived/claims_draft.csv

# 4. (directionality is enforced at source, in 99_validate_recomputed_results.R,
#     which run_all_reproductions.R runs in step 1 -- nothing to do here)

# 5. promote draft -> master (compute_wave1 reads data/derived/claims.csv)
#    claims.csv is rebuilt from claim_map.csv, so your directionality recodes flow through here.
file.copy("data/derived/claims_draft.csv", "data/derived/claims.csv", overwrite = TRUE)
message("Promoted claims_draft.csv -> claims.csv")

# 6. Wave-1 Bayes factors + concordance tables/figures
source("scripts/01_run_analysis.R")

# 7. figures
source("scripts/02_run_figures.R")

message("00_run_all complete.")
