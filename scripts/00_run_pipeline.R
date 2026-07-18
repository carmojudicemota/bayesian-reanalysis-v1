# scripts/run_pipeline.R
# Re-run the analysis pipeline end to end from the repo root.
# Safe to run either way:
#   - terminal:  Rscript scripts/run_pipeline.R
#   - RStudio:   source("scripts/run_pipeline.R")   # will NOT terminate your session
# Requires the reproduction CSVs to already exist in outputs/reproduced/
# (run scripts/run_all_reproductions.R first if you changed any study_*.R).

# --- 0. always run from the repo root -------------------------------------
stopifnot(file.exists("R/reproduce/98_compile_recomputed_results.R"))

# FALSE = build the draft and STOP so you can diff before overwriting the
#         curated master (nothing is promoted, session stays alive).
# TRUE  = also promote claims_draft.csv -> claims.csv and run steps 6-7.
PROMOTE <- FALSE

# --- 1. compile the per-study recomputed CSVs -----------------------------
source("R/reproduce/98_compile_recomputed_results.R")   # writes all_recomputed_results_current_studies.csv

# --- load pipeline functions ----------------------------------------------
source("R/prepare/build_verified_results.R")
source("R/prepare/build_claims.R")                       # sources read_ali_sources.R itself
source("R/prepare/resolve_claim_directionality.R")

# --- 2. verified results draft --------------------------------------------
build_verified_results_draft()                           # -> data/derived/verified_results_draft.csv

# --- 3. claims draft (from claim_map.csv + verified draft) -----------------
build_claims_draft()                                     # -> data/derived/claims_draft.csv

# --- 4. one-sided -> two-sided p where policy requires --------------------
resolve_claim_directionality()                           # -> data/derived/claims_draft.csv (in place)

# --- 5. promotion + downstream (only when PROMOTE = TRUE) ------------------
if (!PROMOTE) {
  message(
    "\nDraft built. PROMOTE = FALSE, so nothing was overwritten.\n",
    "Review the diff, then flip PROMOTE <- TRUE and re-run:\n",
    "  In a terminal:  diff data/derived/claims.csv data/derived/claims_draft.csv\n",
    "Stopped before Bayes factors (compute_wave1 reads claims.csv)."
  )
} else {
  file.copy("data/derived/claims_draft.csv",
            "data/derived/claims.csv", overwrite = TRUE)
  message("Promoted claims_draft.csv -> claims.csv")

  # --- 6. Wave-1 Bayes factors (reads claims.csv) -------------------------
  source("R/analyse/compute_wave1_bayes_factors.R")      # loads library(BayesFactor)
  compute_wave1_bayes_factors()                          # -> outputs/tables/bayes_factor_results.csv

  # --- 7. concordance tables + figures ------------------------------------
  source("R/analyse/classify_concordance.R")
  build_concordance_outputs()                            # -> concordance_*.csv + figures
  message("Pipeline complete.")
}
