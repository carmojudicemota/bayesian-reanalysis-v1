source("scripts/run_all_reproductions.R")
source("R/prepare/build_claims.R"); 
build_claims() 
source("R/prepare/resolve_claim_directionality.R"); 
resolve_claim_directionality() 
source("scripts/01_run_analysis.R")
source("scripts/02_run_figures.R")

