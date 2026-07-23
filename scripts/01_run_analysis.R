source("R/analyse/compute_wave1_bayes_factors.R")
source("R/analyse/compute_wave2_bayes_factors.R")
source("R/analyse/combine_bayes_factor_results.R")
source("R/analyse/dataset.R")
source("R/analyse/classify_concordance.R")
compute_wave1_bayes_factors()      # -> outputs/tables/bayes_factor_results_wave1.csv
compute_wave2_bayes_factors()      # -> outputs/tables/bayes_factor_results_wave2.csv (only if ready Wave 2 claims)
combine_bayes_factor_results()     # -> outputs/tables/bayes_factor_results.csv
build_dataset_outputs()
build_concordance_outputs()
message("Analysis done")
