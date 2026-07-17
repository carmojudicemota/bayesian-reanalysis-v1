# Run the Study 41 reproduction from the repository root.
source("R/reproduce/study_41.R")

data_path   <- file.path("data", "raw", "study_41", "BART_Content_Knowledge_Deidentified.xlsx")
output_path <- file.path("outputs", "reproduced", "study_41_recomputed.csv")
audit_path  <- file.path("outputs", "reproduced", "study_41_recomputation_audit.csv")

results <- reproduce_study_41(data_path)
write_study_41_outputs(results, output_path, audit_path)

print(results, width = Inf)
message("Study 41 reproduction complete.")
message("Main output: ", output_path)
message("Audit output: ", audit_path)
