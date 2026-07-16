# Run the Study 27 reproduction from the repository root.
source("R/reproduce/study_27.R")

data_path   <- file.path("data", "raw", "study_27", "ClassExerciseData.sav")
output_path <- file.path("outputs", "reproduced", "study_27_recomputed.csv")
audit_path  <- file.path("outputs", "reproduced", "study_27_recomputation_audit.csv")

results <- reproduce_study_27(data_path)
write_study_27_outputs(results, output_path, audit_path)

print(results, width = Inf)
message("Study 27 reproduction complete.")
message("Main output:  ", output_path)
message("Audit output: ", audit_path)
