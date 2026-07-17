# Run the Study 37 reproduction from the repository root.
source("R/reproduce/study_37.R")
data_path   <- file.path("data", "raw", "study_37", "osf_data.csv")
output_path <- file.path("outputs", "reproduced", "study_37_recomputed.csv")
results <- reproduce_study_37(data_path)
write_study_37_outputs(results, output_path)
print(results, width = Inf)
message("Study 37 reproduction complete. Output: ", output_path)
