# Run the Study 29 reproduction from the repository root.
source("R/reproduce/study_29.R")

data_path   <- file.path("data", "raw", "study_29", "Morling and Lee Faculty Sample Open Data.csv")
output_path <- file.path("outputs", "reproduced", "study_29_recomputed.csv")

results <- reproduce_study_29(data_path)
write_study_29_outputs(results, output_path)

print(results, width = Inf)
message("Study 29 reproduction complete. Output: ", output_path)
