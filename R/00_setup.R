library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(BayesFactor)

#reads the data
input_csv <- "data/templates/analysis_index_template.csv"

dir.create("outputs", showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

