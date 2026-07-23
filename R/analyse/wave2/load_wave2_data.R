library(haven); library(readxl); library(readr); library(dplyr); library(tidyr)

load_wave2_data <- function(study_id) {
  switch(study_id,
         
         study_06 = {
           dat <- haven::read_sav("data/raw/study_06/Social_Annotation_and_SOB_SOC_Data.sav")
           tibble(outcome = as.numeric(dat$BCBSTotalScore),
                  A = factor(dat$Condition), B = factor(dat$Course)) |> na.omit()
         },
         
         study_35 = {
           raw <- haven::read_sav("data/raw/study_35/Untitled3.sav", user_na = FALSE)
           wide <- data.frame(
             subject = factor(as.character(raw[["Participant"]])),
             Sex     = factor(as.character(raw[["Sex"]])),
             RR      = as.numeric(raw[["CORRECT_ANSWERS_RR"]])     * 100 / 7,
             MMRP    = as.numeric(raw[["CORRECT_ANSWERS_MMRP_A"]]) * 100 / 7,
             RP      = as.numeric(raw[["CORRECT_ANSWERS_RP_A"]])   * 100 / 7)
           wide <- na.omit(wide)
           pivot_longer(wide, c(RR, MMRP, RP), names_to = "condition", values_to = "score") |>
             mutate(condition = factor(condition))
         },
         
         study_13 = {
           dat <- haven::read_sav("data/raw/study_13/DATA_Cleaned_and_coded_for_condition.sav")
           cond <- as.numeric(dat$condition)
           list(x1 = dat[["subj_total"]][cond == 1], x2 = dat[["subj_total"]][cond == 0])
         },
         
         stop("No Wave 2 data loader for ", study_id, call. = FALSE))
}