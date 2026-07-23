library(haven); library(readxl); library(readr); library(dplyr); library(tidyr)

load_wave2_data <- function(study_id, outcome_col = NULL) {
  switch(study_id,
         #FAMILY A
         study_06 = {
           dat <- haven::read_sav("data/raw/study_06/Social_Annotation_and_SOB_SOC_Data.sav")
           tibble(outcome = as.numeric(dat$BCBSTotalScore),
                  A = factor(dat$Condition), B = factor(dat$Course)) |> na.omit()
         },
         study_10 = {                                   
           dat <- readr::read_csv("data/raw/study_10/master.anonymizedOSF.csv", show_col_types = FALSE)
           d   <- dat[dat$university %in% c(1, 3), ]    
           tibble(outcome = as.numeric(d[[outcome_col]]),
                  A = factor(d$replicate),              
                  B = factor(d$university)) |> na.omit()
         },
         
         study_29 = {                                   
           dat  <- readr::read_csv("data/raw/study_29/Morling and Lee Faculty Sample Open Data.csv", show_col_types = FALSE)
           keep <- dat$Title %in% c(1, 2) & dat$Department %in% c(1, 2) & !is.na(dat$univkids)
           d    <- dat[keep, ]
           tibble(outcome = as.numeric(d$univkids),
                  A = factor(d$Title),                  
                  B = factor(d$Department))
         },
         
         study_60 = {                                  
           dat <- as.data.frame(haven::read_sav("data/raw/study_60/Syllabus_Safety_Cues_Instructor_Gender_Data_Sp21_OSF.sav"))
           y <- if (identical(outcome_col, "include_1")) {
             as.numeric(dat$include_1)
           } else {                                      
             raw <- reconstruct_field_belonging_raw(dat)
             winsorize_to_nearest_observed(raw, z = 3)$values
           }
           tibble(outcome = y,
                  A = factor(dat$IV_safetycues, levels = c(0, 1)),   
                  B = factor(dat$IV_profgender, levels = c(0, 1))) |> na.omit()
         },
         
         #FAMILY B
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
         study_47 = {                                    
           d <- as.data.frame(haven::read_sav("data/raw/study_47/Outside_Assistance_Dataset.sav"))
           d <- d[, c("Condition", "PercentRember", "PercentCorrectMC", "PercentCorrectOE")]
           d <- d[stats::complete.cases(d), ]
           d$subject   <- factor(seq_len(nrow(d)))
           d$Condition <- factor(as.numeric(d$Condition))            
           tidyr::pivot_longer(d, c(PercentRember, PercentCorrectMC, PercentCorrectOE),
                               names_to = "condition", values_to = "score") |>
             dplyr::mutate(condition = factor(condition))
         },
         study_43 = {   
           data <- haven::read_sav("data/raw/study_43/Datafile.sav")
           d <- data |>
             dplyr::select(Crit_Score_Testing_old, Crit_Score_Testing_New,
                           Crit_Score_Restudy_old, Crit_Score_Restudy_New,
                           Lecture, questiontype_crit) |>
             na.omit() |>
             dplyr::mutate(
               Lecture = factor(Lecture), questiontype_crit = factor(questiontype_crit),
               reviewed     = Crit_Score_Testing_old - Crit_Score_Restudy_old,        
               questiontype = ((Crit_Score_Testing_old + Crit_Score_Restudy_old) / 2) -
                 ((Crit_Score_Testing_New + Crit_Score_Restudy_New) / 2)) 
           d$contrast <- d[[outcome_col]]             
           d[, c("contrast", "Lecture", "questiontype_crit")]
         },
         
         #FAMILY C
         
         study_13 = {
           dat <- haven::read_sav(
             "data/raw/study_13/DATA_Cleaned_and_coded_for_condition.sav"
           )
           if (
             is.null(outcome_col) ||
             !outcome_col %in% names(dat)
           ) {
             stop(
               "A valid outcome_col is required for study_13. ",
               "Use 'subj_total' or 'obj_total'.",
               call. = FALSE
             )
           }
           outcome <- as.numeric(dat[[outcome_col]])
           condition <- as.numeric(dat$condition)
           keep <- complete.cases(outcome, condition)
           outcome <- outcome[keep]
           condition <- condition[keep]
           list(
             x1 = outcome[condition == 1],
             x2 = outcome[condition == 0]
           )
         },
         
         #ELSE
         stop("No Wave 2 data loader for ", study_id, call. = FALSE))
}