
frequentist_checks <- function(dat) {
  dat |>
    dplyr::mutate(
      p_reconstructed = dplyr::case_when(
        # for the t-tests
        !is.na(t_value) & !is.na(t_df) & p_sidedness == "one_sided" ~
          stats::pt(q= -abs(t_value), df = t_df),
        !is.na(t_value) & !is.na(t_df)~
          2* stats::pt(q= -abs(t_value), df = t_df),
        # for the F tests + ANOVA + MANOVA
        !is.na(f_value) & !is.na(f_df1) & !is.na(f_df2) ~
          stats::pf(q=f_value, df1=f_df1, df2=f_df2, 
                    lower.tail = FALSE),
        # z and Wald
        
      )
    )
}