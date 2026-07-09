
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
        !is.na(z_working) $ p_sidedness == "one_sided" ~
          stats::pnorm(q = -abs(z_working)),
        #chi2
        !is.na(chi2_value) & !is.na(chi2_df) ~
          stats::pchisq(q = chi2_value, df = chi2_df, lower.tail = FALSE),
        #correlations
        !is.na(t_from_r) & !is.na(r_df_working) ~
          2* stats::pt(q = -abs(t_from_r), df = r_df_working),
        TRUE ~ NA_real_
        
      ),
      p_difference = p_value - p_reconstructed,
      p_check = dplyr::case_when(
        is.na(p_value) | is.na(p_reconstructed) ~ "not_checkable",
        p_operator == "<" & p_reconstructed <= p_value ~
          "consistent_with_reported_threshold",
        p_operator == "=" & abs(p_difference) < 0.001 ~ 
          "approximately_consistent",
        p_operator == "=" & abs(p_difference) < 0.01 ~
          "consistent_with_rounding",
        TRUE ~ "check_manually"
      )
    )
}








