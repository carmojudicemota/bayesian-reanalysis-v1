read_analysis_index <- function(path = input_csv) {
  readr::read_csv( path, show_col_types = FALSE, na = c("", "NA")
  ) 
}

#
prepare_analysis_index <- function(dat) {
  dat |> 
    dplyr::mutate(
      #divide into test family identifiers
      test_family = dplyr::case_when(
        stat_test %in% c("one_sample_t_test", "paired_t_test",
                         "independent_t_test", 
                         "welch_independent_t_test") ~ t_test,
        stat_test %in% c("one_way_anova","factorial_between_anova",
        "repeated_measures_anova","mixed_anova","welch_anova") ~"anova",
        stat_test == "manova" ~"manova",
        stat_test %in% c("pearson_correlation", 
                         "spearman_correlation" ) ~ "correlation",
        stat_test %in% c( "wilcoxon_signed_rank", "friedman_test"
        ) ~ "nonparametric",
        stat_test == "chi_square_difference" ~ "chi_square",
        stat_test %in% c( "wald_z", "sem_contrast", 
                          "logistic_regression",
                          "mixed_effects_logistic_regression" ) ~ "regression_or_sem",
        TRUE ~ "other"
      ),
      #specific things within a family
      is_one_sample_or_paired_t = stat_test %in% c(
        "one_sample_t_test", "paired_t_test"
      ),
      is_independent_t = stat_test %in% 
        c("independent_t_test", "welch_independent_t_test"),
      is_repeated_measures = stat_test %in% 
        c("repeated_measures_anova", "mixed_anova"),
      
      #UNDONE: add some sanity checks of the table
      z_from_estimate = dplyr::if_else(
        !is.na(estimate) & !is.na(se_estimate) & se_estimate != 0, 
        estimate / se_estimate,
        NA_real_ ),
      z_working = dplyr::coalesce(z_value, z_from_estimate),
      t_from_r = dplyr::if_else(
        !is.na(r_value) & !is.na(n_total) & abs(r_value) < 1,
        r_value * sqrt((n_total - 2) / (1 - r_value^2)), NA_real_
      ),
      r_df_working = 
        dplyr::if_else(!is.na(r_value) & !is.na(n_total) & abs(r_value)<1,
                       r_value *sqrt((n_total-2)/(1-r_value^2)),
                       NA_real_),
      n_eff_working = dplyr::coalesce(
        n_eff, 
        dplyr::if_else(test_family == t_test & is_one_sample_t &
                         !is.na(t_df), t_df +1, NA_real_),
        dplyr::if_else(test_family == "anova" & is_repeated_measures &
                         f_df1 == 1 & !is.na(f_df2), f_df2 + 1, NA_real_),
        n_total
        ),
      #do z_for_normal_bf
      #do can computes: frequentist, normal prior, jzs, 
    )
}




