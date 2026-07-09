
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(BayesFactor)

analysis_index <- readr::read_csv("data/templates/analysis_index_template.csv",
                                  show_col_types = FALSE)

analysis_index

ttest_rows <- analysis_index |>
  dplyr::filter(
    test_type %in% c(
      "t_test", "one_sample_t_test", "paired_t_test"
    )
)

ttest_rows

compute_frequentist_t_checks <- function(dat) {
  dat |> 
    dplyr::mutate(
      t_abs = abs(t_value),
      p_two_sided_reconstructed = 2 * stats::pt(
        q = -t_abs,
        df = df
      ),
      p_one_sided_reconstructed = stats::pt(
        q = -t_abs,
        df = df
      ),
      p_reconstructed_used = dplyr::case_when(
        p_sidedness == "two_sided" ~p_two_sided_reconstructed,
        p_sidedness == "one_sided" ~ p_one_sided_reconstructed,
        TRUE ~ p_two_sided_reconstructed
      ),
      p_difference = p_value - p_reconstructed_used,
      p_check = dplyr::case_when(
        abs(p_difference) <= 0.001 ~ "okay",
        TRUE ~ "not okay"
      )
    )
}

frequentist_checks <- compute_frequentist_t_checks(ttest_rows)

frequentist_checks |>
  dplyr::select(
    study_id,
    test_type,
    df,
    p_value,
    p_difference,
    p_check
    
  )
