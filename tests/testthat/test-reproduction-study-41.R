testthat::test_that("Study 41 target results reproduce the article table", {
  testthat::skip_if_not_installed("readxl")
  testthat::skip_if_not_installed("readr")
  testthat::skip_if_not_installed("dplyr")
  testthat::skip_if_not_installed("tibble")

  source(file.path("R", "reproduce_study_41.R"))

  path <- file.path(
    "data",
    "raw",
    "study_41",
    "BART_Content_Knowledge_Deidentified.xlsx"
  )

  testthat::skip_if_not(file.exists(path), "Study 41 raw workbook is unavailable.")

  out <- reproduce_study_41(path)

  testthat::expect_equal(nrow(out), 2L)

  analyze <- out[out$analysis_id == "study_41_result_1", ]
  interpret <- out[out$analysis_id == "study_41_result_2", ]

  testthat::expect_equal(analyze$n_total, 37L)
  testthat::expect_equal(analyze$n_complete, 30L)
  testthat::expect_equal(analyze$df, 29)
  testthat::expect_equal(analyze$t_value, 8.532340234397516, tolerance = 1e-10)
  testthat::expect_equal(analyze$effect_size_value, 1.557785071562812, tolerance = 1e-10)
  testthat::expect_true(analyze$matches_reported_t)
  testthat::expect_true(analyze$matches_reported_d)

  testthat::expect_equal(interpret$n_total, 37L)
  testthat::expect_equal(interpret$n_complete, 29L)
  testthat::expect_equal(interpret$df, 28)
  testthat::expect_equal(interpret$t_value, 6.151246852292454, tolerance = 1e-10)
  testthat::expect_equal(interpret$effect_size_value, 1.142257864446973, tolerance = 1e-10)
  testthat::expect_true(interpret$matches_reported_t)
  testthat::expect_true(interpret$matches_reported_d)
})

testthat::test_that("Study 41 uses pairwise deletion and POST minus PRE", {
  testthat::skip_if_not_installed("readxl")
  testthat::skip_if_not_installed("readr")
  testthat::skip_if_not_installed("dplyr")
  testthat::skip_if_not_installed("tibble")

  source(file.path("R", "reproduce_study_41.R"))

  path <- file.path(
    "data",
    "raw",
    "study_41",
    "BART_Content_Knowledge_Deidentified.xlsx"
  )

  testthat::skip_if_not(file.exists(path), "Study 41 raw workbook is unavailable.")

  out <- reproduce_study_41(path)

  testthat::expect_true(all(out$subtraction_direction == "post_minus_pre"))
  testthat::expect_true(all(out$t_value > 0))
  testthat::expect_true(all(out$effect_size_value > 0))
  testthat::expect_equal(out$n_missing_pair, c(7L, 8L))
})
