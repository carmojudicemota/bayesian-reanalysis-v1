testthat::test_that("Study 60 reproduces the published retained results", {
  testthat::skip_if_not_installed("haven")
  testthat::skip_if_not_installed("car")
  source("R/reproduce_study_60.R")

  sav_path <- file.path(
    "data", "raw", "study_60",
    "Syllabus_Safety_Cues_Instructor_Gender_Data_Sp21_OSF.sav"
  )
  testthat::skip_if_not(file.exists(sav_path), "Study 60 raw data are not installed.")

  out <- reproduce_study_60(sav_path)
  field <- out$results[out$results$id == 44, ]
  inclusive <- out$results[out$results$id == 45, ]

  testthat::expect_equal(field$n_total, 326)
  testthat::expect_equal(field$f_df1, 1)
  testthat::expect_equal(field$f_df2, 322)
  testthat::expect_equal(field$f_value, 5.16911976585038, tolerance = 1e-10)
  testthat::expect_equal(field$p_value, 0.0236499529367053, tolerance = 1e-12)
  testthat::expect_equal(field$effect_size_value, 0.0157995344106737, tolerance = 1e-12)

  testthat::expect_equal(inclusive$n_total, 326)
  testthat::expect_equal(inclusive$f_df1, 1)
  testthat::expect_equal(inclusive$f_df2, 322)
  testthat::expect_equal(inclusive$f_value, 42.9518507980829, tolerance = 1e-9)
  testthat::expect_equal(inclusive$p_value, 2.22660053111215e-10, tolerance = 1e-20)
  testthat::expect_equal(inclusive$effect_size_value, 0.117691828947175, tolerance = 1e-12)
})

testthat::test_that("Study 60 explains the field-belonging screenshot discrepancy", {
  testthat::skip_if_not_installed("haven")
  testthat::skip_if_not_installed("car")
  source("R/reproduce_study_60.R")

  sav_path <- file.path(
    "data", "raw", "study_60",
    "Syllabus_Safety_Cues_Instructor_Gender_Data_Sp21_OSF.sav"
  )
  testthat::skip_if_not(file.exists(sav_path), "Study 60 raw data are not installed.")

  out <- reproduce_study_60(sav_path)
  raw_row <- out$audit[out$audit$analysis == "field_unwinsorized_spss_screenshot", ]

  testthat::expect_equal(raw_row$f_value, 5.32057364822764, tolerance = 1e-10)
  testthat::expect_equal(raw_row$p_value, 0.0217095029988215, tolerance = 1e-12)
  testthat::expect_equal(raw_row$mean_control, 3.47034764826176, tolerance = 1e-12)
  testthat::expect_equal(raw_row$mean_safety, 3.59372869734151, tolerance = 1e-12)
})
