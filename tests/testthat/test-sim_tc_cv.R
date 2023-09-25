test_that("sim_tc_cv function works as expected", {
  set.seed(123)
  simulated_data <- sim_tc_cv(n_treatment = 100,
                              n_control = 1000, n_matches = 10)

  expect_equal(length(simulated_data), 3)
  expect_true(is.data.frame(simulated_data$combined_data))
  expect_equal(nrow(simulated_data$combined_data), 100)
  expect_equal(ncol(simulated_data$combined_data), 5)
})
