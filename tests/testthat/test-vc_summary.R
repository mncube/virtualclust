test_that("vc_summary works", {
  set.seed(123)
  simulated_data <- sim_tc_cv(n_treatment = 100,
                              n_control = 1000, n_matches = 10)

  vc_summary_res <- vc_summary(simulated_data)
  expect_s3_class(vc_summary_res, "tbl_df")
})
