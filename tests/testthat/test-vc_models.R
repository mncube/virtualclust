test_that("convert_to_long works",{
  set.seed(123)
  simulated_data <- sim_tc_cv(n_treatment = 100,
                              n_control = 1000, n_matches = 10)
  combined_long <- convert_to_long(simulated_data$combined_data)
})

test_that("vc_models functions work", {
  set.seed(123)
  simulated_data <- sim_tc_cv(n_treatment = 100,
                              n_control = 1000, n_matches = 10)

  mixed_model_pair_res <- mixed_model_pair(simulated_data)
  expect_s3_class(mixed_model_pair_res, "tbl_df")

  mixed_model_shared_group_res <- mixed_model_shared_group(simulated_data)
  expect_s3_class(mixed_model_shared_group_res, "tbl_df")

  mixed_model_treatment_only_res <- mixed_model_treatment_only(simulated_data)
  expect_s3_class(mixed_model_treatment_only_res, "tbl_df")

  paired_t_test_res <- paired_t_test(simulated_data)
  expect_s3_class(paired_t_test_res, "tbl_df")

  two_step_t_test_res <- two_step_t_test(simulated_data)
  expect_s3_class(two_step_t_test_res, "tbl_df")

})
