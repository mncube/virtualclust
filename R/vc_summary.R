#' Summarize Results from Various Modeling Approaches
#'
#' This function takes the simulated data and summarizes results from various modeling
#' approaches, including mixed models and t-tests.
#'
#' @param data A list containing data from the \code{sim_tc_cv} function.
#' @param remove_re Logical. If TRUE, removes the random effects (e.g., "sd__(Intercept)") from the summary. Default is TRUE.
#' @param remove_int Logical. If TRUE, removes the intercept term from the summary. Default is TRUE.
#'
#' @return A tibble summarizing results from the different modeling approaches.
#'
#' @examples
#' set.seed(123)
#' simulated_data <- sim_tc_cv(n_treatment = 100, n_control = 1000, n_matches = 10)
#' summary_results <- vc_summary(simulated_data)
#' head(summary_results)
#' @export
vc_summary <- function(data, remove_re = TRUE, remove_int = TRUE) {
  # Obtain results from each modeling approach
  pair_res <- mixed_model_pair(data)
  shared_group_res <- mixed_model_shared_group(data)
  treatment_only_res <- mixed_model_treatment_only(data)
  t_test_res <- paired_t_test(data)
  two_step_res <- two_step_t_test(data)

  # Organize results into a summary dataframe
  summary_df <- dplyr::bind_rows(
    dplyr::select(pair_res, term, estimate, std.error, statistic, p.value, conf.low, conf.high) |>
      dplyr::mutate(model = "Mixed Model (Pair)"),
    dplyr::select(shared_group_res, term, estimate, std.error, statistic, p.value, conf.low, conf.high) |>
      dplyr::mutate(model = "Mixed Model (Shared Group)"),
    dplyr::select(treatment_only_res, term, estimate, std.error, statistic, p.value, conf.low, conf.high) |>
      dplyr::mutate(model = "Mixed Model (Treatment Only)"),
    dplyr::select(t_test_res, estimate, p.value, conf.low, conf.high) |>
      dplyr::mutate(model = "Paired t-test", term = "difference", std.error = NA, statistic = NA),
    dplyr::select(two_step_res, estimate, p.value, conf.low, conf.high) |>
      dplyr::mutate(model = "Two-Step Adjusted Paired t-test", term = "difference", std.error = NA, statistic = NA)
  )

  # Apply filtering based on the parameters
  if (remove_re) {
    summary_df <- dplyr::filter(summary_df, !stringr::str_starts(term, "sd_"))
  }

  if (remove_int) {
    summary_df <- dplyr::filter(summary_df, term != "(Intercept)")
  }

  return(summary_df)
}
