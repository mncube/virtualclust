#' Simulate and Analyze Results from Virtual Clustering
#'
#' This function runs a simulation based on the parameters provided,
#' generating data using \code{sim_tc_cv_icc} and summarizing the
#' results using \code{vc_summary}. Key simulation metrics are then
#' computed based on the combined summarized results.
#'
#' @param n_iterations In the simulation
#' @param n_treatment Integer. The number of treatment students.
#' @param n_control Integer. The number of control students.
#' @param n_matches Integer. The number of control students to match to each treatment student. Default is 5.
#' @param treat_pre_mean Numeric. The mean of pre-test scores for the treatment group. Default is 50.
#' @param treat_pre_sd Numeric. The standard deviation of pre-test scores for the treatment group. Default is 10.
#' @param treat_post_mean Numeric. The mean of post-test scores for the treatment group. Default is 52.
#' @param treat_post_sd Numeric. The standard deviation of post-test scores for the treatment group. Default is 10.
#' @param control_pre_mean Numeric. The mean of pre-test scores for the control group. Default is 51.
#' @param control_pre_sd Numeric. The standard deviation of pre-test scores for the control group. Default is 10.
#' @param control_post_mean Numeric. The mean of post-test scores for the control group. Default is 51.
#' @param control_post_sd Numeric. The standard deviation of post-test scores for the control group. Default is 10.
#' @param n_teachers Integer. The number of unique teachers for the treatment group. Default is 5.
#' @param icc Numeric. The Intraclass Correlation Coefficient to control the level of clustering in the treatment group. Default is 0.05.
#'
#'
#' @return A list containing summarized results and key simulation metrics.
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' sim_results <- virtualclust_simulation(100, n_treatment = 100, n_control = 1000, n_matches = 10)
#' print(sim_results$metrics)
#' }
#' @export
virtualclust_simulation <- function(n_iterations, n_treatment, n_control, n_matches = 5,
                                    treat_pre_mean = 50, treat_pre_sd = 10,
                                    treat_post_mean = 52, treat_post_sd = 10,
                                    control_pre_mean = 51, control_pre_sd = 10,
                                    control_post_mean = 51, control_post_sd = 10,
                                    n_teachers = 5, icc = 0.05) {

  true_effect <- treat_post_mean - control_post_mean

  all_results <- lapply(1:n_iterations, function(iter) {
    sim_data <- sim_tc_cv_icc(n_treatment, n_control, n_matches, treat_pre_mean, treat_pre_sd,
                              treat_post_mean, treat_post_sd, control_pre_mean, control_pre_sd,
                              control_post_mean, control_post_sd, n_teachers, icc)
    summary_res <- vc_summary(sim_data)
    return(summary_res)
  })

  combined_results <- dplyr::bind_rows(all_results)

  # Compute simulation metrics for each model type
  model_types <- unique(combined_results$model)

  metrics_list <- lapply(model_types, function(model_name) {
    model_data <- dplyr::filter(combined_results, model == model_name)
    metrics <- data.frame(
      model = model_name,
      rmse = sqrt(mean((model_data$estimate - true_effect)^2)),
      absolute_bias = mean(abs(model_data$estimate - true_effect)),
      ci_coverage = mean(model_data$conf.low <= true_effect & model_data$conf.high >= true_effect)
    )
    return(metrics)
  })

  metrics_df <- dplyr::bind_rows(metrics_list)

  return(list(summarized_results = combined_results, metrics = metrics_df))
}
