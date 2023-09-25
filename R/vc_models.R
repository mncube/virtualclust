#' Convert Data to Long Format
#'
#' Internal function to convert the data to a long format using pivot_longer from the tidyr package.
#'
#' @param data A data frame with post_score and virtual_score columns.
#'
#' @return A data frame in long format.
#'
#' @keywords internal
convert_to_long <- function(data) {
  long_data <- data |>
    tidyr::pivot_longer(cols = c("post_score", "virtual_score"),
                        names_to = "group",
                        values_to = "post_score") |>
    dplyr::mutate(group = ifelse(group == "post_score", "treatment", "virtual")) |>
    dplyr::mutate(group = factor(group, levels = c("virtual", "treatment")))
  return(long_data)
}

#' Mixed Effect Model with Pair Identifier
#'
#' Internal function that fits a mixed-effect model using a pair identifier.
#'
#' @param data A list containing data from the \code{sim_tc_cv} function.
#'
#' @return A tibble with model results.
#'
#' @keywords internal
mixed_model_pair <- function(data) {
  long_data <- convert_to_long(data$combined_data)
  model <- lmerTest::lmer(post_score ~ group + (1|student_id), data = long_data)
  result <- broom.mixed::tidy(model)
  result$conf.low <- stats::confint(model, level = 0.95)[, 1]
  result$conf.high <- stats::confint(model, level = 0.95)[, 2]
  return(result)
}

#' Mixed Effect Model with Shared Group Identifier
#'
#' Internal function that fits a mixed-effect model using a shared group identifier.
#'
#' @param data A list containing data from the \code{sim_tc_cv} function.
#'
#' @return A tibble with model results.
#'
#' @keywords internal
mixed_model_shared_group <- function(data) {
  long_data <- convert_to_long(data$combined_data)
  model <- lmerTest::lmer(post_score ~ group + (1|teacher), data = long_data)
  result <- broom.mixed::tidy(model)
  result$conf.low <- stats::confint(model, level = 0.95)[, 1]
  result$conf.high <- stats::confint(model, level = 0.95)[, 2]
  return(result)
}

#' Mixed Effect Model with Treatment-only Group Identifier
#'
#' Internal function that fits a mixed-effect model using a treatment-only group identifier.
#'
#' @param data A list containing data from the \code{sim_tc_cv} function.
#'
#' @return A tibble with model results.
#'
#' @keywords internal
mixed_model_treatment_only <- function(data) {
  long_data <- convert_to_long(data$combined_data) |>
    dplyr::mutate(teacher = ifelse(group == "virtual", 100, teacher))

  model <- lmerTest::lmer(post_score ~ group + (1|teacher), data = long_data)
  result <- broom.mixed::tidy(model)
  result$conf.low <- stats::confint(model, level = 0.95)[, 1]
  result$conf.high <- stats::confint(model, level = 0.95)[, 2]
  return(result)
}

#' Paired T-test Comparison
#'
#' Internal function that conducts a paired t-test comparison.
#'
#' @param data A list containing data from the \code{sim_tc_cv} function.
#'
#' @return A tibble with t-test results.
#'
#' @keywords internal
paired_t_test <- function(data) {
  t_result <- stats::t.test(data$combined_data$post_score, data$combined_data$virtual_score, paired = TRUE)
  return(broom::tidy(t_result))  # Keeping this as broom::tidy since it's not a mixed model
}

#' Two-Step Adjusted Paired t-test
#'
#' Internal function that conducts a two-step adjusted paired t-test.
#'
#' @param data A list containing data from the \code{sim_tc_cv} function.
#'
#' @return A tibble with t-test results.
#'
#' @keywords internal
two_step_t_test <- function(data) {
  # Fit the model to the treatment group only
  model <- lmerTest::lmer(post_score ~ 1 + (1|teacher), data = data$combined_data)

  # Predict/adjust the scores for the treatment group
  data$combined_data$adjusted_score <- stats::predict(model, newdata = data$combined_data)

  # Paired t-test between adjusted scores of treatment group and virtual scores
  t_result <- stats::t.test(data$combined_data$adjusted_score, data$combined_data$virtual_score, paired = TRUE)

  return(broom::tidy(t_result))
}
