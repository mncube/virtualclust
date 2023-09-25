#' Simulate Treatment and Control Student Data with Virtual Control
#'
#' This function simulates data for a treatment student group and a control student group,
#' performs a many-to-one matching algorithm, and summarizes the matched control students
#' to form a virtual control for each treatment student.
#'
#' @param n_treatment Integer. The number of treatment students.
#' @param n_control Integer. The number of control students.
#' @param n_matches Integer. The number of control students to match to each treatment student. Default is 5.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{treatment_data}: A data frame of the treatment student data.
#'   \item \code{control_data}: A data frame of the control student data.
#'   \item \code{combined_data}: A merged data frame of the treatment data with the virtual control.
#' }
#' @examples
#' set.seed(123)
#' sim_data <- sim_tc_cv(n_treatment = 100, n_control = 1000, n_matches = 10)
#' head(sim_data$combined_data)
#' @export
sim_tc_cv <- function(n_treatment, n_control, n_matches = 5) {
  # Simulate treatment student data
  treatment_data <- data.frame(
    student_id = 1:n_treatment,
    pre_score = stats::rnorm(n_treatment, mean = 50, sd = 10),
    post_score = stats::rnorm(n_treatment, mean = 52, sd = 10),  # Assuming a slight improvement
    teacher = sample(1:5, n_treatment, replace = TRUE)
  )

  # Simulate control student data
  control_data <- data.frame(
    student_id = (n_treatment + 1):(n_treatment + n_control),
    score = stats::rnorm(n_control, mean = 51, sd = 10)  # No pre/post distinction for controls
  )

  # Many-to-one matching algorithm
  matched_data <- data.frame()
  for (i in 1:n_treatment) {
    matched_rows <- control_data[sample(1:n_control, n_matches, replace = FALSE), ]
    matched_rows$treatment_student_id <- treatment_data$student_id[i]
    matched_data <- rbind(matched_data, matched_rows)
  }

  # Summarizing matched data for virtual control
  virtual_control <- matched_data |>
    dplyr::group_by(treatment_student_id) |>
    dplyr::summarise(virtual_score = mean(score))

  # Merging with treatment data
  combined_data <- merge(treatment_data, virtual_control, by.x="student_id", by.y="treatment_student_id")

  return(list(treatment_data = treatment_data, control_data = control_data, combined_data = combined_data))
}

#' Simulate Treatment and Control Student Data with Intraclass Correlation
#'
#' This function simulates clustered data for a treatment student group and a control student group,
#' performs propensity score matching based on pre-test scores, and summarizes the matched control students
#' to form a virtual control for each treatment student. Clustering in the treatment group is based on teachers,
#' and the level of clustering is controlled by the Intraclass Correlation Coefficient (ICC).
#'
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
#' @return A list containing:
#' \itemize{
#'   \item \code{treatment_data}: A data frame of the treatment student data.
#'   \item \code{control_data}: A data frame of the control student data.
#'   \item \code{combined_data}: A merged data frame of the treatment data with the virtual control.
#' }
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#' simulated_data <- sim_tc_cv_icc(n_treatment = 100, n_control = 1000, n_matches = 10)
#' head(simulated_data$combined_data)
#' }
#' @export
sim_tc_cv_icc <- function(n_treatment, n_control, n_matches = 5,
                          treat_pre_mean = 50, treat_pre_sd = 10,
                          treat_post_mean = 52, treat_post_sd = 10,
                          control_pre_mean = 51, control_pre_sd = 10,
                          control_post_mean = 51, control_post_sd = 10,
                          n_teachers = 5, icc = 0.05) {

  # Calculate between and within variance for the treatment group based on ICC
  between_variance <- treat_pre_sd^2 * icc
  within_variance <- treat_pre_sd^2 - between_variance

  # Simulate teacher random effects
  teacher_effects <- stats::rnorm(n_teachers, mean = 0, sd = sqrt(between_variance))

  # Simulate treatment student data
  treatment_data <- data.frame(
    student_id = 1:n_treatment,
    teacher = sample(1:n_teachers, n_treatment, replace = TRUE),
    treatment = 1
  )

  treatment_data$pre_score <- with(treatment_data, stats::rnorm(n_treatment, mean = treat_pre_mean + teacher_effects[teacher], sd = sqrt(within_variance)))
  treatment_data$post_score <- with(treatment_data, stats::rnorm(n_treatment, mean = treat_post_mean + teacher_effects[teacher], sd = sqrt(within_variance)))

  # Simulate control student data
  control_data <- data.frame(
    student_id = (n_treatment + 1):(n_treatment + n_control),
    teacher = NA,  # Add dummy teacher column
    pre_score = stats::rnorm(n_control, mean = control_pre_mean, sd = control_pre_sd),
    post_score = stats::rnorm(n_control, mean = control_post_mean, sd = control_post_sd),
    treatment = 0
  )


  # Combine treatment and control data for matching
  combined_data_for_matching <- rbind(treatment_data, control_data)

  # Propensity score matching based on pre-test scores
  m.out <- MatchIt::matchit(treatment ~ pre_score, data = combined_data_for_matching, method = "nearest", ratio = n_matches)
  matched_data <- MatchIt::match.data(m.out)

  # Separate the matched data back into treatment and control
  treatment_matched <- matched_data[matched_data$treatment == 1, ]
  control_matched <- matched_data[matched_data$treatment == 0, ]

  # Calculate virtual post-test scores for controls
  virtual_control <- control_matched |>
    dplyr::group_by(subclass) |>
    dplyr::summarise(virtual_score = mean(post_score))

  # Merging with treatment data
  combined_data <- merge(treatment_matched, virtual_control, by.x="subclass", by.y="subclass")

  return(list(treatment_data = treatment_data, control_data = control_data, combined_data = combined_data))
}
