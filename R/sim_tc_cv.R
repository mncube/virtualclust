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
