% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/virtualclust_simulation.R
\name{virtualclust_simulation}
\alias{virtualclust_simulation}
\title{Simulate and Analyze Results from Virtual Clustering}
\usage{
virtualclust_simulation(
  n_iterations,
  n_treatment,
  n_control,
  n_matches = 5,
  treat_pre_mean = 50,
  treat_pre_sd = 10,
  treat_post_mean = 52,
  treat_post_sd = 10,
  control_pre_mean = 51,
  control_pre_sd = 10,
  control_post_mean = 51,
  control_post_sd = 10,
  n_teachers = 5,
  icc = 0.05
)
}
\arguments{
\item{n_iterations}{In the simulation}

\item{n_treatment}{Integer. The number of treatment students.}

\item{n_control}{Integer. The number of control students.}

\item{n_matches}{Integer. The number of control students to match to each treatment student. Default is 5.}

\item{treat_pre_mean}{Numeric. The mean of pre-test scores for the treatment group. Default is 50.}

\item{treat_pre_sd}{Numeric. The standard deviation of pre-test scores for the treatment group. Default is 10.}

\item{treat_post_mean}{Numeric. The mean of post-test scores for the treatment group. Default is 52.}

\item{treat_post_sd}{Numeric. The standard deviation of post-test scores for the treatment group. Default is 10.}

\item{control_pre_mean}{Numeric. The mean of pre-test scores for the control group. Default is 51.}

\item{control_pre_sd}{Numeric. The standard deviation of pre-test scores for the control group. Default is 10.}

\item{control_post_mean}{Numeric. The mean of post-test scores for the control group. Default is 51.}

\item{control_post_sd}{Numeric. The standard deviation of post-test scores for the control group. Default is 10.}

\item{n_teachers}{Integer. The number of unique teachers for the treatment group. Default is 5.}

\item{icc}{Numeric. The Intraclass Correlation Coefficient to control the level of clustering in the treatment group. Default is 0.05.}
}
\value{
A list containing summarized results and key simulation metrics.
}
\description{
This function runs a simulation based on the parameters provided,
generating data using \code{sim_tc_cv_icc} and summarizing the
results using \code{vc_summary}. Key simulation metrics are then
computed based on the combined summarized results.
}
\examples{
\dontrun{
set.seed(123)
sim_results <- virtualclust_simulation(100, n_treatment = 100, n_control = 1000, n_matches = 10)
print(sim_results$metrics)
}
}
