% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vc_models.R
\name{convert_to_long}
\alias{convert_to_long}
\title{Convert Data to Long Format}
\usage{
convert_to_long(data)
}
\arguments{
\item{data}{A data frame with post_score and virtual_score columns.}
}
\value{
A data frame in long format.
}
\description{
Internal function to convert the data to a long format using pivot_longer from the tidyr package.
}
\keyword{internal}
