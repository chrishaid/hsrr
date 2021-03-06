% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/hsr_likert.r
\name{plot_likert}
\alias{plot_likert}
\title{Plot five point Likert Scale data from HSR survey}
\usage{
plot_likert(.data, topic = "Instructional Leadership", ...)
}
\arguments{
\item{.data}{`data.frame` of Healthy Schools and Regions data in long format}

\item{topic}{character vector of parent topics to filter form `.data`}

\item{...}{arguments passed to \code{\link{pre_process_likert}}}
}
\description{
`plot_likert` generates a floating bar graph (horizontally) aligned
for survey data that is on lickert scale. Currently this function supports a a five
point Likert scale the with levels "Strongly Disagree", "Disagree", "Neutral",
"Agree", and "Strongly Agree"
}
\examples{
data(ex_hsr)

plot_likert(ex_hsr,
            topic="Instructional Leadership",
            school_order=c("KHS1",
                           "KMS1",
                           "KMS2",
                           "KHS2",
                           "KR",
                           "KN"))
}

