% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{brms_mv_1}
\alias{brms_mv_1}
\title{brms_mv_1}
\format{
An object of class \code{brmsfit} of length 19.
}
\usage{
brms_mv_1
}
\description{
Model of class brmsfit
}
\examples{
\dontrun{
set.seed(123)
brms::brm(cbind(cyl, gear, carb) ~ wt + hp, data = mtcars)
}
}
\keyword{datasets}
