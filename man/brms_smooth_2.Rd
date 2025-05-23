% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{brms_smooth_2}
\alias{brms_smooth_2}
\title{brms_smooth_2}
\format{
An object of class \code{brmsfit} of length 21.
}
\usage{
brms_smooth_2
}
\description{
Model of class brmsfit
}
\examples{
\dontshow{if (FALSE) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
\dontrun{
dat <- mgcv::gamSim(1, n = 200, scale = 2)
brm(y ~ t2(x0, x1) + s(x2, by = x3), data = dat, iter = 1000)
}
\dontshow{\}) # examplesIf}
}
\keyword{datasets}
