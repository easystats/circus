% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{stanreg_gamm4_1}
\alias{stanreg_gamm4_1}
\title{stanreg_gamm4_1}
\format{
An object of class \code{stanreg} (inherits from \code{glm}, \code{lm}, \code{gamm4}) of length 27.
}
\usage{
stanreg_gamm4_1
}
\description{
Model of class stanreg
}
\examples{
\dontshow{if (FALSE) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
\dontrun{
stan_gamm4(Sepal.Width ~ s(Petal.Length), data = iris)
}
\dontshow{\}) # examplesIf}
}
\keyword{datasets}
