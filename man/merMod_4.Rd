% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{merMod_4}
\alias{merMod_4}
\title{merMod_4}
\format{
An object of class \code{glmerMod} of length 1.
}
\usage{
merMod_4
}
\description{
Model of class merMod
}
\examples{
\dontshow{if (FALSE) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
lme4::glmer(vs ~ cyl + (1 | gear), data = mtcars, family = binomial(link = "probit"))
\dontshow{\}) # examplesIf}
}
\keyword{datasets}
