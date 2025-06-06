% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{brms_categorical_1_fct}
\alias{brms_categorical_1_fct}
\title{brms_categorical_1_fct}
\format{
An object of class \code{brmsfit} of length 21.
}
\usage{
brms_categorical_1_fct
}
\description{
Model of class brmsfit
}
\examples{
\dontshow{if (FALSE) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
\dontrun{
set.seed(123)
data(mtcars)
mtcars$gear_fct <- factor(mtcars$gear)
brms::brm(gear_fct ~ mpg, data = mtcars, family = categorical())
}
\dontshow{\}) # examplesIf}
}
\keyword{datasets}
