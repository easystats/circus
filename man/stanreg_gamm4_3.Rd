% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{stanreg_gamm4_3}
\alias{stanreg_gamm4_3}
\title{stanreg_gamm4_3}
\format{
An object of class \code{stanreg} (inherits from \code{glm}, \code{lm}, \code{gamm4}, \code{lmerMod}) of length 28.
}
\usage{
stanreg_gamm4_3
}
\description{
Model of class stanreg
}
\examples{
\dontshow{if (FALSE) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
\dontrun{
data <- iris
data$Cat1 <- rep(c("X", "X", "Y"), length.out = nrow(data))

stan_gamm4(
  Sepal.Width ~ Species + s(Petal.Length),
  random = ~(1 | Cat1),
  data = data
)
}
\dontshow{\}) # examplesIf}
}
\keyword{datasets}
