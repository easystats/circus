% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{brms_mo1}
\alias{brms_mo1}
\title{brms_mo1}
\format{
An object of class \code{brmsfit} of length 23.
}
\usage{
brms_mo1
}
\description{
Model of class brmsfit
}
\examples{
\dontshow{if (FALSE) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
\dontrun{
income_options <- c("below_20", "20_to_40", "40_to_100", "greater_100")
income <- factor(
  sample(income_options, 100, TRUE),
  levels = income_options, ordered = TRUE
)
mean_ls <- c(30, 60, 70, 75)
ls <- mean_ls[income] + rnorm(100, sd = 7)
dat <- data.frame(income, ls) # fit a simple monotonic model
brms_mo1 <- brm(ls ~ mo(income), data = dat)
}
\dontshow{\}) # examplesIf}
}
\keyword{datasets}
