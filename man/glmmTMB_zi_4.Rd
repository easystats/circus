% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{glmmTMB_zi_4}
\alias{glmmTMB_zi_4}
\title{glmmTMB_zi_4}
\format{
An object of class \code{glmmTMB} of length 7.
}
\usage{
glmmTMB_zi_4
}
\description{
Model of class glmmTMB
}
\examples{
\dontshow{if (FALSE) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
\dontrun{
# data prepararion: see "glmmTMB_1"

glmmTMB(
  count ~ child + camper + (1 + zg | persons) + (1 | nofish),
  ziformula = ~ child + livebait + (1 | persons),
  data = fish,
  family = truncated_poisson()
)
}
\dontshow{\}) # examplesIf}
}
\keyword{datasets}
