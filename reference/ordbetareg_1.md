# ordbetareg_1

Model of class brmsfit

## Usage

``` r
ordbetareg_1
```

## Format

An object of class `brmsfit` (inherits from `ordbetareg`) of length 25.

## Examples

``` r
if (FALSE) {
# \dontrun{
library(ordbetareg)
data(sleepstudy, package = "lme4")
sleepstudy$y <- datawizard::normalize(sleepstudy$Reaction)
m <- ordbetareg(y ~ Days + (Days | Subject), data = sleepstudy)
# }
}
```
