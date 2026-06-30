# brms_chocomini_2

Model of class brmsfit

## Usage

``` r
brms_chocomini_2
```

## Format

An object of class `brmsfit` of length 23.

## Examples

``` r
if (FALSE) {
# \dontrun{
library(cogmod)
library(brms)

data <- data.frame(
  y = rchoco(
    2000,
    p = 0.5, # Named mu in brms
    confright = 0.8,
    confleft = 0.3
  ),
  cond = rep(c("A", "B"), each = 1000)
)

brm(
  bf(
    y ~ cond, # mu
    confright ~ 1,
    confleft ~ 1,
    precright ~ 1,
    precleft ~ 1,
    pex ~ 1,
    bex ~ 1,
    pmid ~ 1
  ),
  data = data,
  family = choco(),
  stanvars = choco_stanvars(),
  init = 0,
  iter = 500,
  backend = "cmdstanr"
)
# }
}
```
