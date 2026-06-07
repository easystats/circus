# brms_mixed_5

Model of class brmsfit

## Usage

``` r
brms_mixed_5
```

## Format

An object of class `brmsfit` of length 21.

## Examples

``` r
if (FALSE) {
# \dontrun{
library(lme4)
library(brms)
data(sleepstudy)
set.seed(123)
sleepstudy$cat <- as.factor(sample(1:5, size = 180, replace = TRUE))
sleepstudy$Reaction_d <-
  ifelse(sleepstudy$Reaction < median(sleepstudy$Reaction), 0, 1)

brms::brm(
  Reaction_d ~ Days + cat + (1 | Subject),
  data = sleepstudy,
  family = bernoulli()
)
# }
}
```
