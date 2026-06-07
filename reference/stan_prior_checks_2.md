# stan_prior_checks_2

Model of class stanreg

## Usage

``` r
stan_prior_checks_2
```

## Format

An object of class `stanreg` (inherits from `glm`, `lm`) of length 28.

## Examples

``` r
if (FALSE) {
# \dontrun{
library(rstanarm)
# weakly informative prior
stan_glm(
  fall_incidence ~ stay + age + mmse + sex,
  data = d,
  family = binomial("logit"),
  refresh = 0,
  cores = 4,
  seed = 1207,
  # only sample from the prior predictive distribution
  prior_PD = TRUE,
  open_progress = FALSE
)
# }
}
```
