# stan_prior_checks_3

Model of class stanreg

## Usage

``` r
stan_prior_checks_3
```

## Format

An object of class `stanreg` (inherits from `glm`, `lm`) of length 28.

## Examples

``` r
if (FALSE) {
# \dontrun{
library(rstanarm)
prob_fall <- qlogis(0.05)
prob_fall_scale <- 0.5
prior_intercept <- student_t(
  df = 5,
  location = prob_fall,
  scale = prob_fall_scale
)
# skeptical prior
prob_dem_mid <- -log(2)
prob_dem_hi <- -log(3.5)

stan_glm(
  fall_incidence ~ stay + age + mmse + sex,
  data = d,
  family = binomial("logit"),
  prior = student_t(
    df = 5,
    location = c(0, 0, prob_dem_mid, prob_dem_hi, 0, 0, 0, 0, 0, 0, 0),
    scale = c(1, 1, 0.5, 0.5, 1, 1, 1, 1, 1, 1, 1)
  ),
  prior_intercept = prior_intercept,
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
