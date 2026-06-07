# brms_von_mises_1

Model of class brmsfit

## Usage

``` r
brms_von_mises_1
```

## Format

An object of class `brmsfit` of length 23.

## Examples

``` r
if (FALSE) {
# \dontrun{
df <- data.frame(
  y = runif(200, -pi, pi),
  x = as.factor(rep(c("a", "b"), 100))
)

f <- brms::bf(
  y ~ 0 + Intercept + x,
  kappa ~ 0 + Intercept,
  family = brms::von_mises()
)

brms::brm(
  f,
  data = df,
  refresh = 0,
  algorithm = "pathfinder",
  backend = "cmdstanr"
)
# }
}
```
