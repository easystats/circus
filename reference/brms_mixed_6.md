# brms_mixed_6

Model of class brmsfit

## Usage

``` r
brms_mixed_6
```

## Format

An object of class `brmsfit` of length 20.

## Examples

``` r
if (FALSE) {
# \dontrun{
bprior1 <- prior(student_t(5, 0, 10), class = b) + prior(cauchy(0, 2), class = sd)
brms::brm(
  count ~ Age + Base * Trt + (1 | patient),
  data = epilepsy,
  family = poisson(),
  prior = bprior1,
  chains = 1,
  iter = 500
)
# }
}
```
