# brms_trunc_1

Model of class brmsfit

## Usage

``` r
brms_trunc_1
```

## Format

An object of class `brmsfit` of length 22.

## Examples

``` r
if (FALSE) {
# \dontrun{
data(epilepsy)
brms_trunc_1 <- brm(
  count | trunc(ub = 104) ~ zBase * Trt,
  data = epilepsy,
  family = poisson(),
  backend = "cmdstanr",
  cores = 4
)# }
}
```
