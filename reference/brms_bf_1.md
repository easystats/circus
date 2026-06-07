# brms_bf_1

Model of class brmsfit

## Usage

``` r
brms_bf_1
```

## Format

An object of class `brmsfit` of length 22.

## Examples

``` r
if (FALSE) {
# \dontrun{
brm(
  mpg ~ wt,
  data = mtcars,
  prior = set_prior("normal(0, 1)", class = "b"),
  refresh = 0,
  iter = 200,
  chains = 2
)
# }
}
```
