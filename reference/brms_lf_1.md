# brms_lf_1

Model of class brmsfit

## Usage

``` r
brms_lf_1
```

## Format

An object of class `brmsfit` of length 23.

## Examples

``` r
if (FALSE) {
# \dontrun{
brms::brm(
  bf(carb ~ gear * vs) + lf(disc ~ 0 + mo(cyl)),
  data = mtcars,
  family = cumulative("probit")
)
# }
}
```
