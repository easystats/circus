# brms_sigma_1

Model of class brmsfit

## Usage

``` r
brms_sigma_1
```

## Format

An object of class `brmsfit` of length 23.

## Examples

``` r
if (FALSE) {
# \dontrun{
set.seed(123)
brm(bf(mpg ~ hp + (1 | cyl), sigma ~ cyl), data = mtcars, seed = 123)
# }
}
```
