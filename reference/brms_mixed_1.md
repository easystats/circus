# brms_mixed_1

Model of class brmsfit

## Usage

``` r
brms_mixed_1
```

## Format

An object of class `brmsfit` of length 19.

## Examples

``` r
if (FALSE) {
# \dontrun{
set.seed(123)
brms::brm(mpg ~ wt + (1 | cyl) + (1 + wt | gear), data = mtcars)
# }
}
```
