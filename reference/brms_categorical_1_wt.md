# brms_categorical_1_wt

Model of class brmsfit

## Usage

``` r
brms_categorical_1_wt
```

## Format

An object of class `brmsfit` of length 16.

## Examples

``` r
if (FALSE) {
# \dontrun{
set.seed(123)
data(mtcars)
brms::brm(gear | weights(wt) ~ mpg, data = mtcars, family = categorical())
# }
}
```
