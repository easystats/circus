# brms_categorical_1_fct

Model of class brmsfit

## Usage

``` r
brms_categorical_1_fct
```

## Format

An object of class `brmsfit` of length 21.

## Examples

``` r
if (FALSE) {
# \dontrun{
set.seed(123)
data(mtcars)
mtcars$gear_fct <- factor(mtcars$gear)
brms::brm(gear_fct ~ mpg, data = mtcars, family = categorical())
# }
}
```
