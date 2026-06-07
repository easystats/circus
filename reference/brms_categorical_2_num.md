# brms_categorical_2_num

Model of class brmsfit

## Usage

``` r
brms_categorical_2_num
```

## Format

An object of class `brmsfit` of length 23.

## Examples

``` r
if (FALSE) {
# \dontrun{
set.seed(123)
brm(
  Species ~ Sepal.Width,
  data = iris,
  family = categorical(refcat = "setosa"),
  refresh = 0,
  backend = "cmdstanr",
  algorithm = "pathfinder"
)
# }
}
```
