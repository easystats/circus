# brms_ordinal_1_wt

Model of class brmsfit

## Usage

``` r
brms_ordinal_1_wt
```

## Format

An object of class `brmsfit` of length 19.

## Examples

``` r
if (FALSE) {
# \dontrun{
set.seed(123)
data(mtcars)
mtcars$cyl_ord <- as.ordered(mtcars$cyl)
brms::brm(cyl_ord | weights(wt) ~ mpg, data = mtcars, family = cumulative())
# }
}
```
