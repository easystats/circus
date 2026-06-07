# brms_ordinal_1

Model of class brmsfit

## Usage

``` r
brms_ordinal_1
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
brms::brm(cyl_ord ~ mpg, data = mtcars, family = cumulative())
# }
}
```
