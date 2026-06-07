# brms_mv_4

Model of class brmsfit

## Usage

``` r
brms_mv_4
```

## Format

An object of class `brmsfit` of length 19.

## Examples

``` r
if (FALSE) {
# \dontrun{
f1 <- bf(Sepal.Length ~ Petal.Length + Sepal.Width + Species)
f2 <- bf(Sepal.Width ~ Species)
brms::brm(f1 + f2 + set_rescor(FALSE), data = iris, chains = 1, iter = 500)
# }
}
```
