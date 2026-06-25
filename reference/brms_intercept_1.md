# brms_intercept_1

Model of class brmsfit

## Usage

``` r
brms_intercept_1
```

## Format

An object of class `brmsfit` of length 23.

## Examples

``` r
if (FALSE) {
# \dontrun{
set.seed(123)
df <- data.frame(response = rnorm(100), day = rep(paste0('D', 1:5), each = 20))
brm(
  response ~ 1 + (1 | day),
  data = df,
  seed = 123
)
# }
}
```
