# brms_mm_4

Model of class brmsfit

## Usage

``` r
brms_mm_4
```

## Format

An object of class `brmsfit` of length 23.

## Examples

``` r
if (FALSE) {
# \dontrun{
set.seed(123)
dat <- data.frame(
  s1 = sample(200:215, 300, replace = TRUE),
  s2 = sample(200:215, 300, replace = TRUE),
  y = rnorm(300, mean = 85, sd = 8),
  x = rnorm(300, mean = 50, sd = 10)
)

brm(y ~ x + (1 | mm(s1, s2)), data = dat)
# }
}
```
