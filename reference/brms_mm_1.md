# brms_mm_1

Model of class brmsfit

## Usage

``` r
brms_mm_1
```

## Format

An object of class `brmsfit` of length 19.

## Examples

``` r
if (FALSE) {
# \dontrun{
set.seed(123)
dat <- data.frame(
  sid = c(1:300),
  t1id = sample(200:215, 300, replace = TRUE),
  t2id = sample(200:215, 300, replace = TRUE),
  y = rnorm(300, mean = 85, sd = 8),
  x = rnorm(300, mean = 50, sd = 10)
)

brm(y ~ x + (1 | mm(t1id, t2id)), data = dat)
# }
}
```
