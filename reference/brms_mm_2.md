# brms_mm_2

Model of class brmsfit

## Usage

``` r
brms_mm_2
```

## Format

An object of class `brmsfit` of length 19.

## Examples

``` r
if (FALSE) {
# \dontrun{
set.seed(123)
dat <- data.frame(
  y = rnorm(100), x1 = rnorm(100), x2 = rnorm(100),
  g1 = sample(1:10, 100, TRUE), g2 = sample(1:10, 100, TRUE)
)

# multi-membership model with level specific covariate values
dat$xc <- (dat$x1 + dat$x2) / 2
brm(y ~ xc + (1 + mmc(x1, x2) | mm(g1, g2)), data = dat)
# }
}
```
