# brms_corr_re1

Model of class brmsfit

## Usage

``` r
brms_corr_re1
```

## Format

An object of class `brmsfit` of length 19.

## Examples

``` r
if (FALSE) {
# \dontrun{
df <- data.frame(x = factor(rep(c('A', 'B'), each = 100)),
                 y = c(rnorm(100, mean = 0, sd = 1),
                       rnorm(100, mean = 10, sd = 5)),
                 id = factor(rep(1:100, 2)))

brm(
  bf(y ~ x + (1 | i | id), sigma ~ x + (1 | i | id)),
  data = df,
  iter = 500,
  cores = 4
)
# }
}
```
