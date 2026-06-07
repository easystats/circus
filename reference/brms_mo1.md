# brms_mo1

Model of class brmsfit

## Usage

``` r
brms_mo1
```

## Format

An object of class `brmsfit` of length 23.

## Examples

``` r
if (FALSE) {
# \dontrun{
income_options <- c("below_20", "20_to_40", "40_to_100", "greater_100")
income <- factor(
  sample(income_options, 100, TRUE),
  levels = income_options, ordered = TRUE
)
mean_ls <- c(30, 60, 70, 75)
ls <- mean_ls[income] + rnorm(100, sd = 7)
dat <- data.frame(income, ls) # fit a simple monotonic model
brms_mo1 <- brm(ls ~ mo(income), data = dat)
# }
}
```
