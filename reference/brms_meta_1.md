# brms_meta_1

Model of class brmsfit

## Usage

``` r
brms_meta_1
```

## Format

An object of class `brmsfit` of length 19.

## Examples

``` r
if (FALSE) {
# \dontrun{
# Data from
# https://github.com/MathiasHarrer/Doing-Meta-Analysis-in-R/blob/master/_data/Meta_Analysis_Data.RData
set.seed(123)
priors <- c(prior(normal(0,1), class = Intercept),
            prior(cauchy(0,0.5), class = sd))

brm(TE|se(seTE) ~ 1 + (1|Author),
   data = Meta_Analysis_Data,
   prior = priors,
   iter = 4000)
# }
}
```
