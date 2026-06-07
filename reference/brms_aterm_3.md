# brms_aterm_3

Model of class brmsfit

## Usage

``` r
brms_aterm_3
```

## Format

An object of class `brmsfit` of length 22.

## Examples

``` r
if (FALSE) {
# \dontrun{
data(kidney)
brm(time | cens(censored) ~ age * sex + disease + (1|patient),
    data = kidney, family = lognormal(),
    chains = 2, iter = 200,
    backend = "cmdstanr", cores = 2)# }
}
```
