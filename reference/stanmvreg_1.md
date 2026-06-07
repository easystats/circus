# stanmvreg_1

Model of class stanmvreg

## Usage

``` r
stanmvreg_1
```

## Format

An object of class `stanmvreg` (inherits from `stanreg`, `lmerMod`) of
length 20.

## Examples

``` r
if (FALSE) {
# \dontrun{
library(rstanarm)
stan_mvmer(
  formula = list(
    logBili ~ year + (1 | id),
    albumin ~ sex + year + (year | id)
  ),
  data = pbcLong,
  # this next line is only to keep the example small in size!
  chains = 1, cores = 1, seed = 12345, iter = 1000
)
# }
}
```
