# stanreg_merMod_5

Model of class stanreg

## Usage

``` r
stanreg_merMod_5
```

## Format

An object of class `stanreg` (inherits from `glm`, `lm`, `lmerMod`) of
length 27.

## Examples

``` r
if (FALSE) {
# \dontrun{
stan_glmer(
  cbind(incidence, size - incidence) ~ size + period + (1 | herd),
  data = lme4::cbpp, family = binomial, QR = TRUE,
  chains = 2, cores = 1, seed = 12345, iter = 500, refresh = 0
)
# }
}
```
