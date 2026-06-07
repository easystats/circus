# brms_zoib_1

Model of class brmsfit

## Usage

``` r
brms_zoib_1
```

## Format

An object of class `brmsfit` of length 23.

## Examples

``` r
if (FALSE) {
# \dontrun{
df <- data.frame(
  y = cogmod::rbext(1000),
  participant = rep(paste0("S", 1:10), each = 100)
)

f <- bf(
  y ~ 1 + (1 | participant),
  phi ~ 1 + (1 | participant),
  zoi ~ 1 + (1 | participant),
  coi ~ 1 + (1 | participant)
)

brm(f, data = df, family = zero_one_inflated_beta(), init = 0,
    chains = 4, iter = 500, backend = "cmdstanr", refresh = 0)
# }
}
```
