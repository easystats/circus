# brms_slopes_1

Model of class brmsfit

## Usage

``` r
brms_slopes_1
```

## Format

An object of class `brmsfit` of length 23.

## Examples

``` r
if (FALSE) {
# \dontrun{
states <- as.data.frame(state.x77)
states$HSGrad <- states$`HS Grad`
brm(
  Income ~ HSGrad + Murder * Illiteracy,
  data = states,
  chains = 2,
  iter = 100,
  cores = 2,
  backend = 'cmdstanr'
)
# }
}
```
