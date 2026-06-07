# brms_zi_3

Model of class brmsfit

## Usage

``` r
brms_zi_3
```

## Format

An object of class `brmsfit` of length 19.

## Examples

``` r
if (FALSE) {
# \dontrun{
brms::brm(
  bf(count ~ child + camper + (1 | persons), zi ~ child + camper + (1 | persons)),
  data = zinb,
  family = zero_inflated_poisson(),
  chains = 1,
  iter = 500
)
# }
}
```
