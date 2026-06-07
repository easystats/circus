# brms_zi_4

Model of class brmsfit

## Usage

``` r
brms_zi_4
```

## Format

An object of class `brmsfit` of length 23.

## Examples

``` r
if (FALSE) {
# \dontrun{
brms::brm(
  bf(
    count ~ child + camper + (1 + xb | persons) + (1 + zg | ID),
    zi ~ child + livebait + (1 + zg + nofish | ID)
  ),
  data = zinb,
  family = zero_inflated_poisson(),
  chains = 1,
  iter = 500
)
# }
}
```
