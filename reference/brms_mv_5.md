# brms_mv_5

Model of class brmsfit

## Usage

``` r
brms_mv_5
```

## Format

An object of class `brmsfit` of length 21.

## Examples

``` r
if (FALSE) {
# \dontrun{
bf1 <- bf(count ~ child + camper + (1 | persons), zi ~ camper + (1 | persons))
bf2 <- bf(count2 ~ child + livebait + (1 | persons), zi ~ child + (1 | persons))
brms::brm(bf1 + bf2, data = zinb, family = zero_inflated_poisson(), chains = 1, iter = 500)
# }
}
```
