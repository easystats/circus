# brms_ordinal_2

Model of class brmsfit

## Usage

``` r
brms_ordinal_2
```

## Format

An object of class `brmsfit` of length 19.

## Examples

``` r
if (FALSE) {
# \dontrun{
data(inhaler)
brm(
  rating ~ treat + period + carry + (1 | subject),
  family = cumulative(),
  data = inhaler
)
# }
}
```
