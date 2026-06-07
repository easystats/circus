# brms_ipw_1

Model of class brmsfit

## Usage

``` r
brms_ipw_1
```

## Format

An object of class `brmsfit` of length 23.

## Examples

``` r
if (FALSE) {
# \dontrun{
brms::brm(
  QoL | weights(ipw) ~ treatment * time + treatment * education +
    hospital + age + phq4 + (1 | ID) ,
  data = datawizard::qolcancer
)
# }
}
```
