# brms_beta_1

Model of class brmsfit

## Usage

``` r
brms_beta_1
```

## Format

An object of class `brmsfit` of length 23.

## Examples

``` r
if (FALSE) {
# \dontrun{
data(FoodExpenditure, package = "betareg")
m <- brm(
  I(food / income) ~ income + (1 | persons),
  data = FoodExpenditure,
  family = Beta()
)
# }
}
```
