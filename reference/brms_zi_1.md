# brms_zi_1

Model of class brmsfit

## Usage

``` r
brms_zi_1
```

## Format

An object of class `brmsfit` of length 19.

## Examples

``` r
if (FALSE) {
# \dontrun{
zinb <- read.csv("http://stats.idre.ucla.edu/stat/data/fish.csv")
set.seed(123)
brms::brm(bf(
    count ~ persons + child + camper,
    zi ~ child + camper
  ),
  data = zinb,
  family = zero_inflated_poisson()
)
# }
}
```
