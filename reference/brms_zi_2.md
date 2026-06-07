# brms_zi_2

Model of class brmsfit

## Usage

``` r
brms_zi_2
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
    count ~ persons + child + camper + (1 | persons),
    zi ~ child + camper + (1 | persons)
  ),
  data = zinb,
  family = zero_inflated_poisson()
)
# }
}
```
