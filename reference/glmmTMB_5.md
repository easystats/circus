# glmmTMB_5

Model of class glmmTMB

## Usage

``` r
glmmTMB_5
```

## Format

An object of class `glmmTMB` of length 7.

## Examples

``` r
if (FALSE) {
# \dontrun{
data(cbpp, package = "lme4")
glmmTMB(
  cbind(incidence, size - incidence) ~ period + (1 | herd),
  data = cbpp,
  family = binomial
)
# }
}
```
