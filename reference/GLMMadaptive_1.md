# GLMMadaptive_1

Model of class GLMMadaptive

## Usage

``` r
GLMMadaptive_1
```

## Format

An object of class `MixMod` of length 21.

## Examples

``` r
if (FALSE) {
# \dontrun{
data(cbpp, package = "lme4")

GLMMadaptive::mixed_model(
  cbind(incidence, size - incidence) ~ period,
  random = ~ 1 | herd,
  data = cbpp,
  family = binomial
)
# }
}
```
