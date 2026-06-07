# stanreg_merMod_4

Model of class stanreg

## Usage

``` r
stanreg_merMod_4
```

## Format

An object of class `stanreg` (inherits from `glm`, `lm`, `lmerMod`) of
length 27.

## Examples

``` r
if (FALSE) {
# \dontrun{
stan_glmer(
  vs ~ cyl + (1 | gear),
  data = mtcars,
  family = binomial(link = "probit")
)
# }
}
```
