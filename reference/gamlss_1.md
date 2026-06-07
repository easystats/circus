# gamlss_1

Model of class gamlss

## Usage

``` r
gamlss_1
```

## Format

An object of class `gamlss` (inherits from `gam`, `glm`, `lm`) of length
88.

## Examples

``` r
if (FALSE) {
data(abdom)
gamlss(
  y ~ pb(x), sigma.formula =  ~ pb(x),
  family = BCT, data = abdom, method = mixed(1, 20)
)
}
```
