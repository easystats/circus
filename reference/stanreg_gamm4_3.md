# stanreg_gamm4_3

Model of class stanreg

## Usage

``` r
stanreg_gamm4_3
```

## Format

An object of class `stanreg` (inherits from `glm`, `lm`, `gamm4`,
`lmerMod`) of length 28.

## Examples

``` r
if (FALSE) {
# \dontrun{
data <- iris
data$Cat1 <- rep(c("X", "X", "Y"), length.out = nrow(data))

stan_gamm4(
  Sepal.Width ~ Species + s(Petal.Length),
  random = ~(1 | Cat1),
  data = data
)
# }
}
```
