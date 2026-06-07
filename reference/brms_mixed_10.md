# brms_mixed_10

Model of class brmsfit

## Usage

``` r
brms_mixed_10
```

## Format

An object of class `brmsfit` of length 23.

## Examples

``` r
if (FALSE) {
# \dontrun{
data(iris)
iris$Group <- as.factor(rep(c("G1", "G2", "G3"), each = 50))
set.seed(123)
brms::brm(
  Sepal.Width ~ Petal.Width + (Petal.Width | Group),
  data = iris,
  refresh = 0
)
# }
}
```
