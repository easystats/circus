# brms_sigma_3

Model of class brmsfit

## Usage

``` r
brms_sigma_3
```

## Format

An object of class `brmsfit` of length 23.

## Examples

``` r
if (FALSE) {
# \dontrun{
set.seed(123)
data(iris)
iris$Group <- as.factor(rep(c("G1", "G2", "G3"), each = 50))
brms_sigma_3 <- brms::brm(
  brms::bf(
    Sepal.Width ~ Petal.Width + (Petal.Width | Group),
    sigma ~ Petal.Width + (Petal.Width | Group)
  ),
  data = iris,
  refresh=0
)
# }
}
```
