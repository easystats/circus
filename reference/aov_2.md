# aov_2

Model of class aov

## Usage

``` r
aov_2
```

## Format

An object of class `aov` (inherits from `lm`) of length 13.

## Examples

``` r
if (FALSE) {
data <- iris
data$Cat1 <- rep(c("X", "X", "Y"), length.out = nrow(data))
data$Cat2 <- rep(c("A", "B"), length.out = nrow(data))

aov(Sepal.Length ~ Species * Cat1 * Cat2, data = data)
}
```
