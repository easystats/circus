# anova_2

Model of class anova

## Usage

``` r
anova_2
```

## Format

An object of class `anova` (inherits from `data.frame`) with 8 rows and
5 columns.

## Examples

``` r
if (FALSE) {
data <- iris
data$Cat1 <- rep(c("X", "X", "Y"), length.out = nrow(data))
data$Cat2 <- rep(c("A", "B"), length.out = nrow(data))

anova(lm(Sepal.Length ~ Species * Cat1 * Cat2, data = data))
}
```
