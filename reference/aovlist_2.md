# aovlist_2

Model of class aovlist

## Usage

``` r
aovlist_2
```

## Format

An object of class `aovlist` (inherits from `listof`) of length 3.

## Examples

``` r
if (FALSE) {
data <- iris
data$Cat1 <- rep(c("X", "X", "Y"), length.out = nrow(data))
data$Cat2 <- rep(c("A", "B"), length.out = nrow(data))

aov(Sepal.Length ~ Species * Cat1 + Error(Cat2), data = data)
}
```
