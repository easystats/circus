# lmerMod_5

Model of class lmerMod

## Usage

``` r
lmerMod_5
```

## Format

An object of class `lmerMod` of length 1.

## Examples

``` r
if (FALSE) {
data <- iris
data$Cat1 <- rep(c("X", "X", "Y"), length.out = nrow(data))
data$Cat2 <- rep(c("A", "B"), length.out = nrow(data))
lme4::lmer(Petal.Width ~ Cat1 + (1 + Cat1 | Species), data = data)
}
```
