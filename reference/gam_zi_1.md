# gam_zi_1

Model of class gam

## Usage

``` r
gam_zi_1
```

## Format

An object of class `gam` (inherits from `glm`, `lm`) of length 53.

## Examples

``` r
if (FALSE) {
library(mgcv)
f0 <- function(x) 2 * sin(pi * x)
f1 <- function(x) exp(2 * x)
f2 <- function(x) 0.2 * x ^ 11 * (10 * (1 - x)) ^ 6 + 10 * (10 * x) ^ 3 * (1 - x) ^ 10
n <- 500
set.seed(5)
x0 <- runif(n)
x1 <- runif(n)
x2 <- runif(n)
x3 <- runif(n)

eta1 <- f0(x0) + f1(x1) - 3
p <- binomial()$linkinv(eta1)
y <- as.numeric(runif(n) < p) ## 1 for presence, 0 for absence

ind <- y > 0
eta2 <- f2(x2[ind]) / 3
y[ind] <- rpois(exp(eta2), exp(eta2))

gam(list(y ~ s(x2) + s(x3),  ~ s(x0) + s(x1)), family = ziplss())
}
```
