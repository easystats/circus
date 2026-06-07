# gam_mv_1

Model of class gam

## Usage

``` r
gam_mv_1
```

## Format

An object of class `gam` (inherits from `glm`, `lm`) of length 53.

## Examples

``` r
if (FALSE) {
library(mgcv)
V <- matrix(c(2, 1, 1, 2), 2, 2)
f0 <- function(x) 2 * sin(pi * x)
f1 <- function(x) exp(2 * x)
f2 <- function(x) 0.2 * x ^ 11 * (10 * (1 - x)) ^ 6 + 10 * (10 * x) ^ 3 * (1 - x) ^ 10

n <- 300
x0 <- runif(n)
x1 <- runif(n)
x2 <- runif(n)
x3 <- runif(n)
y <- matrix(0, n, 2)

for (i in 1:n) {
  mu <- c(f0(x0[i]) + f1(x1[i]), f2(x2[i]))
  y[i, ] <- rmvn(1, mu, V)
}

dat <- data.frame(y0 = y[, 1], y1 = y[, 2], x0 = x0, x1 = x1, x2 = x2, x3 = x3)

gam(list(y0 ~ s(x0) + s(x1), y1 ~ s(x2) + s(x3)), family = mvn(d = 2), data = dat)
}
```
