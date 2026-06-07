# brms_3

Model of class brmsfit

## Usage

``` r
brms_3
```

## Format

An object of class `brmsfit` of length 19.

## Examples

``` r
if (FALSE) {
# \dontrun{
set.seed(123)
x <- sample(c(1:3), 1000, replace = TRUE)
w <- runif(1000, min = 0.3, max = 3)
y <- rnorm(1000, mean = 0, sd = 1)
data <- data.frame(x, w, y)
data$x <- factor(data$x)

brm(x | weights(w) ~ y, data = data, family = categorical)# }
}
```
