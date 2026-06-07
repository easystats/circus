# brms_mm_3

Model of class brmsfit

## Usage

``` r
brms_mm_3
```

## Format

An object of class `brmsfit` of length 23.

## Examples

``` r
if (FALSE) {
# \dontrun{
set.seed(123)
data(mzcars)
mtcars$w <- 1
brm(
  mpg ~ 1 + hp + (1 | cyl) +
    (1 | carb:am:hp) +
    (1 | mm(carb, am, weights = cbind(w, w), scale = FALSE)) +
    (1 | mm(carb:cyl, am:cyl, weights = cbind(w, w), scale = FALSE)),
  data = mtcars, family = gaussian, chains = 4,
  iter = 100, control = list(adapt_delta = 0.99, max_treedepth = 15),
  cores = 4, seed = 12345,
  backend = "cmdstanr"
)
# }
}
```
