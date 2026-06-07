# glm_5

Model of class glm

## Usage

``` r
glm_5
```

## Format

An object of class `glm` (inherits from `lm`) of length 30.

## Examples

``` r
if (FALSE) {
set.seed(123)
mtcars$count <- rpois(nrow(mtcars), 2)
glm(formula = count ~ wt + cyl, family = "poisson", data = mtcars)
}
```
