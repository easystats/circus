# stanreg_glm_5

Model of class stanreg

## Usage

``` r
stanreg_glm_5
```

## Format

An object of class `stanreg` (inherits from `glm`, `lm`) of length 27.

## Examples

``` r
if (FALSE) {
# \dontrun{
set.seed(123)
mtcars$count <- rpois(nrow(mtcars), 2)
rstanarm::stan_glm(count ~ wt + cyl, data = mtcars, family = "poisson")
# }
}
```
