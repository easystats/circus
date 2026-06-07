# brms_mv_2

Model of class brmsfit

## Usage

``` r
brms_mv_2
```

## Format

An object of class `brmsfit` of length 19.

## Examples

``` r
if (FALSE) {
# \dontrun{
set.seed(123)
f1 <- bf(mpg ~ wt + disp + cyl + hp + (1 |CAR| gear))
f2 <- bf(wt ~ disp + cyl + hp + (1 |CAR| gear))
brms::brm(f1 + f2 + set_rescor(FALSE), data = mtcars)
# }
}
```
