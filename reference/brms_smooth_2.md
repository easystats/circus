# brms_smooth_2

Model of class brmsfit

## Usage

``` r
brms_smooth_2
```

## Format

An object of class `brmsfit` of length 21.

## Examples

``` r
if (FALSE) {
# \dontrun{
dat <- mgcv::gamSim(1, n = 200, scale = 2)
brm(y ~ t2(x0, x1) + s(x2, by = x3), data = dat, iter = 1000)
# }
}
```
