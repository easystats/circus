# brms_smooth_1

Model of class brmsfit

## Usage

``` r
brms_smooth_1
```

## Format

An object of class `brmsfit` of length 19.

## Examples

``` r
if (FALSE) {
# \dontrun{
dat <- mgcv::gamSim(1, n = 200, scale = 2)
brm(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, iter = 1000)
# }
}
```
