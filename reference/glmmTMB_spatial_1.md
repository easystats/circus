# glmmTMB_spatial_1

Model of class glmmTMB

## Usage

``` r
glmmTMB_spatial_1
```

## Format

An object of class `glmmTMB` of length 7.

## Examples

``` r
if (FALSE) {
# \dontrun{
library(geoR)
data(ca20)
dat <- data.frame(
  x = ca20$coords[, 1],
  y = ca20$coords[, 2],
  calcium = ca20$data,
  elevation = ca20$covariate[, 1],
  region = factor(ca20$covariate[, 2])
)
dat$pos <- numFactor(scale(dat$x), scale(dat$y))
dat$ID <- factor(rep(1, nrow(dat)))

glmmTMB(calcium ~ elevation + region + mat(pos + 0 | ID), data = dat)
# }
}
```
