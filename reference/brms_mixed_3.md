# brms_mixed_3

Model of class brmsfit

## Usage

``` r
brms_mixed_3
```

## Format

An object of class `brmsfit` of length 19.

## Examples

``` r
if (FALSE) {
# \dontrun{
set.seed(123)
sleepstudy$grp <- sample(1:5, size = 180, replace = TRUE)
sleepstudy <- sleepstudy %>%
  group_by(grp) %>%
  mutate(subgrp = sample(1:15, size = n(), replace = TRUE))

brms::brm(Reaction ~ Days + (1 | grp / subgrp) + (1 | Subject), data = sleepstudy)
# }
}
```
