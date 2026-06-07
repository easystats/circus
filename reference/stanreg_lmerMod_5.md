# stanreg_lmerMod_5

Model of class stanreg

## Usage

``` r
stanreg_lmerMod_5
```

## Format

An object of class `stanreg` (inherits from `glm`, `lm`, `lmerMod`) of
length 27.

## Examples

``` r
if (FALSE) {
# \dontrun{
dat <- iris
dat$Cat1 <- rep(c("X", "X", "Y"), length.out = nrow(dat))
dat$Cat2 <- rep(c("A", "B"), length.out = nrow(dat))
rstanarm::stan_lmer(Petal.Width ~ Cat1 + (1 + Cat1 | Species), data = dat)
# }
}
```
