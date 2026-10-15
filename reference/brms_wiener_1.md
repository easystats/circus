# brms_wiener_1

Model of class brmsfit

## Usage

``` r
brms_wiener_1
```

## Format

An object of class `brmsfit` of length 23.

## Examples

``` r
if (FALSE) {
# \dontrun{
options(wiener_backend = "rtdists") # for posterior_predict(); RWiener works too
set.seed(1)
sim <- rbind(
  rtdists::rdiffusion(150, a = 1.5, v = 0.5, t0 = 0.3, z = 0.75),
  rtdists::rdiffusion(150, a = 1.5, v = 1.5, t0 = 0.3, z = 0.75)
)
d <- data.frame(
  x = rep(c("a", "b"), each = 150),
  rt = sim$rt,
  resp = as.integer(sim$response == "upper")
)
brm(
  bf(rt | dec(resp) ~ x),
  data = d,
  family = wiener()
)
# }
}
```
