# glmmTMB_4

Model of class glmmTMB

## Usage

``` r
glmmTMB_4
```

## Format

An object of class `glmmTMB` of length 7.

## Examples

``` r
if (FALSE) {
# \dontrun{
glmmTMB(
  count ~ spp + mined + (1 | site),
  ziformula =  ~ spp + mined,
  family = truncated_poisson,
  data = Salamanders
)
# }
}
```
