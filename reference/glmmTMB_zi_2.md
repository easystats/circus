# glmmTMB_zi_2

Model of class glmmTMB

## Usage

``` r
glmmTMB_zi_2
```

## Format

An object of class `glmmTMB` of length 7.

## Examples

``` r
if (FALSE) {
# \dontrun{
# data prepararion: see "glmmTMB_1"

glmmTMB(
  count ~ child + camper + (1 | persons),
  ziformula = ~ child + livebait + (1 | persons),
  data = fish,
  family = poisson()
)
# }
}
```
