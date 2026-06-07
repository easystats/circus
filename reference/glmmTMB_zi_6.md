# glmmTMB_zi_6

Model of class glmmTMB

## Usage

``` r
glmmTMB_zi_6
```

## Format

An object of class `glmmTMB` of length 7.

## Examples

``` r
if (FALSE) {
# \dontrun{
# data prepararion: see "glmmTMB_1"

glmmTMB(
  count ~ child + camper + (1 + xb | persons),
  ziformula = ~ child + livebait + (1 + zg + nofish | ID),
  dispformula = ~xb,
  data = fish,
  family = truncated_poisson()
)
# }
}
```
