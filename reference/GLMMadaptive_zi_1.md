# GLMMadaptive_zi_1

Model of class GLMMadaptive

## Usage

``` r
GLMMadaptive_zi_1
```

## Format

An object of class `MixMod` of length 21.

## Examples

``` r
if (FALSE) {
# \dontrun{
# data prepararion: see "glmmTMB_1"

GLMMadaptive::mixed_model(
  count ~ child + camper,
  random = ~ 1 | persons,
  zi_fixed = ~ child + livebait,
  data = fish,
  family = GLMMadaptive::zi.poisson()
)
# }
}
```
