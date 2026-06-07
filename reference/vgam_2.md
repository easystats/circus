# vgam_2

Model of class vgam

## Usage

``` r
vgam_2
```

## Format

An object of class `vgam` of length 1.

## Examples

``` r
if (FALSE) {
data("hunua")
vgam(
  cbind(agaaus, kniexc) ~ vitluc + s(altitude, df = c(2, 3)),
  binomialff(multiple.responses = TRUE),
  data = hunua
)
}
```
