# brms_4

Model of class brmsfit

## Usage

``` r
brms_4
```

## Format

An object of class `brmsfit` of length 23.

## Examples

``` r
if (FALSE) {
# \dontrun{
brm(
  Sepal.Length ~ Sepal.Width * Species,
  data = iris,
  family = gaussian(),
  chains = 2, iter = 2000, warmup = 1000, cores = 2
)# }
}
```
