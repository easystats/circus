# brms_aterm_1

Model of class brmsfit

## Usage

``` r
brms_aterm_1
```

## Format

An object of class `brmsfit` of length 22.

## Examples

``` r
if (FALSE) {
# \dontrun{
brm(am | trials(1) ~ hp,
  data = mtcars, family = binomial(),
  chains = 2, iter = 200,
  backend = "cmdstanr", cores = 2
)# }
}
```
