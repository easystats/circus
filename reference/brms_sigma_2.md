# brms_sigma_2

Model of class brmsfit

## Usage

``` r
brms_sigma_2
```

## Format

An object of class `brmsfit` of length 23.

## Examples

``` r
if (FALSE) {
# \dontrun{
set.seed(123)
d <- data.frame(
  Condition = rep(c("A", "B", "C"), each=30),
  Participant = as.factor(rep_len(paste0("S", 1:5), 90)),
  Response = rnorm(90)
)
brm(
  bf(
    Response ~ Condition + (Condition | Participant),
    sigma ~ Condition + (Condition | Participant),
    family = exgaussian()
  ),
  data = d,
  iter = 2000,
  chains = 4,
  cores = 4,
  backend = "cmdstanr"
)
# }
}
```
