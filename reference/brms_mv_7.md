# brms_mv_7

Model of class brmsfit

## Usage

``` r
brms_mv_7
```

## Format

An object of class `brmsfit` of length 23.

## Examples

``` r
if (FALSE) {
# \dontrun{
set.seed(123)
d <- data.frame(
  participant = sprintf("P%02d", 1:100),
  age = sample(18:70, 100, replace = TRUE),
  trials1 = sample(100:150, 100, replace = TRUE),
  trials2 = sample(100:150, 100, replace = TRUE),
  trials3 = sample(100:150, 100, replace = TRUE)
)
d$outcome1 <- vapply(trials1, function(i) rbinom(1, size = i, prob = 0.4), numeric(1))
d$outcome2 <- vapply(trials2, function(i) rbinom(1, size = i, prob = 0.6), numeric(1))
d$outcome3 <- vapply(trials3, function(i) rbinom(1, size = i, prob = 0.5), numeric(1))

outcome1_bf <- bf(outcome1 | trials(trials1) ~ age + (1 | p | participant)) + binomial()
outcome2_bf <- bf(outcome2 | trials(trials2) ~ age + (1 | p | participant)) + binomial()
outcome3_bf <- bf(outcome3 | trials(trials3) ~ age + (1 | p | participant)) + binomial()

set.seed(123)
brms_mv_7 <- brm(
  outcome1_bf + outcome2_bf + outcome3_bf,
  data = d,
  cores = 4,
  iter = 2000
)
# }
}
```
