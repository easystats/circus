# stanreg_bernoulli_1

Model of class stanreg

## Usage

``` r
stanreg_bernoulli_1
```

## Format

An object of class `stanreg` (inherits from `glm`, `lm`) of length 28.

## Examples

``` r
if (FALSE) {
# \dontrun{
set.seed(1234)
dat <- data.frame(
  outcome = rbinom(n = 100, size = 1, prob = 0.35),
  var_binom = as.factor(rbinom(n = 100, size = 1, prob = 0.3)),
  var_cont = rnorm(n = 100, mean = 10, sd = 7),
  groups = sample(letters[1:2], size = 100, replace = TRUE)
)
set.seed(1234)
stan_glm(outcome ~ var_binom * groups + var_cont, data = dat, family = binomial())
# }
}
```
