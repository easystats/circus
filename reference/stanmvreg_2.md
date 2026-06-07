# stanmvreg_2

Model of class stanmvreg

## Usage

``` r
stanmvreg_2
```

## Format

An object of class `stanmvreg` (inherits from `stanreg`, `lmerMod`) of
length 20.

## Examples

``` r
if (FALSE) {
# \dontrun{
library(rstanarm)
library(mediation)
data(jobs)
stan_mvmer(
  list(job_seek ~ treat + econ_hard + sex + age + (1 | occp),
       depress2 ~ treat + job_seek + econ_hard + sex + age + (1 | occp)),
  data = jobs,
  cores = 4,
  seed = 1234,
  refresh = 0)
# }
}
```
