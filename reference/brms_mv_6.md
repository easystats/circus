# brms_mv_6

Model of class brmsfit

## Usage

``` r
brms_mv_6
```

## Format

An object of class `brmsfit` of length 21.

## Examples

``` r
if (FALSE) {
# \dontrun{
data(jobs, package = "mediation")
f1 <- bf(job_seek ~ treat + econ_hard + sex + age)
f2 <- bf(depress2 ~ treat + job_seek + econ_hard + sex + age)
brm(f1 + f2 + set_rescor(FALSE), data = jobs)
# }
}
```
