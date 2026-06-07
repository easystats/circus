# brms_mv_3

Model of class brmsfit

## Usage

``` r
brms_mv_3
```

## Format

An object of class `brmsfit` of length 19.

## Examples

``` r
if (FALSE) {
# \dontrun{
data(epilepsy)
set.seed(123)
epilepsy$visit <- as.numeric(epilepsy$visit)
epilepsy$Base2 <- sample(epilepsy$Base, nrow(epilepsy), replace = TRUE)
f1 <- bf(Base ~ zAge + count + (1 |ID| patient))
f2 <- bf(Base2 ~ zAge + Trt + (1 |ID| patient))
brms::brm(f1 + f2 + set_rescor(FALSE), data = epilepsy)
# }
}
```
