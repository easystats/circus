# brms_logistic_1

Model of class brmsfit

## Usage

``` r
brms_logistic_1
```

## Format

An object of class `brmsfit` of length 23.

## Examples

``` r
if (FALSE) {
# \dontrun{
data(efc, package = "ggeffects")
efc <- datawizard::to_factor(efc, c("c161sex", "c172code", "e16sex", "e42dep"))
levels(efc$c172code) <- c("low", "mid", "high")
brm(barthtot ~ e16sex + c161sex + c172code * c160age + c12hour + e42dep, data = efc)
# }
}
```
