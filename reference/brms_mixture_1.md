# brms_mixture_1

Model of class brmsfit

## Usage

``` r
brms_mixture_1
```

## Format

An object of class `brmsfit` of length 23.

## Examples

``` r
if (FALSE) {
# \dontrun{
brm(
  formula = QoL ~ time + hospital + education + age + (1 + time | ID),
  data = qol_cancer, # Your prepared dataset
  family = mixture(gaussian, nmix = 3), # Gaussian distribution for outcome, K_classes components
  chains = 4, # Number of MCMC chains
  iter = 1000, # Total iterations per chain (includes warmup)
  cores = 4,  # Number of CPU cores to use
  seed = 1234 # For reproducibility
)
# }
}
```
