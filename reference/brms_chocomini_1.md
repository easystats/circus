# brms_chocomini_1

Model of class brmsfit

## Usage

``` r
brms_chocomini_1
```

## Format

An object of class `brmsfit` of length 23.

## Examples

``` r
if (FALSE) {
# \dontrun{
# Define formula and priors
f <- bf(Real ~ 0 + Intercept + (0 + Intercept|Participant),
        delta ~ 0 + Intercept + (0 + Intercept|Participant),
        phi ~ 0 + Intercept,
        k ~ 0 + Intercept + (0 + Intercept|Participant),
        family = chocomini)

# Define priors (brms::default_prior(f_mini, data=df))
priors <- c(
  prior("normal(0, 0.3)", class = "b", coef = "Intercept"),
  prior("normal(0, 0.3)", class = "b", coef = "Intercept", dpar = "delta"),
  prior("normal(3, 0.5)", class = "b", coef = "Intercept", dpar = "phi"),
  prior("normal(3, 0.5)", class = "b", coef = "Intercept", dpar = "k")
) |> brms::validate_prior(formula = f, data = df)

# Fit the model
brms_chocomini_1 <- brm(
  formula = f,
  data = df,
  prior = priors,
  family = chocomini,
  stanvars = stanvars_chocomini,
  chains = 1,
  iter = 500,
  backend = "cmdstanr"
)
# }
}
```
