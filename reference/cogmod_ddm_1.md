# cogmod_ddm_1

Model of class brms

## Usage

``` r
cogmod_ddm_1
```

## Format

An object of class `brmsfit` of length 23.

## Examples

``` r
if (FALSE) {
# \dontrun{
set.seed(123) # For reproducibility
# Experiment 1 of Wagenmakers et al. (2008), from rtdists.
data(speed_acc, package = "rtdists")

df <- data.frame(
  Participant = as.integer(as.character(speed_acc$id)),
  Condition = unname(c(accuracy = "Accuracy", speed = "Speed")[as.character(speed_acc$condition)]),
  RT = speed_acc$rt,
  Error = as.integer(as.character(speed_acc$response) != as.character(speed_acc$stim_cat)),
  Frequency = unname(c(high = "High", low = "Low", very_low = "Very Low")[sub("^nw_", "", as.character(speed_acc$frequency))])
)
df <- df[df$Participant %in% c(1, 2, 3) & df$RT <= 2, ]

f <- bf(
  RT | dec(Error) ~ Condition,
  boundary ~ Condition,
  bias ~ 1,
  ndt ~ 1,
  sigmadrift = 0,
  sigmabias = 0,
  sigmandt = 0,
  family = cogmod_ddm()
)

brm(
  f,
  data = df,
  prior = cogmod_priors(f, df),
  init = cogmod_inits(f, df),
  stanvars = cogmod_stanvars(f),
  chains = 4,
  iter = 500,
  backend = "cmdstanr"
)
# }
}
```
