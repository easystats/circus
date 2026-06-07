# brms_miss_1

brms_miss_1

## Usage

``` r
brms_miss_1
```

## Format

An object of class `brmsfit` of length 23.

## Examples

``` r
if (FALSE) {
# \dontrun{
dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/carData/TitanicSurvival.csv")
dat$survived <- ifelse(dat$survived == "yes", 1, 0)
dat$woman <- ifelse(dat$sex == "female", 1, 0) |> as.factor()
sum(is.na(dat$age))  # <-- 263 missing values

brm(
  bf(
    survived ~ woman * mi(age) + passengerClass,
    family = bernoulli(link = "logit")
  ) +
    bf(age | mi() ~ passengerClass + woman) +
    set_rescor(FALSE),
  data = dat,
  backend = "cmdstanr",
  cores = 4
)
# }
}
```
