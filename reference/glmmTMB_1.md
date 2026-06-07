# glmmTMB_1

Model of class glmmTMB

## Usage

``` r
glmmTMB_1
```

## Format

An object of class `glmmTMB` of length 7.

## Examples

``` r
if (FALSE) {
# \dontrun{
set.seed(123)
fish <- read.csv("https://stats.idre.ucla.edu/stat/data/fish.csv")
fish$nofish <- as.factor(fish$nofish)
fish$livebait <- as.factor(fish$livebait)
fish$camper <- as.factor(fish$camper)
fish$ID <- sample(1:4, nrow(fish), replace = TRUE)

glmmTMB(
  count ~ child + camper + (1 | persons),
  data = fish,
  family = poisson()
)
# }
}
```
