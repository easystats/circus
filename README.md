
# circus

***The Circus of Monsters\!***

`circus` contains a variety fitted models to help the systematic testing
of other packages.

## Installation

Run the following:

``` r
install.packages("devtools")
devtools::install_github("easystats/circus")
library("circus")
```

## How to use it

You can use the package in your `testthat` block by directly calling the
models. However, for it to work on **travis**, it is easier to directly
download them from github with the `download_model` in the
[`insight`](https://github.com/easystats/insight) package:

``` r
test_that("my_function_works", {
  library(insight)

  # model <- circus::lmerMod_1  # Local solution
  model <- insight::download_model("lmerMod_1")
  testthat::expect_equal(myFunction(model), 0.333)
})
```

## Contribute

Feel free to add any model you find missing\! Any scary creature for the
depth of your mind has its place here\!

In order to add models, do the following:

1.  Add model creation code in the `README.Rmd`
2.  Add your model-name in the `usethis::use_data()` function (last
    chunk) in the `README.Rmd`
3.  Add documentation for your model in the
    [`R/data.R/`](https://github.com/easystats/circus/blob/main/R/data.R)
    file
4.  Now fit your model and save it to the data-folder, using
    `usethis::use_data(<yourmodel>)`.
5.  Knit the `README.Rmd` file to generate the `README.md`. Since
    code-chunks are not evaluated, this runs pretty quickly.
6.  Check and build documentation for the package (to generate the
    `.rd`-files)
7.  Upload following files to github: `/data/<yourmodel.rda>`,
    `README.Rmd`, `README.md`, `data.R` and `data.rd`.

**Note** When you build or install the package, it is recommended to do
so with following build-options: `R CMD INSTALL --no-multiarch
--with-keep.source --no-libs --no-data`. Furthermore, when building the
documentation, make sure to **not** build the vignettes.

## List of Models

### Base

``` r
# h-tests
htest_1 <- cor.test(iris$Sepal.Width, iris$Sepal.Length, method = "spearman")
htest_2 <- cor.test(iris$Sepal.Width, iris$Sepal.Length, method = "pearson")
htest_3 <- cor.test(iris$Sepal.Width, iris$Sepal.Length, method = "kendall")
htest_4 <- t.test(iris$Sepal.Width, iris$Sepal.Length)
htest_5 <- t.test(iris$Sepal.Width, iris$Sepal.Length, var.equal = TRUE)
htest_6 <- t.test(iris$Sepal.Width, iris$Sepal.Length)
htest_7 <- t.test(mtcars$mpg ~ mtcars$vs)
htest_8 <- t.test(iris$Sepal.Width, mu = 1)

# ANOVAs
anova_1 <- anova(lm(Sepal.Width ~ Species, data=iris))
aov_1 <- aov(Sepal.Width ~ Species, data=iris)
aovlist_1 <- aov(wt ~ cyl + Error(gear), data=mtcars)

data <- iris
data$Cat1 <- rep(c("X", "X", "Y"), length.out = nrow(data))
data$Cat2 <- rep(c("A", "B"), length.out = nrow(data))

aov_2 <- aov(Sepal.Length ~ Species * Cat1 * Cat2, data=data)
anova_2 <- anova(lm(Sepal.Length ~ Species * Cat1 * Cat2, data=data))
aovlist_2 <- aov(Sepal.Length ~ Species * Cat1+ Error(Cat2), data=data)

aov_3 <- aov(Sepal.Length ~ Species / Cat1 * Cat2, data=data)
anova_3 <- anova(lm(Sepal.Length ~ Species / Cat1 * Cat2, data=data))
aovlist_3 <- aov(Sepal.Length ~ Species / Cat1 + Error(Cat2), data=data)


# GLMs
lm_0 <- lm(mpg ~ 1, data = mtcars)
lm_1 <- lm(mpg ~ wt, data = mtcars)
lm_2 <- lm(mpg ~ wt + cyl, data = mtcars)
lm_3 <- lm(mpg ~ wt * cyl, data = mtcars)
lm_4 <- lm(mpg ~ wt + poly(cyl, 2), data = mtcars)
lm_5 <- lm(mpg ~ wt + poly(cyl, 2, raw=TRUE), data = mtcars)
lm_6 <- lm(mpg ~ wt * as.factor(gear), data = mtcars)
lm_7 <- lm(mpg ~ as.factor(gear) / wt, data = mtcars)

set.seed(123)
mtcars$count <- rpois(nrow(mtcars), 2)

glm_0 <- glm(vs ~ 1, data = mtcars, family="binomial")
glm_1 <- glm(vs ~ wt, data = mtcars, family="binomial")
glm_2 <- glm(vs ~ wt + cyl, data = mtcars, family="binomial")
glm_3 <- glm(vs ~ wt * cyl, data = mtcars, family="binomial")
glm_4 <- glm(vs ~ wt + cyl, data = mtcars, family=binomial(link="probit"))
glm_5 <- glm(count ~ wt + cyl, family = "poisson", data = mtcars)

anova_4 <- anova(lm_0, lm_1, lm_2)
```

### lme4

``` r
library(lme4)

lmerMod_0 <- lme4::lmer(wt ~ 1 + (1|gear), data = mtcars)
lmerMod_1 <- lme4::lmer(wt ~ cyl + (1|gear), data = mtcars)
lmerMod_2 <- lme4::lmer(wt ~ drat + cyl + (1|gear), data = mtcars)
lmerMod_3 <- lme4::lmer(wt ~ drat * cyl + (1|gear), data = mtcars)
lmerMod_4 <- lme4::lmer(wt ~ drat / cyl + (1|gear), data = mtcars)
lmerMod_5 <- lme4::lmer(Petal.Width ~ Cat1 + (1+Cat1|Species), data=data)

merMod_0 <- lme4::glmer(vs ~ 1 + (1|gear), data = mtcars, family="binomial")
merMod_1 <- lme4::glmer(vs ~ cyl + (1|gear), data = mtcars, family="binomial")
merMod_2 <- lme4::glmer(vs ~ drat + cyl + (1|gear), data = mtcars, family="binomial")
merMod_3 <- lme4::glmer(vs ~ drat * cyl + (1|gear), data = mtcars, family="binomial")
merMod_4 <- lme4::glmer(vs ~ cyl + (1|gear), data = mtcars, family=binomial(link="probit"))

anova_lmerMod_0 <- anova(lmerMod_0)
anova_lmerMod_1 <- anova(lmerMod_1)
anova_lmerMod_2 <- anova(lmerMod_2)
anova_lmerMod_3 <- anova(lmerMod_3)
anova_lmerMod_4 <- anova(lmerMod_4)
anova_lmerMod_5 <- anova(lmerMod_5)

anova_lmerMod_6 <- anova(lmerMod_0, lmerMod_1, lmerMod_2)
```

### glmmTMB

``` r
library(glmmTMB)

set.seed(123)
fish <- read.csv("https://stats.idre.ucla.edu/stat/data/fish.csv")
fish$nofish <- as.factor(fish$nofish)
fish$livebait <- as.factor(fish$livebait)
fish$camper <- as.factor(fish$camper)
fish$ID <- sample(1:4, nrow(fish), replace = TRUE)

glmmTMB_1 <- glmmTMB(
  count ~ child + camper + (1 | persons),
  data = fish,
  family = poisson()
)

glmmTMB_2 <- glmmTMB(count ~ mined + (1 | site),
  ziformula =  ~ mined,
  family = poisson,
  data = Salamanders
)

glmmTMB_3 <- glmmTMB(
  count ~ spp + mined + (1 | site),
  ziformula =  ~ spp + mined,
  family = nbinom2,
  data = Salamanders
)

glmmTMB_4 <- glmmTMB(
  count ~ spp + mined + (1 | site),
  ziformula =  ~ spp + mined,
  family = truncated_poisson,
  data = Salamanders
)

data(cbpp, package = "lme4")
glmmTMB_5 <- glmmTMB(
  cbind(incidence, size - incidence) ~ period + (1 | herd),
  data = cbpp,
  family = binomial
)
  
glmmTMB_zi_1 <- glmmTMB(
  count ~ child + camper + (1 | persons),
  ziformula = ~ child + camper + (1 | persons),
  data = fish,
  family = truncated_poisson()
)

glmmTMB_zi_2 <- glmmTMB(
  count ~ child + camper + (1 | persons),
  ziformula = ~ child + livebait + (1 | persons),
  data = fish,
  family = poisson()
)

glmmTMB_zi_3 <- glmmTMB(
  count ~ child + camper + (1 | persons),
  ziformula = ~ child + livebait + (1 | ID),
  dispformula = ~xb,
  data = fish,
  family = truncated_poisson()
)
```

### GLMMadaptive

``` r
library("GLMMadaptive")
library("lme4")

data(cbpp)

fish <- read.csv("https://stats.idre.ucla.edu/stat/data/fish.csv")
fish$nofish <- as.factor(fish$nofish)
fish$livebait <- as.factor(fish$livebait)
fish$camper <- as.factor(fish$camper)

GLMMadaptive_zi_1 <- GLMMadaptive::mixed_model(
  count ~ child + camper,
  random = ~ 1 | persons,
  zi_fixed = ~ child + livebait,
  data = fish,
  family = GLMMadaptive::zi.poisson()
)

GLMMadaptive_zi_2 <- GLMMadaptive::mixed_model(
  count ~ child + camper,
  random = ~ 1 | persons,
  zi_fixed = ~ child + livebait,
  zi_random = ~ 1 | persons,
  data = fish,
  family = GLMMadaptive::zi.poisson()
)

GLMMadaptive_1 <- GLMMadaptive::mixed_model(
  cbind(incidence, size - incidence) ~ period,
  random = ~ 1 | herd,
  data = cbpp,
  family = binomial
)
```

### Rstanarm

``` r
set.seed(333)

library(rstanarm)

stanreg_lm_0 <- rstanarm::stan_glm(mpg ~ 1, data = mtcars)
stanreg_lm_1 <- rstanarm::stan_glm(mpg ~ wt, data = mtcars)
stanreg_lm_2 <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
stanreg_lm_3 <- rstanarm::stan_glm(mpg ~ wt * cyl, data = mtcars)
stanreg_lm_4 <- rstanarm::stan_glm(mpg ~ wt + poly(cyl, 2), data = mtcars)
stanreg_lm_5 <- rstanarm::stan_glm(mpg ~ wt + poly(cyl, 2, raw=TRUE), data = mtcars)
stanreg_lm_6 <- rstanarm::stan_glm(mpg ~ wt * as.factor(gear), data = mtcars)
stanreg_lm_7 <- rstanarm::stan_glm(mpg ~ as.factor(gear) / wt, data = mtcars)

set.seed(123)
mtcars$count <- rpois(nrow(mtcars), 2)

stanreg_glm_0 <- rstanarm::stan_glm(vs ~ 1, data = mtcars, family="binomial")
stanreg_glm_1 <- rstanarm::stan_glm(vs ~ wt, data = mtcars, family="binomial")
stanreg_glm_2 <- rstanarm::stan_glm(vs ~ wt + cyl, data = mtcars, family="binomial")
stanreg_glm_3 <- rstanarm::stan_glm(vs ~ wt * cyl, data = mtcars, family="binomial")
stanreg_glm_4 <- rstanarm::stan_glm(vs ~ wt + cyl, data = mtcars, family=binomial(link="probit"))
stanreg_glm_5 <- rstanarm::stan_glm(count ~ wt + cyl, data = mtcars, family="poisson")
stanreg_glm_6 <- rstanarm::stan_glm(Sepal.Width ~ Species * Petal.Length, data = iris)

stanreg_lmerMod_0 <- rstanarm::stan_lmer(wt ~ 1 + (1|gear), data = mtcars)
stanreg_lmerMod_1 <- rstanarm::stan_lmer(wt ~ cyl + (1|gear), data = mtcars)
stanreg_lmerMod_2 <- rstanarm::stan_lmer(wt ~ drat + cyl + (1|gear), data = mtcars)
stanreg_lmerMod_3 <- rstanarm::stan_lmer(wt ~ drat * cyl + (1|gear), data = mtcars)
stanreg_lmerMod_4 <- rstanarm::stan_lmer(wt ~ drat / cyl + (1|gear), data = mtcars)
stanreg_lmerMod_5 <- rstanarm::stan_lmer(Petal.Width ~ Cat1 + (1+Cat1|Species), data=data)

stanreg_merMod_0 <- rstanarm::stan_glmer(vs ~ 1 + (1|gear), data = mtcars, family="binomial")
stanreg_merMod_1 <- rstanarm::stan_glmer(vs ~ cyl + (1|gear), data = mtcars, family="binomial")
stanreg_merMod_2 <- rstanarm::stan_glmer(vs ~ drat + cyl + (1|gear), data = mtcars, family="binomial")
stanreg_merMod_3 <- rstanarm::stan_glmer(vs ~ drat * cyl + (1|gear), data = mtcars, family="binomial")
stanreg_merMod_4 <- rstanarm::stan_glmer(vs ~ cyl + (1|gear), data = mtcars, family=binomial(link="probit"))
stanreg_merMod_5 <- stan_glmer(
  cbind(incidence, size - incidence) ~ size + period + (1 | herd),
  data = lme4::cbpp, family = binomial, QR = TRUE,
  chains = 2, cores = 1, seed = 12345, iter = 500, refresh = 0
)


stanreg_meanfield_lm_1 <- update(stanreg_lm_1, algorithm="meanfield")
stanreg_fullrank_lm_1 <- update(stanreg_lm_1, algorithm="fullrank")


stanreg_gamm4_1 <- stan_gamm4(Sepal.Width ~ s(Petal.Length), data=iris)
stanreg_gamm4_2 <- stan_gamm4(Sepal.Width ~ Species + s(Petal.Length), data=iris)
stanreg_gamm4_3 <- stan_gamm4(Sepal.Width ~ Species + s(Petal.Length), random = ~ (1 | Cat1), data=data)
```

### BRMS

``` r
library(lme4)
library(brms)
library(dplyr)

data(sleepstudy)
data(mtcars)
data(epilepsy)
data(jobs, package = "mediation")

set.seed(123)
epilepsy$visit <- as.numeric(epilepsy$visit)
epilepsy$Base2 <- sample(epilepsy$Base, nrow(epilepsy), replace = TRUE)
mtcars$cyl_ord <- as.ordered(mtcars$cyl)
mtcars$gear_fct <- as.ordered(mtcars$gear)
zinb <- read.csv("http://stats.idre.ucla.edu/stat/data/fish.csv")

dat <- read.table(header = TRUE, text = "
      n r r/n group treat c2 c1 w
      62 3 0.048387097 1 0 0.1438 1.941115288 1.941115288
      96 1 0.010416667 1 0 0.237 1.186583128 1.186583128
      17 0 0 0 0 0.2774 1.159882668 3.159882668
      41 2 0.048780488 1 0 0.2774 1.159882668 3.159882668
      212 170 0.801886792 0 0 0.2093 1.133397521 1.133397521
      143 21 0.146853147 1 1 0.1206 1.128993008 1.128993008
      143 0 0 1 1 0.1707 1.128993008 2.128993008
      143 33 0.230769231 0 1 0.0699 1.128993008 1.128993008
      73 62 1.260273973 0 1 0.1351 1.121927228 1.121927228
      73 17 0.232876712 0 1 0.1206 1.121927228 1.121927228")
dat$treat <- as.factor(dat$treat)

set.seed(123)
sleepstudy$grp <- sample(1:5, size = 180, replace = TRUE)
sleepstudy$cat <- as.factor(sample(1:5, size = 180, replace = TRUE))
sleepstudy$Reaction_d <-
  ifelse(sleepstudy$Reaction < median(sleepstudy$Reaction), 0, 1)
sleepstudy <- sleepstudy %>%
  group_by(grp) %>%
  mutate(subgrp = sample(1:15, size = n(), replace = TRUE))
zinb$count2 <- rpois(250, 5)
zinb$count2[sample(1:250, 100, replace = FALSE)] <- 0
bprior1 <- prior(student_t(5, 0, 10), class = b) + prior(cauchy(0, 2), class = sd)

brms_1 <- brm(mpg ~ wt + cyl, data = mtcars)
brms_2 <- brm(
  r | trials(n) ~ treat * c2, 
  data = dat, 
  family = binomial(link = logit), 
  chains = 1, iter = 500
)

brms_ordinal_1 <- brm(cyl_ord ~ mpg, data = mtcars, family = cumulative())
brms_ordinal_1_wt <- brm(cyl_ord | weights(wt) ~ mpg, data = mtcars, family = cumulative())

brms_categorical_1_num <- brm(gear ~ mpg, data = mtcars, family = categorical())
brms_categorical_1_fct <- brm(gear_fct ~ mpg, data = mtcars, family = categorical())
brms_categorical_1_wt <- brm(gear | weights(wt) ~ mpg, data = mtcars, family = categorical())

brms_mixed_1 <- brm(mpg ~ wt + (1 | cyl) + (1 + wt | gear), data = mtcars)
brms_mixed_2 <- brm(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
brms_mixed_3 <- brm(Reaction ~ Days + (1 | grp / subgrp) + (1 | Subject), data = sleepstudy)
brms_mixed_3 <- brm(Petal.Length ~ Petal.Width + (1 | Species), data = iris)
brms_mixed_5 <- brm(Reaction_d ~ Days + cat + (1 | Subject), data = sleepstudy, family = bernoulli())
brms_mixed_6 <- brm(
  count ~ Age + Base * Trt + (1 | patient),
  data = epilepsy,
  family = poisson(),
  prior = bprior1,
  chains = 1,
  iter = 500
)

brms_mv_1 <- brm(cbind(cyl, gear, carb) ~ wt + hp, data = mtcars)
f1 <- bf(mpg ~ wt + disp + cyl + hp + (1 |CAR| gear))
f2 <- bf(wt ~ disp + cyl + hp + (1 |CAR| gear))
brms_mv_2 <- brm(f1 + f2 + set_rescor(FALSE), data = mtcars)
f1 <- bf(Base ~ Age + count + (1 |ID| patient))
f2 <- bf(Base2 ~ Age + Trt + (1 |ID| patient))
brms_mv_3 <- brm(f1 + f2 + set_rescor(FALSE), data = epilepsy)
f1 <- bf(Sepal.Length ~ Petal.Length + Sepal.Width + Species)
f2 <- bf(Sepal.Width ~ Species)
brms_mv_4 <- brm(f1 + f2 + set_rescor(FALSE), data = iris, chains = 1, iter = 500)
bf1 <- bf(count ~ child + camper + (1 | persons), zi ~ camper + (1 | persons))
bf2 <- bf(count2 ~ child + livebait + (1 | persons), zi ~ child + (1 | persons))
brms_mv_5 <- brm(bf1 + bf2, data = zinb, family = zero_inflated_poisson(), chains = 1, iter = 500)
f1 <- bf(job_seek ~ treat + econ_hard + sex + age)
f2 <- bf(depress2 ~ treat + job_seek + econ_hard + sex + age)
brms_mv_5 <- brm(f1 + f2 + set_rescor(FALSE), data = jobs)

brms_zi_1 <- brm(bf(count ~ persons + child + camper, zi ~ child + camper), data = zinb, family = zero_inflated_poisson())
brms_zi_2 <- brm(bf(count ~ persons + child + camper + (1 | persons), zi ~ child + camper + (1 | persons)), data = zinb, family = zero_inflated_poisson())
brms_zi_3 <- brm(
  bf(
    count ~ child + camper + (1 | persons),
    zi ~ child + camper + (1 | persons)
  ),
  data = zinb,
  family = zero_inflated_poisson(),
  chains = 1,
  iter = 500
)

brms_4bf_1 <- brm(Sepal.Length ~ 1, data = iris, save_all_pars = TRUE)
brms_4bf_2 <- brm(Sepal.Length ~ Species, data = iris, save_all_pars = TRUE)
brms_4bf_3 <- brm(Sepal.Length ~ Petal.Length, data = iris, save_all_pars = TRUE)
brms_4bf_4 <- brm(Sepal.Length ~ Species + Petal.Length, data = iris, save_all_pars = TRUE)
brms_4bf_5 <- brm(Sepal.Length ~ Species * Petal.Length, data = iris, save_all_pars = TRUE)
```

### Other packages

``` r
library(betareg)
data("GasolineYield")
data("FoodExpenditure")

betareg_1 <- betareg(yield ~ batch + temp, data = GasolineYield)
betareg_2 <- betareg(I(food/income) ~ income + persons, data = FoodExpenditure)

library(AER)
library(censReg)
data("Affairs", package = "AER")

censReg_1 <- censReg(affairs ~ age + yearsmarried + religiousness + occupation + rating, data = Affairs)

data(CigarettesSW)
CigarettesSW$rprice <- with(CigarettesSW, price / cpi)
CigarettesSW$rincome <- with(CigarettesSW, income / population / cpi)
CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax) / cpi)

ivreg_1 <- ivreg(
  log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff + I(tax / cpi), 
  data = CigarettesSW, 
  subset = year == "1995"
)

library(ordinal)
data(wine)
clm_1 <- clm(rating ~ temp * contact, data = wine)
clm2_1 <- clm2(rating ~ temp * contact, data = wine)
```

## Save

``` r
usethis::use_data(htest_1,
                  htest_2,
                  htest_3,
                  htest_4,
                  htest_5,
                  htest_6,
                  htest_7,
                  htest_8,
                  
                  anova_1,
                  aov_1,
                  aovlist_1,
                  anova_2,
                  aov_2,
                  aovlist_2,
                  anova_3,
                  aov_3,
                  aovlist_3,
                  anova_4,
                  
                  lm_0,
                  lm_1,
                  lm_2,
                  lm_3,
                  lm_4,
                  lm_5,
                  lm_6,
                  lm_7,
                  glm_0,
                  glm_1,
                  glm_2,
                  glm_3,
                  glm_4,
                  glm_5,
                  
                  lmerMod_0,
                  lmerMod_1,
                  lmerMod_2,
                  lmerMod_3,
                  lmerMod_4,
                  lmerMod_5,
                  merMod_0,
                  merMod_1,
                  merMod_2,
                  merMod_3,
                  merMod_4,
                  
                  anova_lmerMod_0,
                  anova_lmerMod_1,
                  anova_lmerMod_2,
                  anova_lmerMod_3,
                  anova_lmerMod_4,
                  anova_lmerMod_5,
                  anova_lmerMod_6,
                  
                  glmmTMB_1,
                  glmmTMB_2,
                  glmmTMB_3,
                  glmmTMB_4,
                  glmmTMB_5,
                  
                  glmmTMB_zi_1,
                  glmmTMB_zi_2,
                  glmmTMB_zi_3,
                  
                  GLMMadaptive_1,
                  GLMMadaptive_zi_1,
                  GLMMadaptive_zi_2,
                  
                  stanreg_lm_0,
                  stanreg_lm_1,
                  stanreg_lm_2,
                  stanreg_lm_3,
                  stanreg_lm_4,
                  stanreg_lm_5,
                  stanreg_lm_6,
                  stanreg_lm_7,
                  stanreg_glm_0,
                  stanreg_glm_1,
                  stanreg_glm_2,
                  stanreg_glm_3,
                  stanreg_glm_4,
                  stanreg_glm_5,
                  stanreg_glm_6,
                  stanreg_lmerMod_0,
                  stanreg_lmerMod_1,
                  stanreg_lmerMod_2,
                  stanreg_lmerMod_3,
                  stanreg_lmerMod_4,
                  stanreg_lmerMod_5,
                  stanreg_merMod_0,
                  stanreg_merMod_1,
                  stanreg_merMod_2,
                  stanreg_merMod_3,
                  stanreg_merMod_4,
                  stanreg_merMod_5,
                  stanreg_meanfield_lm_1,
                  stanreg_fullrank_lm_1,
                  
                  stanreg_gamm4_1,
                  stanreg_gamm4_2,
                  stanreg_gamm4_3,
                  
                  brms_1,
                  brms_2,
                  
                  brms_mixed_1,
                  brms_mixed_2,
                  brms_mixed_3,
                  brms_mixed_4,
                  brms_mixed_5,
                  brms_mixed_6,
                  
                  brms_mv_1,
                  brms_mv_2,
                  brms_mv_3,
                  brms_mv_4,
                  brms_mv_5,
                  brms_mv_6,
                  
                  brms_zi_1,
                  brms_zi_2,
                  brms_zi_3,
                  
                  brms_4bf_1,
                  brms_4bf_2,
                  brms_4bf_3,
                  brms_4bf_4,
                  brms_4bf_5,
                  
                  brms_ordinal_1,
                  brms_ordinal_1_wt,
                  
                  brms_categorical_1_num,
                  brms_categorical_1_fct,
                  brms_categorical_1_wt,
                  
                  betareg_1,
                  betareg_2,
                  censReg_1,
                  ivreg_1,
                  clm_1,
                  clm2_1,
                  
                  overwrite=TRUE)
```
