
# circus

***The Circus of Monsters\!***

`circus` contains a variety fitted models to help the systematic testing
of other packages.

## Installation

Run the following:

``` r
install.packages("devtools")
devtools::install_github("easystats/circus")
```

``` r
library("circus")
```

## Models Creation

### Base

``` r
htest_1 <- cor.test(iris$Sepal.Width, iris$Sepal.Length, method = "spearman")
htest_2 <- cor.test(iris$Sepal.Width, iris$Sepal.Length, method = "pearson")
htest_3 <- cor.test(iris$Sepal.Width, iris$Sepal.Length, method = "kendall")
htest_4 <- t.test(iris$Sepal.Width, iris$Sepal.Length)
htest_5 <- t.test(iris$Sepal.Width, iris$Sepal.Length, var.equal = TRUE)
htest_6 <- t.test(iris$Sepal.Width, iris$Sepal.Length)
htest_7 <- t.test(mtcars$mpg ~ mtcars$vs)
htest_8 <- t.test(iris$Sepal.Width, mu = 1)

anova_1 <- anova(lm(Sepal.Width ~ Species, data=iris))
aov_1 <- aov(Sepal.Width ~ Species, data=iris)
aovlist_1 <- aov(wt ~ cyl + Error(gear), data=mtcars)

lm_1 <- lm(mpg ~ wt + cyl, data = mtcars)

glm_1 <- glm(vs ~ wt + cyl, data = mtcars, family="binomial")
glm_2 <- glm(vs ~ wt + cyl, data = mtcars, family=binomial(link="probit"))
```

### lme4

``` r
library(lme4)

lmerMod_1 <- lme4::lmer(wt ~ cyl + (1|gear), data = mtcars)

merMod_1 <- lme4::glmer(vs ~ cyl + (1|gear), data = mtcars, family="binomial")
merMod_2 <- lme4::glmer(vs ~ cyl + (1|gear), data = mtcars, family=binomial(link="probit"))
```

### Rstanarm

``` r
library(rstanarm)

stanreg_lm_1 <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars, chains=2)
stanreg_lm_2 <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars, algorithm="meanfield")
stanreg_lm_3 <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars, algorithm="fullrank")

stanreg_glm_1 <- rstanarm::stan_glm(vs ~ wt + cyl, data = mtcars, family="binomial", chains=2)
stanreg_glm_2 <- rstanarm::stan_glm(vs ~ wt + cyl, data = mtcars, family=binomial(link="probit"), chains=2)

stanreg_lmerMod_1 <- rstanarm::stan_lmer(wt ~ cyl + (1|gear), data = mtcars, chains=2)
stanreg_merMod_1 <- rstanarm::stan_glmer(vs ~ cyl + (1|gear), data = mtcars, family="binomial", chains=2)
stanreg_merMod_2 <- rstanarm::stan_glmer(vs ~ cyl + (1|gear), data = mtcars, family=binomial(link="probit"), chains=2)
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
                  
                  lm_1,
                  
                  glm_1,
                  glm_2,
                  
                  lmerMod_1,
                  merMod_1,
                  merMod_2,
                  
                  stanreg_lm_1,
                  stanreg_lm_2,
                  stanreg_lm_3,
                  stanreg_glm_1,
                  stanreg_glm_2,
                  stanreg_lmerMod_1,
                  stanreg_merMod_1,
                  stanreg_merMod_2,
                  
                  overwrite=TRUE)
```
