
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
                  
                  overwrite=TRUE)
## <U+2714> Setting active project to 'C:/Users/Dom/Dropbox/RECHERCHE/N/easystats/circus'
## <U+2714> Saving 'htest_1', 'htest_2', 'htest_3', 'htest_4', 'htest_5', 'htest_6', 'htest_7', 'htest_8', 'anova_1', 'aov_1', 'aovlist_1', 'lm_1', 'glm_1', 'glm_2' to 'data/htest_1.rda', 'data/htest_2.rda', 'data/htest_3.rda', 'data/htest_4.rda', 'data/htest_5.rda', 'data/htest_6.rda', 'data/htest_7.rda', 'data/htest_8.rda', 'data/anova_1.rda', 'data/aov_1.rda', 'data/aovlist_1.rda', 'data/lm_1.rda', 'data/glm_1.rda', 'data/glm_2.rda'
```
