#' gamlss_1
#'
#' Model of class gamlss
#' @examples
#' data(abdom)
#' gamlss(
#'   y ~ pb(x), sigma.formula =  ~ pb(x),
#'   family = BCT, data = abdom, method = mixed(1, 20)
#' )
"gamlss_1"



#' gamlss_2
#'
#' Model of class gamlss
#' @examples
#' data(aids)
#' gamlss(y ~ x + qrt, data = aids, family = NBI)
"gamlss_2"





#' vgam_1
#'
#' Model of class vgam
#' @examples
#' data("hunua")
#' vgam(agaaus ~ vitluc + s(altitude, df = 2), binomialff, data = hunua)
"vgam_1"


#' vgam_2
#'
#' Model of class vgam
#' @examples
#' data("hunua")
#' vgam(
#'   cbind(agaaus, kniexc) ~ vitluc + s(altitude, df = c(2, 3)),
#'   binomialff(multiple.responses = TRUE),
#'   data = hunua
#' )
"vgam_2"





#' gam_zi_1
#'
#' Model of class gam
#' @examples
#' library(mgcv)
#' f0 <- function(x) 2 * sin(pi * x)
#' f1 <- function(x) exp(2 * x)
#' f2 <- function(x) 0.2 * x ^ 11 * (10 * (1 - x)) ^ 6 + 10 * (10 * x) ^ 3 * (1 - x) ^ 10
#' n <- 500
#' set.seed(5)
#' x0 <- runif(n)
#' x1 <- runif(n)
#' x2 <- runif(n)
#' x3 <- runif(n)
#'
#' eta1 <- f0(x0) + f1(x1) - 3
#' p <- binomial()$linkinv(eta1)
#' y <- as.numeric(runif(n) < p) ## 1 for presence, 0 for absence
#'
#' ind <- y > 0
#' eta2 <- f2(x2[ind]) / 3
#' y[ind] <- rpois(exp(eta2), exp(eta2))
#'
#' gam(list(y ~ s(x2) + s(x3),  ~ s(x0) + s(x1)), family = ziplss())
"gam_zi_1"



#' gam_mv_1
#'
#' Model of class gam
#' @examples
#' library(mgcv)
#' V <- matrix(c(2, 1, 1, 2), 2, 2)
#' f0 <- function(x) 2 * sin(pi * x)
#' f1 <- function(x) exp(2 * x)
#' f2 <- function(x) 0.2 * x ^ 11 * (10 * (1 - x)) ^ 6 + 10 * (10 * x) ^ 3 * (1 - x) ^ 10
#'
#' n <- 300
#' x0 <- runif(n)
#' x1 <- runif(n)
#' x2 <- runif(n)
#' x3 <- runif(n)
#' y <- matrix(0, n, 2)
#'
#' for (i in 1:n) {
#'   mu <- c(f0(x0[i]) + f1(x1[i]), f2(x2[i]))
#'   y[i, ] <- rmvn(1, mu, V)
#' }
#'
#' dat <- data.frame(y0 = y[, 1], y1 = y[, 2], x0 = x0, x1 = x1, x2 = x2, x3 = x3)
#'
#' gam(list(y0 ~ s(x0) + s(x1), y1 ~ s(x2) + s(x3)), family = mvn(d = 2), data = dat)
"gam_mv_1"





#' htest_1
#'
#' Model of class htest
#' @examples
#' cor.test(iris$Sepal.Width, iris$Sepal.Length, method = "spearman")
"htest_1"

#' htest_2
#'
#' Model of class htest
#' @examples
#' cor.test(iris$Sepal.Width, iris$Sepal.Length, method = "pearson")
"htest_2"

#' htest_3
#'
#' Model of class htest
#' @examples
#' cor.test(iris$Sepal.Width, iris$Sepal.Length, method = "kendall")
"htest_3"

#' htest_4
#'
#' Model of class htest
#' @examples
#' t.test(iris$Sepal.Width, iris$Sepal.Length)
"htest_4"

#' htest_5
#'
#' Model of class htest
#' @examples
#' t.test(iris$Sepal.Width, iris$Sepal.Length, var.equal = TRUE)
"htest_5"

#' htest_6
#'
#' Model of class htest
#' @examples
#' t.test(iris$Sepal.Width, iris$Sepal.Length)
"htest_6"

#' htest_7
#'
#' Model of class htest
#' @examples
#' t.test(mtcars$mpg ~ mtcars$vs)
"htest_7"

#' htest_8
#'
#' Model of class htest
#' @examples
#' t.test(iris$Sepal.Width, mu = 1)
"htest_8"




#' anova_1
#'
#' Model of class anova
#' @examples
#' anova(lm(Sepal.Width ~ Species, data = iris))
"anova_1"


#' aov_1
#'
#' Model of class aov
#' @examples
#' aov(Sepal.Width ~ Species, data = iris)
"aov_1"


#' aovlist_1
#'
#' Model of class aovlist
#' @examples
#' aov(wt ~ cyl + Error(gear), data = mtcars)
"aovlist_1"


#' anova_2
#'
#' Model of class anova
#' @examples
#' data <- iris
#' data$Cat1 <- rep(c("X", "X", "Y"), length.out = nrow(data))
#' data$Cat2 <- rep(c("A", "B"), length.out = nrow(data))
#'
#' anova(lm(Sepal.Length ~ Species * Cat1 * Cat2, data = data))
"anova_2"


#' aov_2
#'
#' Model of class aov
#' @examples
#' data <- iris
#' data$Cat1 <- rep(c("X", "X", "Y"), length.out = nrow(data))
#' data$Cat2 <- rep(c("A", "B"), length.out = nrow(data))
#'
#' aov(Sepal.Length ~ Species * Cat1 * Cat2, data = data)
"aov_2"


#' aovlist_2
#'
#' Model of class aovlist
#' @examples
#' data <- iris
#' data$Cat1 <- rep(c("X", "X", "Y"), length.out = nrow(data))
#' data$Cat2 <- rep(c("A", "B"), length.out = nrow(data))
#'
#' aov(Sepal.Length ~ Species * Cat1 + Error(Cat2), data = data)
"aovlist_2"


#' anova_3
#'
#' Model of class anova
"anova_3"


#' aov_3
#'
#' Model of class aov
"aov_3"


#' aovlist_3
#'
#' Model of class aovlist
"aovlist_3"


#' anova_4
#'
#' Model of class anova
"anova_4"







#' lm_0
#'
#' Model of class lm
#' @examples
#' lm(formula = mpg ~ 1, data = mtcars)
"lm_0"


#' lm_1
#'
#' Model of class lm
#' @examples
#' lm_1 <- lm(mpg ~ wt, data = mtcars)
"lm_1"


#' lm_2
#'
#' Model of class lm
#' @examples
#' lm_1 <- lm(mpg ~ wt + cyl, data = mtcars)
"lm_2"


#' lm_3
#'
#' Model of class lm
#' @examples
#' lm(formula = mpg ~ wt * cyl, data = mtcars)
"lm_3"


#' lm_4
#'
#' Model of class lm
#' @examples
#' lm(formula = mpg ~ wt + poly(cyl, 2), data = mtcars)
"lm_4"


#' lm_5
#'
#' Model of class lm
#' @examples
#' lm(formula = mpg ~ wt + poly(cyl, 2, raw = TRUE), data = mtcars)
"lm_5"


#' lm_6
#'
#' Model of class lm
#' @examples
#' lm(formula = mpg ~ wt * as.factor(gear), data = mtcars)
"lm_6"


#' lm_7
#'
#' Model of class lm
#' @examples
#' lm(formula = mpg ~ as.factor(gear)/wt, data = mtcars)
"lm_7"





#' glm_0
#'
#' Model of class glm
#' @examples
#' glm(formula = vs ~ 1, family = "binomial", data = mtcars)
"glm_0"


#' glm_1
#'
#' Model of class glm
#' @examples
#' glm(formula = vs ~ wt, family = "binomial", data = mtcars)
"glm_1"


#' glm_2
#'
#' Model of class glm
#' @examples
#' glm(formula = vs ~ wt + cyl, family = "binomial", data = mtcars)
"glm_2"


#' glm_3
#'
#' Model of class glm
#' @examples
#' glm(formula = vs ~ wt * cyl, family = "binomial", data = mtcars)
"glm_3"


#' glm_4
#'
#' Model of class glm
#' @examples
#' glm(  formula = vs ~ wt + cyl,  family = binomial(link = "probit"),  data = mtcars)
"glm_4"


#' glm_5
#'
#' Model of class glm
#' @examples
#' set.seed(123)
#' mtcars$count <- rpois(nrow(mtcars), 2)
#' glm(formula = count ~ wt + cyl, family = "poisson", data = mtcars)
"glm_5"



#' glm_nb_1
#'
#' Model of class glm
#' @examples
#' library(MASS)
#' data(efc, package = "sjmisc")
#' glm.nb(tot_sc_e ~ neg_c_7 + e42dep + c172code, data = efc)
"glm_nb_1"








#' lmerMod_0
#'
#' Model of class lmerMod
#' @examples
#' lme4::lmer(wt ~ 1 + (1 | gear), data = mtcars)
"lmerMod_0"


#' lmerMod_1
#'
#' Model of class lmerMod
#' @examples
#' lme4::lmer(wt ~ cyl + (1 | gear), data = mtcars)
"lmerMod_1"


#' lmerMod_2
#'
#' Model of class lmerMod
#' @examples
#' lme4::lmer(wt ~ drat + cyl + (1 | gear), data = mtcars)
"lmerMod_2"


#' lmerMod_3
#'
#' Model of class lmerMod
#' @examples
#' lme4::lmer(wt ~ drat * cyl + (1 | gear), data = mtcars)
"lmerMod_3"


#' lmerMod_4
#'
#' Model of class lmerMod
#' @examples
#' lme4::lmer(wt ~ drat/cyl + (1 | gear), data = mtcars)
"lmerMod_4"


#' lmerMod_5
#'
#' Model of class lmerMod
#' @examples
#' data <- iris
#' data$Cat1 <- rep(c("X", "X", "Y"), length.out = nrow(data))
#' data$Cat2 <- rep(c("A", "B"), length.out = nrow(data))
#' lme4::lmer(Petal.Width ~ Cat1 + (1 + Cat1 | Species), data = data)
"lmerMod_5"




#' merMod_0
#'
#' Model of class merMod
#' @examples
#' lme4::glmer(vs ~ 1 + (1 | gear), data = mtcars, family = "binomial")
"merMod_0"

#' merMod_1
#'
#' Model of class merMod
#' @examples
#' lme4::glmer(vs ~ cyl + (1 | gear), data = mtcars, family = "binomial")
"merMod_1"


#' merMod_2
#'
#' Model of class merMod
#' @examples
#' lme4::glmer(vs ~ drat + cyl + (1 | gear), data = mtcars, family = "binomial")
"merMod_2"


#' merMod_3
#'
#' Model of class merMod
#' @examples
#' lme4::glmer(vs ~ drat * cyl + (1 | gear), data = mtcars, family = "binomial")
"merMod_3"


#' merMod_4
#'
#' Model of class merMod
#' @examples
#' lme4::glmer(vs ~ cyl + (1 | gear), data = mtcars, family = binomial(link = "probit"))
"merMod_4"



#' merMod_5
#'
#' Model of class glmerMod
#' @examples
#' \dontrun{
#' # for data, see '?glmer.nb'
#' lme4::glmer.nb(y ~ f1 * f2 + (1 | g), data = dd)
#' }
"merMod_5"



#' merMod_nb_1
#'
#' Model of class glmerMod
#' @examples
#' library(lme4)
#' library(glmmTMB)
#' data(Owls)
#' glmer.nb(SiblingNegotiation ~ SexParent + ArrivalTime + (1 | Nest), data = Owls)
"merMod_nb_1"






#' anova_lmerMod_0
#'
#' Model of class anova of merMod
#' @examples
#' anova(lmer(wt ~ 1 + (1 | gear), data = mtcars))
"anova_lmerMod_0"


#' anova_lmerMod_1
#'
#' Model of class anova of merMod
#' @examples
#' anova(lmer(wt ~ cyl + (1 | gear), data = mtcars))
"anova_lmerMod_1"

#' anova_lmerMod_2
#'
#' Model of class anova of merMod
#' @examples
#' anova(lmer(wt ~ drat + cyl + (1 | gear), data = mtcars))
"anova_lmerMod_2"

#' anova_lmerMod_3
#'
#' Model of class anova of merMod
#' @examples
#' anova(lmer(wt ~ drat * cyl + (1 | gear), data = mtcars))
"anova_lmerMod_3"

#' anova_lmerMod_4
#'
#' Model of class anova of merMod
#' @examples
#' anova(lmer(wt ~ drat/cyl + (1 | gear), data = mtcars))
"anova_lmerMod_4"

#' anova_lmerMod_5
#'
#' Model of class anova of merMod
#' @examples
#' anova(lmerMod_5)
"anova_lmerMod_5"


#' anova_lmerMod_6
#'
#' Model of class anova of merMod
#' @examples
#' anova(lmerMod_0, lmerMod_1, lmerMod_2)
"anova_lmerMod_6"











#' glmmTMB_spatial_1
#'
#' Model of class glmmTMB
#' @examples
#' \dontrun{
#' library(geoR)
#' data(ca20)
#' dat <- data.frame(
#'   x = ca20$coords[, 1],
#'   y = ca20$coords[, 2],
#'   calcium = ca20$data,
#'   elevation = ca20$covariate[, 1],
#'   region = factor(ca20$covariate[, 2])
#' )
#' dat$pos <- numFactor(scale(dat$x), scale(dat$y))
#' dat$ID <- factor(rep(1, nrow(dat)))
#'
#' glmmTMB(calcium ~ elevation + region + mat(pos + 0 | ID), data = dat)
#' }
"glmmTMB_spatial_1"



#' glmmTMB_1
#'
#' Model of class glmmTMB
#' @examples
#' \dontrun{
#' set.seed(123)
#' fish <- read.csv("https://stats.idre.ucla.edu/stat/data/fish.csv")
#' fish$nofish <- as.factor(fish$nofish)
#' fish$livebait <- as.factor(fish$livebait)
#' fish$camper <- as.factor(fish$camper)
#' fish$ID <- sample(1:4, nrow(fish), replace = TRUE)
#'
#' glmmTMB(
#'   count ~ child + camper + (1 | persons),
#'   data = fish,
#'   family = poisson()
#' )
#' }
"glmmTMB_1"


#' glmmTMB_2
#'
#' Model of class glmmTMB
#' @examples
#' \dontrun{
#' glmmTMB(count ~ mined + (1 | site),
#'   ziformula =  ~ mined,
#'   family = poisson,
#'   data = Salamanders
#' )
#' }
"glmmTMB_2"


#' glmmTMB_3
#'
#' Model of class glmmTMB
#' @examples
#' \dontrun{
#' glmmTMB(
#'   count ~ spp + mined + (1 | site),
#'   ziformula =  ~ spp + mined,
#'   family = nbinom2,
#'   data = Salamanders
#' )
#' }
"glmmTMB_3"


#' glmmTMB_4
#'
#' Model of class glmmTMB
#' @examples
#' \dontrun{
#' glmmTMB(
#'   count ~ spp + mined + (1 | site),
#'   ziformula =  ~ spp + mined,
#'   family = truncated_poisson,
#'   data = Salamanders
#' )
#' }
"glmmTMB_4"


#' glmmTMB_5
#'
#' Model of class glmmTMB
#' @examples
#' \dontrun{
#' data(cbpp, package = "lme4")
#' glmmTMB(
#'   cbind(incidence, size - incidence) ~ period + (1 | herd),
#'   data = cbpp,
#'   family = binomial
#' )
#' }
"glmmTMB_5"


#' glmmTMB_zi_1
#'
#' Model of class glmmTMB
#' @examples
#' \dontrun{
#' # data prepararion: see "glmmTMB_1"
#'
#' glmmTMB(
#'   count ~ child + camper + (1 | persons),
#'   ziformula = ~ child + camper + (1 | persons),
#'   data = fish,
#'   family = truncated_poisson()
#' )
#' }
"glmmTMB_zi_1"


#' glmmTMB_zi_2
#'
#' Model of class glmmTMB
#' @examples
#' \dontrun{
#' # data prepararion: see "glmmTMB_1"
#'
#' glmmTMB(
#'   count ~ child + camper + (1 | persons),
#'   ziformula = ~ child + livebait + (1 | persons),
#'   data = fish,
#'   family = poisson()
#' )
#' }
"glmmTMB_zi_2"


#' glmmTMB_zi_3
#'
#' Model of class glmmTMB
#' @examples
#' \dontrun{
#' # data prepararion: see "glmmTMB_1"
#'
#' glmmTMB(
#'   count ~ child + camper + (1 | persons),
#'   ziformula = ~ child + livebait + (1 | persons),
#'   dispformula = ~xb,
#'   data = fish,
#'   family = truncated_poisson()
#' )
#' }
"glmmTMB_zi_3"



#' glmmTMB_zi_4
#'
#' Model of class glmmTMB
#' @examples
#' \dontrun{
#' # data prepararion: see "glmmTMB_1"
#'
#' glmmTMB(
#'   count ~ child + camper + (1 + zg | persons) + (1 | nofish),
#'   ziformula = ~ child + livebait + (1 | persons),
#'   data = fish,
#'   family = truncated_poisson()
#' )
#' }
"glmmTMB_zi_4"



#' glmmTMB_zi_5
#'
#' Model of class glmmTMB
#' @examples
#' \dontrun{
#' # data prepararion: see "glmmTMB_1"
#'
#' glmmTMB(
#'   count ~ child + camper + (1 | persons)
#'   ziformula = ~ child + livebait + (1 | ID),
#'   data = fish,
#'   family = truncated_poisson()
#' )
#' }
"glmmTMB_zi_5"



#' glmmTMB_zi_6
#'
#' Model of class glmmTMB
#' @examples
#' \dontrun{
#' # data prepararion: see "glmmTMB_1"
#'
#' glmmTMB(
#'   count ~ child + camper + (1 + xb | persons),
#'   ziformula = ~ child + livebait + (1 + zg + nofish | ID),
#'   dispformula = ~xb,
#'   data = fish,
#'   family = truncated_poisson()
#' )
#' }
"glmmTMB_zi_6"






#' GLMMadaptive_1
#'
#' Model of class GLMMadaptive
#' @examples
#' \dontrun{
#' data(cbpp, package = "lme4")
#'
#' GLMMadaptive::mixed_model(
#'   cbind(incidence, size - incidence) ~ period,
#'   random = ~ 1 | herd,
#'   data = cbpp,
#'   family = binomial
#' )
#' }
"GLMMadaptive_1"


#' GLMMadaptive_zi_1
#'
#' Model of class GLMMadaptive
#' @examples
#' \dontrun{
#' # data prepararion: see "glmmTMB_1"
#'
#' GLMMadaptive::mixed_model(
#'   count ~ child + camper,
#'   random = ~ 1 | persons,
#'   zi_fixed = ~ child + livebait,
#'   data = fish,
#'   family = GLMMadaptive::zi.poisson()
#' )
#' }
"GLMMadaptive_zi_1"


#' GLMMadaptive_zi_2
#'
#' Model of class GLMMadaptive
#' @examples
#' \dontrun{
#' # data prepararion: see "glmmTMB_1"
#'
#' GLMMadaptive::mixed_model(
#'   count ~ child + camper,
#'   random = ~ 1 | persons,
#'   zi_fixed = ~ child + livebait,
#'   zi_random = ~ 1 | persons,
#'   data = fish,
#'   family = GLMMadaptive::zi.poisson()
#' )
#' }
"GLMMadaptive_zi_2"















#' stanreg_bernoulli_1
#'
#' Model of class stanreg
#' @examples
#' \dontrun{
#' set.seed(1234)
#' dat <- data.frame(
#'   outcome = rbinom(n = 100, size = 1, prob = 0.35),
#'   var_binom = as.factor(rbinom(n = 100, size = 1, prob = 0.3)),
#'   var_cont = rnorm(n = 100, mean = 10, sd = 7),
#'   groups = sample(letters[1:2], size = 100, replace = TRUE)
#' )
#' set.seed(1234)
#' stan_glm(outcome ~ var_binom * groups + var_cont, data = dat, family = binomial())
#' }
"stanreg_bernoulli_1"



#' stanreg_lm_0
#'
#' Model of class stanreg
#' @examples
#' \dontrun{
#' rstanarm::stan_glm(mpg ~ 1, data = mtcars)
#' }
"stanreg_lm_0"


#' stanreg_lm_1
#'
#' Model of class stanreg
#' @examples
#' \dontrun{
#' rstanarm::stan_glm(mpg ~ wt, data = mtcars)
#' }
"stanreg_lm_1"


#' stanreg_lm_2
#'
#' Model of class stanreg
#' @examples
#' \dontrun{
#' rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
#' }
"stanreg_lm_2"

#' stanreg_lm_3
#'
#' Model of class stanreg
#' @examples
#' \dontrun{
#' rstanarm::stan_glm(mpg ~ wt * cyl, data = mtcars)
#' }
"stanreg_lm_3"

#' stanreg_lm_4
#'
#' Model of class stanreg
#' @examples
#' \dontrun{
#' rstanarm::stan_glm(mpg ~ wt + poly(cyl, 2), data = mtcars)
#' }
"stanreg_lm_4"

#' stanreg_lm_5
#'
#' Model of class stanreg
#' @examples
#' \dontrun{
#' rstanarm::stan_glm(mpg ~ wt + poly(cyl, 2, raw = TRUE), data = mtcars)
#' }
"stanreg_lm_5"

#' stanreg_lm_6
#'
#' Model of class stanreg
#' @examples
#' \dontrun{
#' rstanarm::stan_glm(mpg ~ wt * as.factor(gear), data = mtcars)
#' }
"stanreg_lm_6"

#' stanreg_lm_7
#'
#' Model of class stanreg
#' @examples
#' \dontrun{
#' rstanarm::stan_glm(mpg ~ as.factor(gear)/wt, data = mtcars)
#' }
"stanreg_lm_7"



#' stanreg_glm_0
#'
#' Model of class stanreg
#' @examples
#' \dontrun{
#' rstanarm::stan_glm(vs ~ 1, data = mtcars, family = "binomial")
#' }
"stanreg_glm_0"

#' stanreg_glm_1
#'
#' Model of class stanreg
#' @examples
#' \dontrun{
#' rstanarm::stan_glm(vs ~ wt, data = mtcars, family = "binomial")
#' }
"stanreg_glm_1"

#' stanreg_glm_2
#'
#' Model of class stanreg
#' @examples
#' \dontrun{
#' rstanarm::stan_glm(vs ~ wt + cyl, data = mtcars, family = "binomial")
#' }
"stanreg_glm_2"

#' stanreg_glm_3
#'
#' Model of class stanreg
#' @examples
#' \dontrun{
#' rstanarm::stan_glm(vs ~ wt * cyl, data = mtcars, family = "binomial")
#' }
"stanreg_glm_3"

#' stanreg_glm_4
#'
#' Model of class stanreg
#' @examples
#' \dontrun{
#' rstanarm::stan_glm(vs ~ wt + cyl, data = mtcars, family = binomial(link = "probit"))
#' }
"stanreg_glm_4"

#' stanreg_glm_5
#'
#' Model of class stanreg
#' @examples
#' \dontrun{
#' set.seed(123)
#' mtcars$count <- rpois(nrow(mtcars), 2)
#' rstanarm::stan_glm(count ~ wt + cyl, data = mtcars, family = "poisson")
#' }
"stanreg_glm_5"

#' stanreg_glm_6
#'
#' Model of class stanreg
#' @examples
#' \dontrun{
#' set.seed(123)
#' rstanarm::stan_glm(Sepal.Width ~ Species * Petal.Length, data = iris)
#' }
"stanreg_glm_6"






#' stanreg_lmerMod_0
#'
#' Model of class stanreg
#' @examples
#' \dontrun{
#' rstanarm::stan_lmer(wt ~ 1 + (1 | gear), data = mtcars)
#' }
"stanreg_lmerMod_0"

#' stanreg_lmerMod_1
#'
#' Model of class stanreg
#' @examples
#' \dontrun{
#' rstanarm::stan_lmer(wt ~ cyl + (1 | gear), data = mtcars)
#' }
"stanreg_lmerMod_1"

#' stanreg_lmerMod_2
#'
#' Model of class stanreg
#' @examples
#' \dontrun{
#' rstanarm::stan_lmer(wt ~ drat + cyl + (1 | gear), data = mtcars)
#' }
"stanreg_lmerMod_2"

#' stanreg_lmerMod_3
#'
#' Model of class stanreg
#' @examples
#' \dontrun{
#' rstanarm::stan_lmer(wt ~ drat * cyl + (1 | gear), data = mtcars)
#' }
"stanreg_lmerMod_3"

#' stanreg_lmerMod_4
#'
#' Model of class stanreg
#' @examples
#' \dontrun{
#' rstanarm::stan_lmer(wt ~ drat / cyl + (1 | gear), data = mtcars)
#' }
"stanreg_lmerMod_4"

#' stanreg_lmerMod_5
#'
#' Model of class stanreg
#' @examples
#' \dontrun{
#' dat <- iris
#' dat$Cat1 <- rep(c("X", "X", "Y"), length.out = nrow(dat))
#' dat$Cat2 <- rep(c("A", "B"), length.out = nrow(dat))
#' rstanarm::stan_lmer(Petal.Width ~ Cat1 + (1 + Cat1 | Species), data = dat)
#' }
"stanreg_lmerMod_5"




#' stanreg_merMod_0
#'
#' Model of class stanreg
#' @examples
#' \dontrun{
#' stan_glmer(vs ~ 1 + (1 | gear), data = mtcars, family = "binomial")
#' }
"stanreg_merMod_0"


#' stanreg_merMod_1
#'
#' Model of class stanreg
#' @examples
#' \dontrun{
#' stan_glmer(vs ~ cyl + (1 | gear), data = mtcars, family = "binomial")
#' }
"stanreg_merMod_1"

#' stanreg_merMod_2
#'
#' Model of class stanreg
#' @examples
#' \dontrun{
#' stan_glmer(vs ~ drat + cyl + (1 | gear), data = mtcars, family = "binomial")
#' }
"stanreg_merMod_2"

#' stanreg_merMod_3
#'
#' Model of class stanreg
#' @examples
#' \dontrun{
#' stan_glmer(vs ~ drat * cyl + (1 | gear), data = mtcars, family = "binomial")
#' }
"stanreg_merMod_3"

#' stanreg_merMod_4
#'
#' Model of class stanreg
#' @examples
#' \dontrun{
#' stan_glmer(
#'   vs ~ cyl + (1 | gear),
#'   data = mtcars,
#'   family = binomial(link = "probit")
#' )
#' }
"stanreg_merMod_4"

#' stanreg_merMod_5
#'
#' Model of class stanreg
#' @examples
#' \dontrun{
#' stan_glmer(
#'   cbind(incidence, size - incidence) ~ size + period + (1 | herd),
#'   data = lme4::cbpp, family = binomial, QR = TRUE,
#'   chains = 2, cores = 1, seed = 12345, iter = 500, refresh = 0
#' )
#' }
"stanreg_merMod_5"

#' stanreg_merMod_6
#'
#' Model of class stanreg
#' @examples
#' \dontrun{
#' stan_glmer(
#'   Reaction ~ Days + (1 + Days | Subject),
#'   data = lme4::sleepstudy, chains = 2, cores = 1
#'   seed = 12345, iter = 500, refresh = 0
#' )
#' }
"stanreg_merMod_6"





#' stanreg_meanfield_lm_1
#'
#' Model of class stanreg
#' @examples
#' \dontrun{
#' update(stanreg_lm_1, algorithm = "meanfield")
#' }
"stanreg_meanfield_lm_1"

#' stanreg_fullrank_lm_1
#'
#' Model of class stanreg
#' @examples
#' \dontrun{
#' update(stanreg_lm_1, algorithm = "fullrank")
#' }
"stanreg_fullrank_lm_1"


#' stanreg_gamm4_1
#'
#' Model of class stanreg
#' @examples
#' \dontrun{
#' stan_gamm4(Sepal.Width ~ s(Petal.Length), data = iris)
#' }
"stanreg_gamm4_1"

#' stanreg_gamm4_2
#'
#' Model of class stanreg
#' @examples
#' \dontrun{
#' stan_gamm4(Sepal.Width ~ Species + s(Petal.Length), data = iris)
#' }
"stanreg_gamm4_2"

#' stanreg_gamm4_3
#'
#' Model of class stanreg
#' @examples
#' \dontrun{
#' data <- iris
#' data$Cat1 <- rep(c("X", "X", "Y"), length.out = nrow(data))
#'
#' stan_gamm4(
#'   Sepal.Width ~ Species + s(Petal.Length),
#'   random = ~(1 | Cat1),
#'   data = data
#' )
#' }
"stanreg_gamm4_3"










#' brms_1
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' set.seed(333)
#' brms::brm(mpg ~ wt + cyl, data = mtcars)
#' }
"brms_1"

#' brms_2
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' dat <- read.table(header = TRUE, text = "
#'   n r r/n group treat c2 c1 w
#'   62 3 0.048387097 1 0 0.1438 1.941115288 1.941115288
#'   96 1 0.010416667 1 0 0.237 1.186583128 1.186583128
#'   17 0 0 0 0 0.2774 1.159882668 3.159882668
#'   41 2 0.048780488 1 0 0.2774 1.159882668 3.159882668
#'   212 170 0.801886792 0 0 0.2093 1.133397521 1.133397521
#'   143 21 0.146853147 1 1 0.1206 1.128993008 1.128993008
#'   143 0 0 1 1 0.1707 1.128993008 2.128993008
#'   143 33 0.230769231 0 1 0.0699 1.128993008 1.128993008
#'   73 62 1.260273973 0 1 0.1351 1.121927228 1.121927228
#'   73 17 0.232876712 0 1 0.1206 1.121927228 1.121927228")
#' dat$treat <- as.factor(dat$treat)
#'
#' brms::brm(
#'   r | trials(n) ~ treat * c2,
#'   data = dat,
#'   family = binomial(link = logit),
#'   chains = 1, iter = 500
#')}
"brms_2"

#' brms_3
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' set.seed(123)
#' x <- sample(c(1:3), 1000, replace = TRUE)
#' w <- runif(1000, min = 0.3, max = 3)
#' y <- rnorm(1000, mean = 0, sd = 1)
#' data <- data.frame(x, w, y)
#' data$x <- factor(data$x)
#'
#' brm(x | weights(w) ~ y, data = data, family = categorical)}
"brms_3"


#' brms_4
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' brm(
#'   Sepal.Length ~ Sepal.Width * Species,
#'   data = iris,
#'   family = gaussian(),
#'   chains = 2, iter = 2000, warmup = 1000, cores = 2
#' )}
"brms_4"


#' brms_linear_1
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' data(efc, package = "ggeffects")
#' efc <- datawizard::to_factor(efc, c("c161sex", "c172code", "e16sex", "e42dep"))
#' levels(efc$c172code) <- c("low", "mid", "high")
#' brm(barthtot ~ e16sex + c161sex + c172code * c160age + c12hour + e42dep, data = efc)
#' }
"brms_linear_1"


#' brms_logistic_1
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' data(efc, package = "ggeffects")
#' efc <- datawizard::to_factor(efc, c("c161sex", "c172code", "e16sex", "e42dep"))
#' levels(efc$c172code) <- c("low", "mid", "high")
#' brm(barthtot ~ e16sex + c161sex + c172code * c160age + c12hour + e42dep, data = efc)
#' }
"brms_logistic_1"


#' brms_null_1
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' set.seed(333)
#' brms::brm(mpg ~ 1, data = mtcars)
#' }
"brms_null_1"

#' brms_null_2
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' set.seed(333)
#' brms::brm(mpg ~ 1 + (1 | cyl), data = mtcars)
#' }
"brms_null_2"


#' brms_mixed_1
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' set.seed(123)
#' brms::brm(mpg ~ wt + (1 | cyl) + (1 + wt | gear), data = mtcars)
#' }
"brms_mixed_1"

#' brms_mixed_2
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' set.seed(123)
#' brms::brm(Reaction ~ Days + (1 + Days | Subject), data = sleepstudy)
#' }
"brms_mixed_2"

#' brms_mixed_3
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' set.seed(123)
#' sleepstudy$grp <- sample(1:5, size = 180, replace = TRUE)
#' sleepstudy <- sleepstudy %>%
#'   group_by(grp) %>%
#'   mutate(subgrp = sample(1:15, size = n(), replace = TRUE))
#'
#' brms::brm(Reaction ~ Days + (1 | grp / subgrp) + (1 | Subject), data = sleepstudy)
#' }
"brms_mixed_3"

#' brms_mixed_4
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' set.seed(333)
#' brms::brm(Petal.Length ~ Petal.Width + (1 | Species), data = iris)
#' }
"brms_mixed_4"

#' brms_mixed_5
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' library(lme4)
#' library(brms)
#' data(sleepstudy)
#' set.seed(123)
#' sleepstudy$cat <- as.factor(sample(1:5, size = 180, replace = TRUE))
#' sleepstudy$Reaction_d <-
#'   ifelse(sleepstudy$Reaction < median(sleepstudy$Reaction), 0, 1)
#'
#' brms::brm(
#'   Reaction_d ~ Days + cat + (1 | Subject),
#'   data = sleepstudy,
#'   family = bernoulli()
#' )
#' }
"brms_mixed_5"

#' brms_mixed_6
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' bprior1 <- prior(student_t(5, 0, 10), class = b) + prior(cauchy(0, 2), class = sd)
#' brms::brm(
#'   count ~ Age + Base * Trt + (1 | patient),
#'   data = epilepsy,
#'   family = poisson(),
#'   prior = bprior1,
#'   chains = 1,
#'   iter = 500
#' )
#' }
"brms_mixed_6"

#' brms_mixed_7
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' brm(mpg ~ wt + (1 | cyl) + (1 + wt | gear), data = mtcars)
#' }
"brms_mixed_7"

#' brms_mixed_8
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' bprior1 <- prior(student_t(5, 0, 10), class = b) + prior(cauchy(0, 2), class = sd)
#' brms::brm(
#'   count ~ Age + Base * Trt + (1 + Age | patient),
#'   data = epilepsy,
#'   family = poisson(),
#'   prior = bprior1,
#'   chains = 1,
#'   iter = 500
#' )
#' }
"brms_mixed_8"



#' brms_mixed_9
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' brms::brm(mpg ~ hp + (1 | cyl), data = mtcars, seed = 123)
#' }
"brms_mixed_9"




#' brms_mixed_10
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' data(iris)
#' iris$Group <- as.factor(rep(c("G1", "G2", "G3"), each = 50))
#' set.seed(123)
#' brms::brm(
#'   Sepal.Width ~ Petal.Width + (Petal.Width | Group),
#'   data = iris,
#'   refresh = 0
#' )
#' }
"brms_mixed_10"





#' brms_sigma_1
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' set.seed(123)
#' brm(bf(mpg ~ hp + (1 | cyl), sigma ~ cyl), data = mtcars, seed = 123)
#' }
"brms_sigma_1"




#' brms_sigma_2
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' set.seed(123)
#' d <- data.frame(
#'   Condition = rep(c("A", "B", "C"), each=30),
#'   Participant = as.factor(rep_len(paste0("S", 1:5), 90)),
#'   Response = rnorm(90)
#' )
#' brm(
#'   bf(
#'     Response ~ Condition + (Condition | Participant),
#'     sigma ~ Condition + (Condition | Participant),
#'     family = exgaussian()
#'   ),
#'   data = d,
#'   iter = 2000,
#'   chains = 4,
#'   cores = 4,
#'   backend = "cmdstanr"
#' )
#' }
"brms_sigma_2"




#' brms_sigma_3
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' set.seed(123)
#' data(iris)
#' iris$Group <- as.factor(rep(c("G1", "G2", "G3"), each = 50))
#' brms_sigma_3 <- brms::brm(
#'   brms::bf(
#'     Sepal.Width ~ Petal.Width + (Petal.Width | Group),
#'     sigma ~ Petal.Width + (Petal.Width | Group)
#'   ),
#'   data = iris,
#'   refresh=0
#' )
#' }
"brms_sigma_3"





#' brms_mo1
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' income_options <- c("below_20", "20_to_40", "40_to_100", "greater_100")
#' income <- factor(
#'   sample(income_options, 100, TRUE),
#'   levels = income_options, ordered = TRUE
#' )
#' mean_ls <- c(30, 60, 70, 75)
#' ls <- mean_ls[income] + rnorm(100, sd = 7)
#' dat <- data.frame(income, ls) # fit a simple monotonic model
#' brms_mo1 <- brm(ls ~ mo(income), data = dat)
#' }
"brms_mo1"




#' brms_mo2
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' brm(mpg ~ mo(gear), data = mtcars, refresh = 0)
#' }
"brms_mo2"





#' brms_von_mises_1
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   y = runif(200, -pi, pi),
#'   x = as.factor(rep(c("a", "b"), 100))
#' )
#'
#' f <- brms::bf(
#'   y ~ 0 + Intercept + x,
#'   kappa ~ 0 + Intercept,
#'   family = brms::von_mises()
#' )
#'
#' brms::brm(
#'   f,
#'   data = df,
#'   refresh = 0,
#'   algorithm = "pathfinder",
#'   backend = "cmdstanr"
#' )
#' }
"brms_von_mises_1"





#' brms_meta_1
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' # Data from
#' # https://github.com/MathiasHarrer/Doing-Meta-Analysis-in-R/blob/master/_data/Meta_Analysis_Data.RData
#' set.seed(123)
#' priors <- c(prior(normal(0,1), class = Intercept),
#'             prior(cauchy(0,0.5), class = sd))
#'
#' brm(TE|se(seTE) ~ 1 + (1|Author),
#'    data = Meta_Analysis_Data,
#'    prior = priors,
#'    iter = 4000)
#' }
"brms_meta_1"






#' brms_mv_1
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' set.seed(123)
#' brms::brm(cbind(cyl, gear, carb) ~ wt + hp, data = mtcars)
#' }
"brms_mv_1"

#' brms_mv_2
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' set.seed(123)
#' f1 <- bf(mpg ~ wt + disp + cyl + hp + (1 |CAR| gear))
#' f2 <- bf(wt ~ disp + cyl + hp + (1 |CAR| gear))
#' brms::brm(f1 + f2 + set_rescor(FALSE), data = mtcars)
#' }
"brms_mv_2"

#' brms_mv_3
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' data(epilepsy)
#' set.seed(123)
#' epilepsy$visit <- as.numeric(epilepsy$visit)
#' epilepsy$Base2 <- sample(epilepsy$Base, nrow(epilepsy), replace = TRUE)
#' f1 <- bf(Base ~ zAge + count + (1 |ID| patient))
#' f2 <- bf(Base2 ~ zAge + Trt + (1 |ID| patient))
#' brms::brm(f1 + f2 + set_rescor(FALSE), data = epilepsy)
#' }
"brms_mv_3"

#' brms_mv_4
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' f1 <- bf(Sepal.Length ~ Petal.Length + Sepal.Width + Species)
#' f2 <- bf(Sepal.Width ~ Species)
#' brms::brm(f1 + f2 + set_rescor(FALSE), data = iris, chains = 1, iter = 500)
#' }
"brms_mv_4"

#' brms_mv_5
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' bf1 <- bf(count ~ child + camper + (1 | persons), zi ~ camper + (1 | persons))
#' bf2 <- bf(count2 ~ child + livebait + (1 | persons), zi ~ child + (1 | persons))
#' brms::brm(bf1 + bf2, data = zinb, family = zero_inflated_poisson(), chains = 1, iter = 500)
#' }
"brms_mv_5"


#' brms_mv_6
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' data(jobs, package = "mediation")
#' f1 <- bf(job_seek ~ treat + econ_hard + sex + age)
#' f2 <- bf(depress2 ~ treat + job_seek + econ_hard + sex + age)
#' brm(f1 + f2 + set_rescor(FALSE), data = jobs)
#' }
"brms_mv_6"



#' brms_miss_1
#'
#' @examples
#' \dontrun{
#' dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/carData/TitanicSurvival.csv")
#' dat$survived <- ifelse(dat$survived == "yes", 1, 0)
#' dat$woman <- ifelse(dat$sex == "female", 1, 0) |> as.factor()
#' sum(is.na(dat$age))  # <-- 263 missing values
#'
#' brm(
#'   bf(
#'     survived ~ woman * mi(age) + passengerClass,
#'     family = bernoulli(link = "logit")
#'   ) +
#'     bf(age | mi() ~ passengerClass + woman) +
#'     set_rescor(FALSE),
#'   data = dat,
#'   backend = "cmdstanr",
#'   cores = 4
#' )
#' }
"brms_miss_1"




#' brms_beta_1
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' data(FoodExpenditure, package = "betareg")
#' m <- brm(
#'   I(food / income) ~ income + (1 | persons),
#'   data = FoodExpenditure,
#'   family = Beta()
#' )
#' }
"brms_beta_1"




#' brms_zi_1
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' zinb <- read.csv("http://stats.idre.ucla.edu/stat/data/fish.csv")
#' set.seed(123)
#' brms::brm(bf(
#'     count ~ persons + child + camper,
#'     zi ~ child + camper
#'   ),
#'   data = zinb,
#'   family = zero_inflated_poisson()
#' )
#' }
"brms_zi_1"

#' brms_zi_2
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' zinb <- read.csv("http://stats.idre.ucla.edu/stat/data/fish.csv")
#' set.seed(123)
#' brms::brm(bf(
#'     count ~ persons + child + camper + (1 | persons),
#'     zi ~ child + camper + (1 | persons)
#'   ),
#'   data = zinb,
#'   family = zero_inflated_poisson()
#' )
#' }
"brms_zi_2"

#' brms_zi_3
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' brms::brm(
#'   bf(count ~ child + camper + (1 | persons), zi ~ child + camper + (1 | persons)),
#'   data = zinb,
#'   family = zero_inflated_poisson(),
#'   chains = 1,
#'   iter = 500
#' )
#' }
"brms_zi_3"

#' brms_zi_4
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' brms::brm(
#'   bf(
#'     count ~ child + camper + (1 + xb | persons) + (1 + zg | ID),
#'     zi ~ child + livebait + (1 + zg + nofish | ID)
#'   ),
#'   data = zinb,
#'   family = zero_inflated_poisson(),
#'   chains = 1,
#'   iter = 500
#' )
#' }
"brms_zi_4"



#' brms_ipw_1
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' brms::brm(
#'   QoL | weights(ipw) ~ treatment * time + treatment * education +
#'     hospital + age + phq4 + (1 | ID) ,
#'   data = datawizard::qolcancer
#' )
#' }
"brms_ipw_1"




#' brms_lf_1
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' brms::brm(
#'   bf(carb ~ gear * vs) + lf(disc ~ 0 + mo(cyl)),
#'   data = mtcars,
#'   family = cumulative("probit")
#' )
#' }
"brms_lf_1"



#' brms_ordinal_1
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' set.seed(123)
#' data(mtcars)
#' mtcars$cyl_ord <- as.ordered(mtcars$cyl)
#' brms::brm(cyl_ord ~ mpg, data = mtcars, family = cumulative())
#' }
"brms_ordinal_1"

#' brms_ordinal_1_wt
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' set.seed(123)
#' data(mtcars)
#' mtcars$cyl_ord <- as.ordered(mtcars$cyl)
#' brms::brm(cyl_ord | weights(wt) ~ mpg, data = mtcars, family = cumulative())
#' }
"brms_ordinal_1_wt"


#' brms_ordinal_2
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' data(inhaler)
#' brm(
#'   rating ~ treat + period + carry + (1 | subject),
#'   family = cumulative(),
#'   data = inhaler
#' )
#' }
"brms_ordinal_2"


#' brms_categorical_1_num
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' set.seed(123)
#' data(mtcars)
#' brms::brm(gear ~ mpg, data = mtcars, family = categorical())
#' }
"brms_categorical_1_num"


#' brms_categorical_2_num
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' set.seed(123)
#' brm(
#'   Species ~ Sepal.Width,
#'   data = iris,
#'   family = categorical(refcat = "setosa"),
#'   refresh = 0,
#'   backend = "cmdstanr",
#'   algorithm = "pathfinder"
#' )
#' }
"brms_categorical_2_num"


#' brms_categorical_1_fct
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' set.seed(123)
#' data(mtcars)
#' mtcars$gear_fct <- factor(mtcars$gear)
#' brms::brm(gear_fct ~ mpg, data = mtcars, family = categorical())
#' }
"brms_categorical_1_fct"


#' brms_categorical_1_wt
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' set.seed(123)
#' data(mtcars)
#' brms::brm(gear | weights(wt) ~ mpg, data = mtcars, family = categorical())
#' }
"brms_categorical_1_wt"


#' brms_smooth_1
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' dat <- mgcv::gamSim(1, n = 200, scale = 2)
#' brm(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = dat, iter = 1000)
#' }
"brms_smooth_1"


#' brms_smooth_2
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' dat <- mgcv::gamSim(1, n = 200, scale = 2)
#' brm(y ~ t2(x0, x1) + s(x2, by = x3), data = dat, iter = 1000)
#' }
"brms_smooth_2"



#' brms_mm_1
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' set.seed(123)
#' dat <- data.frame(
#'   sid = c(1:300),
#'   t1id = sample(200:215, 300, replace = TRUE),
#'   t2id = sample(200:215, 300, replace = TRUE),
#'   y = rnorm(300, mean = 85, sd = 8),
#'   x = rnorm(300, mean = 50, sd = 10)
#' )
#'
#' brm(y ~ x + (1 | mm(t1id, t2id)), data = dat)
#' }
"brms_mm_1"


#' brms_mm_2
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' set.seed(123)
#' dat <- data.frame(
#'   y = rnorm(100), x1 = rnorm(100), x2 = rnorm(100),
#'   g1 = sample(1:10, 100, TRUE), g2 = sample(1:10, 100, TRUE)
#' )
#'
#' # multi-membership model with level specific covariate values
#' dat$xc <- (dat$x1 + dat$x2) / 2
#' brm(y ~ xc + (1 + mmc(x1, x2) | mm(g1, g2)), data = dat)
#' }
"brms_mm_2"


#' brms_mm_3
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' set.seed(123)
#' data(mzcars)
#' mtcars$w <- 1
#' brm(
#'   mpg ~ 1 + hp + (1 | cyl) +
#'     (1 | carb:am:hp) +
#'     (1 | mm(carb, am, weights = cbind(w, w), scale = FALSE)) +
#'     (1 | mm(carb:cyl, am:cyl, weights = cbind(w, w), scale = FALSE)),
#'   data = mtcars, family = gaussian, chains = 4,
#'   iter = 100, control = list(adapt_delta = 0.99, max_treedepth = 15),
#'   cores = 4, seed = 12345,
#'   backend = "cmdstanr"
#' )
#' }
"brms_mm_3"





#' brms_bernoulli_1
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' set.seed(1234)
#' dat <- data.frame(
#'   outcome = rbinom(n = 100, size = 1, prob = 0.35),
#'   var_binom = as.factor(rbinom(n = 100, size = 1, prob = 0.3)),
#'   var_cont = rnorm(n = 100, mean = 10, sd = 7),
#'   groups = sample(letters[1:2], size = 100, replace = TRUE)
#' )
#' set.seed(1234)
#' brm(outcome ~ var_binom * groups + var_cont, data = dat, family = bernoulli())
#' }
"brms_bernoulli_1"





#' brms_bf_1
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' brm(
#'   mpg ~ wt,
#'   data = mtcars,
#'   prior = set_prior("normal(0, 1)", class = "b"),
#'   refresh = 0,
#'   iter = 200,
#'   chains = 2
#' )
#' }
"brms_bf_1"


#' brms_mm_2
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' df <- data.frame(x = factor(rep(c('A', 'B'), each = 100)),
#'                  y = c(rnorm(100, mean = 0, sd = 1),
#'                        rnorm(100, mean = 10, sd = 5)),
#'                  id = factor(rep(1:100, 2)))
#'
#' brm(
#'   bf(y ~ x + (1 | i | id), sigma ~ x + (1 | i | id)),
#'   data = df,
#'   iter = 500,
#'   cores = 4
#' )
#' }
"brms_corr_re1"



#' brms_trunc_1
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' data(epilepsy)
#' brms_trunc_1 <- brm(
#'   count | trunc(ub = 104) ~ zBase * Trt,
#'   data = epilepsy,
#'   family = poisson(),
#'   backend = "cmdstanr",
#'   cores = 4
#' )}
"brms_trunc_1"


#' brms_aterm_1
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' brm(am | trials(1) ~ hp,
#'   data = mtcars, family = binomial(),
#'   chains = 2, iter = 200,
#'   backend = "cmdstanr", cores = 2
#' )}
"brms_aterm_1"


#' brms_aterm_2
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' brm(am | trials(cyl) ~ hp,
#'   data = mtcars, family = binomial(),
#'   chains = 2, iter = 200,
#'   backend = "cmdstanr", cores = 2
#' )}
"brms_aterm_2"


#' brms_aterm_3
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' data(kidney)
#' brm(time | cens(censored) ~ age * sex + disease + (1|patient),
#'     data = kidney, family = lognormal(),
#'     chains = 2, iter = 200,
#'     backend = "cmdstanr", cores = 2)}
"brms_aterm_3"


#' brms_aterm_4
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' data(kidney)
#' brm(brm(time | cens(censored, y2 = 3) ~ age * sex + disease + (1|patient),
#'     data = kidney, family = lognormal(),
#'     chains = 2, iter = 200,
#'     backend = "cmdstanr", cores = 2)}
"brms_aterm_4"



#' ordbetareg_1
#'
#' Model of class brmsfit
#' @examples
#' \dontrun{
#' library(ordbetareg)
#' data(sleepstudy, package = "lme4")
#' sleepstudy$y <- datawizard::normalize(sleepstudy$Reaction)
#' m <- ordbetareg(y ~ Days + (Days | Subject), data = sleepstudy)
#' }
"ordbetareg_1"


#' betareg_1
#'
#' Model of class betareg
#' @examples
#' betareg(yield ~ batch + temp, data = GasolineYield)
"betareg_1"

#' betareg_2
#'
#' Model of class betareg
#' @examples
#' betareg(I(food/income) ~ income + persons, data = FoodExpenditure)
"betareg_2"



#' censReg_1
#'
#' Model of class censReg
#' @examples
#' censReg(
#'   affairs ~ age + yearsmarried + religiousness + occupation + rating,
#'   data = Affairs
#' )
"censReg_1"


#' ivreg_1
#'
#' Model of class ivreg
#' @examples
#' data(CigarettesSW)
#' CigarettesSW$rprice <- with(CigarettesSW, price / cpi)
#' CigarettesSW$rincome <- with(CigarettesSW, income / population / cpi)
#' CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax) / cpi)
#'
#' ivreg_1 <- ivreg(
#'   log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff + I(tax / cpi),
#'   data = CigarettesSW,
#'   subset = year == "1995"
#' )
"ivreg_1"


#' clm_1
#'
#' Model of class clm
#' @examples
#' clm_1 <- clm(rating ~ temp * contact, data = wine)
"clm_1"

#' clm2_1
#'
#' Model of class clm2
#' @examples
#' clm2_1 <- clm2(rating ~ temp * contact, data = wine)
"clm2_1"


#' brms_4bf_1
#'
#' Example of class brmsfit with \code{save_all_pars = TRUE}.
#' @examples
#' \dontrun{
#' brm(Sepal.Length ~ 1, data = iris, save_all_pars = TRUE)
#' }
"brms_4bf_1"

#' brms_4bf_2
#'
#' Example of class brmsfit with \code{save_all_pars = TRUE}.
#' @examples
#' \dontrun{
#' brm(Sepal.Length ~ Species, data = iris, save_all_pars = TRUE)
#' }
"brms_4bf_2"

#' brms_4bf_3
#'
#' Example of class brmsfit with \code{save_all_pars = TRUE}.
#' @examples
#' \dontrun{
#' brm(Sepal.Length ~ Petal.Length, data = iris, save_all_pars = TRUE)
#' }
#'
"brms_4bf_3"

#' brms_4bf_4
#'
#' Example of class brmsfit with \code{save_all_pars = TRUE}.
#' @examples
#' \dontrun{
#' brm(Sepal.Length ~ Species + Petal.Length, data = iris, save_all_pars = TRUE)
#' }
#'
"brms_4bf_4"

#' brms_4bf_5
#'
#' Example of class brmsfit with \code{save_all_pars = TRUE}.
#' @examples
#' \dontrun{
#' brm(Sepal.Length ~ Species * Petal.Length, data = iris, save_all_pars = TRUE)
#' }
#'
"brms_4bf_5"



#' stanmvreg_1
#'
#' Model of class stanmvreg
#' @examples
#' \dontrun{
#' library(rstanarm)
#' stan_mvmer(
#'   formula = list(
#'     logBili ~ year + (1 | id),
#'     albumin ~ sex + year + (year | id)
#'   ),
#'   data = pbcLong,
#'   # this next line is only to keep the example small in size!
#'   chains = 1, cores = 1, seed = 12345, iter = 1000
#' )
#' }
"stanmvreg_1"



#' stanmvreg_2
#'
#' Model of class stanmvreg
#' @examples
#' \dontrun{
#' library(rstanarm)
#' library(mediation)
#' data(jobs)
#' stan_mvmer(
#'   list(job_seek ~ treat + econ_hard + sex + age + (1 | occp),
#'        depress2 ~ treat + job_seek + econ_hard + sex + age + (1 | occp)),
#'   data = jobs,
#'   cores = 4,
#'   seed = 1234,
#'   refresh = 0)
#' }
"stanmvreg_2"


#' illusiongame
#'
#' Data from Makowski et al., (2023) for the Illusion Game
#' @examples
#' \dontrun{
#' df <- read.csv("https://raw.githubusercontent.com/RealityBending/IllusionGameValidation/refs/heads/main/data/study1.csv")
#' df <- df[c("Participant", "Illusion_Type", "Trial", "RT", "Error", "Illusion_Strength", "Illusion_Difference")]
#' df$RT <- df$RT / 1000
#' df <- df[df$Illusion_Strength > 0, ]
#' df <- df[df$Illusion_Type %in% c("Müller-Lyer", "Delboeuf", "Ebbinghaus", "Vertical-Horizontal", "Ponzo"), ]
#' write.csv(df, "../data/illusiongame.csv", row.names = FALSE)
#' }
"illusiongame"
