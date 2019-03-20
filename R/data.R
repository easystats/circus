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
"anova_1"


#' aov_1
#'
#' Model of class aov
"aov_1"


#' aovlist_1
#'
#' Model of class aovlist
"aovlist_1"


#' anova_2
#'
#' Model of class anova
"anova_2"


#' aov_2
#'
#' Model of class aov
"aov_2"


#' aovlist_2
#'
#' Model of class aovlist
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










#' lmerMod_0
#'
#' Model of class lmerMod
"lmerMod_0"


#' lmerMod_1
#'
#' Model of class lmerMod
"lmerMod_1"


#' lmerMod_2
#'
#' Model of class lmerMod
"lmerMod_2"


#' lmerMod_3
#'
#' Model of class lmerMod
"lmerMod_3"


#' lmerMod_4
#'
#' Model of class lmerMod
"lmerMod_4"


#' lmerMod_5
#'
#' Model of class lmerMod
"lmerMod_5"




#' merMod_0
#'
#' Model of class merMod
"merMod_0"

#' merMod_1
#'
#' Model of class merMod
"merMod_1"


#' merMod_2
#'
#' Model of class merMod
"merMod_2"


#' merMod_3
#'
#' Model of class merMod
"merMod_3"


#' merMod_4
#'
#' Model of class merMod
"merMod_4"



#' anova_lmerMod_0
#'
#' Model of class anova of merMod
"anova_lmerMod_0"


#' anova_lmerMod_1
#'
#' Model of class anova of merMod
"anova_lmerMod_1"

#' anova_lmerMod_2
#'
#' Model of class anova of merMod
"anova_lmerMod_2"

#' anova_lmerMod_3
#'
#' Model of class anova of merMod
"anova_lmerMod_3"

#' anova_lmerMod_4
#'
#' Model of class anova of merMod
"anova_lmerMod_4"

#' anova_lmerMod_5
#'
#' Model of class anova of merMod
"anova_lmerMod_5"


#' anova_lmerMod_6
#'
#' Model of class anova of merMod
"anova_lmerMod_6"












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
"stanreg_glm_0"

#' stanreg_glm_1
#'
#' Model of class stanreg
"stanreg_glm_1"

#' stanreg_glm_2
#'
#' Model of class stanreg
"stanreg_glm_2"

#' stanreg_glm_3
#'
#' Model of class stanreg
"stanreg_glm_3"

#' stanreg_glm_4
#'
#' Model of class stanreg
"stanreg_glm_4"

#' stanreg_glm_5
#'
#' Model of class stanreg
"stanreg_glm_5"






#' stanreg_lmerMod_0
#'
#' Model of class stanreg
"stanreg_lmerMod_0"

#' stanreg_lmerMod_1
#'
#' Model of class stanreg
"stanreg_lmerMod_1"

#' stanreg_lmerMod_2
#'
#' Model of class stanreg
"stanreg_lmerMod_2"

#' stanreg_lmerMod_3
#'
#' Model of class stanreg
"stanreg_lmerMod_3"

#' stanreg_lmerMod_4
#'
#' Model of class stanreg
"stanreg_lmerMod_4"

#' stanreg_lmerMod_5
#'
#' Model of class stanreg
"stanreg_lmerMod_5"




#' stanreg_merMod_0
#'
#' Model of class stanreg
"stanreg_merMod_0"


#' stanreg_merMod_1
#'
#' Model of class stanreg
"stanreg_merMod_1"

#' stanreg_merMod_2
#'
#' Model of class stanreg
"stanreg_merMod_2"

#' stanreg_merMod_3
#'
#' Model of class stanreg
"stanreg_merMod_3"

#' stanreg_merMod_4
#'
#' Model of class stanreg
"stanreg_merMod_4"



#' stanreg_meanfield_lm_1
#'
#' Model of class stanreg
"stanreg_meanfield_lm_1"

#' stanreg_fullrank_lm_1
#'
#' Model of class stanreg
"stanreg_fullrank_lm_1"


#' stanreg_gamm4_1
#'
#' Model of class stanreg
"stanreg_gamm4_1"

#' stanreg_gamm4_2
#'
#' Model of class stanreg
"stanreg_gamm4_2"

#' stanreg_gamm4_3
#'
#' Model of class stanreg
"stanreg_gamm4_3"














#' brms_mixed_1
#'
#' Model of class brms
"brms_mixed_1"

#' brms_mixed_2
#'
#' Model of class brms
"brms_mixed_2"

#' brms_mixed_3
#'
#' Model of class brms
"brms_mixed_3"




#' brms_mv_1
#'
#' Model of class brms
"brms_mv_1"

#' brms_mv_2
#'
#' Model of class brms
"brms_mv_2"




#' brms_zi_1
#'
#' Model of class brms
"brms_zi_1"

#' brms_zi_2
#'
#' Model of class brms
"brms_zi_2"
