setwd("//media//kswada//MyFiles//R//icu")

packages <- c("dplyr", "vcd", "MASS", "vcdExtra")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Death in the ICU
# ------------------------------------------------------------------------------

data("ICU", package = "vcdExtra")

str(ICU)

dim(ICU)


car::some(ICU)



# ----------
# Here we do not apply "rage" and "coma"
# race, coma is represented initially as 3-level factors, but the recoded to binary variables (white, uncons)
# we apply binary variables

ICU2 <- ICU[, -c(4, 20)]

str(ICU2)



# ------------------------------------------------------------------------------
# Allow a nonlinear term (natural cubic splines)
# and all 2-way interactions of the binary predictors
# ------------------------------------------------------------------------------

summary(icu.step2)



# splines::ns (natural spline)
# This method constrains the fitted cubic spline to be linear at lower and upper limitso f x, and, for knots, fits df = k + 1 parameters not counting the intercept.
# The k knots can be conveniently chosen as k cutpoints in the percentiles of the distribution of x
# k = 3:  knots would be placed at the quartiles of the distribution
library(splines)

icu.glm3 <- update(icu.step2, . ~ . - age + ns(age, 3) + (cancer + admit + uncons)^2)

icu.glm4 <- update(icu.step2, . ~ . - age + age * (cancer + admit + uncons))



# ----------
lmtest::coeftest(icu.glm3)

lmtest::coeftest(icu.glm4)



# --> 
# None of these additional terms have effect



# ------------------------------------------------------------------------------
# Likelihood ratio tests of goodness of fit
# ------------------------------------------------------------------------------

vcdExtra::LRstats(icu.step1, icu.step2, icu.glm3, icu.glm4)


# -->
# Note that all 4 models are non-significant LR X^2 (residual deviance)


# ----------
anova(icu.step2, icu.glm3, test = "Chisq")

anova(icu.step2, icu.glm4, test = "Chisq")
