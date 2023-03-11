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
# Term significance:  Wald test
# ------------------------------------------------------------------------------

# estimated coefficients (Wald test)
lmtest::coeftest(icu.step1)

lmtest::coeftest(icu.step2)



# ------------------------------------------------------------------------------
# Term significance:  Type II tests of each effect
# ------------------------------------------------------------------------------
car::Anova(icu.step1)

car::Anova(icu.step2)




# ------------------------------------------------------------------------------
# Model comparison (goodness of fit):  Likelihood ratio test
# ------------------------------------------------------------------------------

# Two models are nested. Likelihood ratio test can be applied

anova(icu.step2, icu.step1, test="Chisq")


# -->
# larger model is significantly better


# ----------
# Likelihood ratio tests of goodness of fit, together with AIC and BIC statistics
vcdExtra::LRstats(icu.step2, icu.step1)


# -->
# by AIC icu.step1 is best, but by BIC icu.step2 is best
# Note that residual deviance LR Chisq for both models are not significant


