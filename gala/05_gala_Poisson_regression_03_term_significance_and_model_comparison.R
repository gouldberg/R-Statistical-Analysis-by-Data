setwd("//media//kswada//MyFiles//R//gala")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gala
# ------------------------------------------------------------------------------

data("gala", package = "faraway")

str(gala)

dim(gala)


car::some(gala)



# ------------------------------------------------------------------------------
# Term significance:  Wald test
# ------------------------------------------------------------------------------

# estimated coefficients (Wald test)
lmtest::coeftest(modp.step1)

lmtest::coeftest(modp.step2)

lmtest::coeftest(modp2)



# ------------------------------------------------------------------------------
# Term significance:  Type II tests of each effect
# ------------------------------------------------------------------------------

car::Anova(modp.step1)

car::Anova(modp.step2)

car::Anova(modp2)



# ------------------------------------------------------------------------------
# Model comparison (goodness of fit):  Likelihood ratio test
# ------------------------------------------------------------------------------

# Two models are nested. Likelihood ratio test can be applied

anova(modp.step2, modp, test="Chisq")


# -->
# Not significantly different



# ----------
anova(modp.step2, modp2, test="Chisq")
deviance(modp.step2);  deviance(modp2)


# -->
# Significantly different
# Deviance of modp.step2 is much smaller



# ----------
# Likelihood ratio tests of goodness of fit, together with AIC and BIC statistics
vcdExtra::LRstats(modp, modp.step2, modp2, sortby = "BIC")


# -->
# In both of AIC and BIC, modp.step2 is the best


