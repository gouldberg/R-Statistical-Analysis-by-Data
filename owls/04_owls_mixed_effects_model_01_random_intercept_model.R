# setwd("//media//kswada//MyFiles//R//owls")
setwd("//media//kswada//MyFiles//R//Mixed_effects_and_multilevel_model//owls")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Owls
# ------------------------------------------------------------------------------

Owls <- read.table(file = "Owls.txt", header = TRUE)


str(Owls)

dim(Owls)

car::some(Owls)



# ------------------------------------------------------------------------------
# The Random Intercept Model by lme (linear mixed effects model)
#   - Note that the random intercept is also part of the 'choose a variance structure' process.
# ------------------------------------------------------------------------------

library(nlme)


Form <- formula(LogNeg ~ SexParent * FoodTreatment + SexParent * ArrivalTime)


# ~1 | Nest specifies a random intercept model.
# model is estimated with REML to apply the likelihood ratio test
M1.lme <- lme(Form, random = ~ 1 | Nest, method = "REML", data = Owls)



# ----------
summary(M1.lme)


# -->
# the residual variance is estimated as 0.232^2 = 0.0053 and
# and the variance for the random intercept is estimated as 0.093^2 = 0.008649.




# ----------
# Compare new model with linear regression
anova(M.gls, M1.lme)



# -->
# The likelihood ratio test indicates that the model with the random intercept is considerable better.
# Recall that we are testing on the boundary here.
# If we did the correction for testing on the boundary, the p-value would get even smaller.
# Because the random intercept is highly significant, testing on the boundary is not a problem here.

