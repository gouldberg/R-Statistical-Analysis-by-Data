setwd("//media//kswada//MyFiles//R//ozone")

packages <- c("dplyr", "faraway")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ozone
# ------------------------------------------------------------------------------

data(ozone, package="faraway")

str(ozone)



# ------------------------------------------------------------------------------
# Multivariate Adaptive Regression Splines (MARS)
# earth package which is built on the functionality in the original mda package
# We try Poisson GLM
# ------------------------------------------------------------------------------

library(earth)


# Poisson GAM and allow second-order (2-way) interaction terms
mmod_p <- earth(O3 ~ ., ozone, nk = 7, degree = 2, glm = list(family = quasipoisson(link = "log")))

summary(mmod_p)



# -->
# Only very slight improvement in terms of Rsq




# ----------
# Now let's see how the terms enter into the model.
plotmo(mmod_p)


# -->
# we see similar transformations but temperature is smoothly transformed



# ----------
# Now check diagnostics
par(mfrow=c(2,2))
plot(mmod_p)
plot(mmod_p, 3)



# -->
# No problem with normality, but some indication of nonconstant variance.



