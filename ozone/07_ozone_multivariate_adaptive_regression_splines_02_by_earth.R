setwd("//media//kswada//MyFiles//R//ozone")

packages <- c("dplyr", "faraway")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ozone
# ------------------------------------------------------------------------------

data(ozone, package="faraway")

str(ozone)



# ------------------------------------------------------------------------------
# Multivariate Adaptive Regression Splines (MARS):  earth package which is built on the functionality in the original mda package
# ------------------------------------------------------------------------------

library(earth)


# The default choice allows only additive (first-order) predictors and chooses the model size using a GCV criterion.
mmod <- earth(O3 ~., ozone)

summary(mmod)



# ----------
# The parameter nk controls the maximum number of model terms.
mmod <- earth(O3 ~., ozone, nk = 7)

summary(mmod)



# ----------
# Allow second-order (2-way) interaction terms.
mmod <- earth(O3 ~ ., ozone, nk = 7, degree = 2)

summary(mmod)



# ----------
# Now let's see how the terms enter into the model.
plotmo(mmod)


# -->
# we see similar transformations to those used previously.



# ----------
# Now check diagnostics
par(mfrow=c(1,2))
plot(mmod, 3)
plot(mmod, 4)



# -->
# No problem with normality, but some indication of nonconstant variance.



