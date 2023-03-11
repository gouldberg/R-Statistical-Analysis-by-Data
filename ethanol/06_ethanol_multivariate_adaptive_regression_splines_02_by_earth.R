setwd("//media//kswada//MyFiles//R//ethanol")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ethanol
# ------------------------------------------------------------------------------

data(ethanol, package="lattice")

str(ethanol)



# ------------------------------------------------------------------------------
# Multivariate Adaptive Regression Splines (MARS):  earth package which is built on the functionality in the original mda package
# ------------------------------------------------------------------------------

library(earth)


# The default choice allows only additive (first-order) predictors and chooses the model size using a GCV criterion.
mmod <- earth(NOx ~., ethanol)

summary(mmod)



# ----------
# The parameter nk controls the maximum number of model terms.
mmod <- earth(NOx ~., ethanol, nk = 7, degree = 2)

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





