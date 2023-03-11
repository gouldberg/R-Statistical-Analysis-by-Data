setwd("//media//kswada//MyFiles//R//glasses")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  glasses
# ------------------------------------------------------------------------------
data("glasses", package = "gamlss.data")


str(glasses)

car::some(glasses)



# ------------------------------------------------------------------------------
# Fit two-component distribution models
# ------------------------------------------------------------------------------

library(gamlss.mx)

set.seed(3683)



# ------------------------------------------------------------------------------
# Refit simple logistic mixture model with K = 2 by gamlssMXfits
# ------------------------------------------------------------------------------

# Refit with n = 10
readLO12 <- gamlssMXfits(n = 10, ageread ~ 1, family = LO, K = 2, data = glasses)


# Refit the model using gamlssNP()
# If "MASS" is included in the predictor for a distribution parameter (mu, nu and tau),
# then the predictor intercepts differ between the K components.

readnp2 <- gamlssNP(ageread ~ 1, sigma.fo = ~MASS, family = LO, K = 2, data = glasses)



# ------------------------------------------------------------------------------
# Refit simple logistic mixture model with K = 3 by gamlssMXfits
# ------------------------------------------------------------------------------

# this sometimes fails ...
readLO13 <- gamlssMXfits(n = 10, ageread ~ 1, family = LO, K = 3, data = glasses)



# ----------
# compare models with GAIC (a high penalty)
GAIC(readLO12, readnp2, readLO13, k = log(1016))


# -->
# readnp2 and readLO12 is almost similar results



# ----------
# plot the fitted distributions and compare
fn2 <- getpdfMX(readLO12, observation = 1)
fn3 <- getpdfMX(readLO13, observation = 1)

truehist(glasses$ageread, nbins = 25, col = "grey", xlab = "Age", ylab = "Density", ymax = 0.05)
lines(seq(0.5, 90.5, 1), fn2(seq(0.5, 90.5, 1)))
lines(seq(0.5, 90.5, 1), fn3(seq(0.5, 90.5, 1)), col = "blue")



# ----------
# worm plots to check goodness of fit
wp(readLO12)

wp(readLO13)


# -->
# we could see the parallel lines in the worm plots.



# ----------
# original data
barplot(table(glasses$ageread))


# -->
# Some measurement problems ...

