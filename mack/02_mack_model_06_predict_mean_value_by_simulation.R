setwd("//media//kswada//MyFiles//R//mack")

packages <- c("dplyr", "lattice", "gamair")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This tutorial is based on
# Chapter 7. GAMs in Practive: mgcv from "Generalized Additive Models An Introduction with R 2nd Edition" (by Simon N. Wood)


# ------------------------------------------------------------------------------
# data:  mack
# ------------------------------------------------------------------------------

data(mack, package = "gamair")

data(mackp, package = "gamair")

data(coast, package = "gamair")


str(mack)

str(mackp)

str(coast)



# ----------
# mackp contains prediction grid data for 1992 mackerel egg model.
# A data from with 5 columns. Each row corresponds to one spatial location within the survey area.

# coast:  European coastline from -11 to 0 East and from 43 to 59 North



# ------------------------------------------------------------------------------
# Predict mean value by simulation
# ------------------------------------------------------------------------------

# Typically, uncertainty estimates are required for quantities derived from fitted model predictions.
# These can be obtained by simulation from the posterior distribution of the model coefficients
# A disadvantage of such simulations is that they are conditional on the estimated smoothing parameters.

set.seed(4)
br1 <- rmvn(n = 1000, coef(gm2), vcov(gm2))

br1


Xp <- predict(gm2, newdata = mackp, type = "lpmatrix")
mean.eggs1 <- colMeans(exp(Xp %*% t(br1)))


# ----------
hist(mean.eggs1)



# ----------
# smoothing parameter uncertainty correction by substituting vcov(gm2, unconditional = TRUE) for vcov(gm2)

set.seed(4)
br2 <- rmvn(n = 1000, coef(gm2), vcov(gm2, unconditional = TRUE))

br2


mean.eggs2 <- colMeans(exp(Xp %*% t(br2)))




# ----------
par(mfrow = c(2, 1), mar = c(2,2,2,2))

hist(mean.eggs1)

hist(mean.eggs2)



# ----------
summary(mean.eggs1)

summary(mean.eggs2)


# -->
# Usually, the unconditional distribution will be a little wider than the fixed smoothing parameter version.

