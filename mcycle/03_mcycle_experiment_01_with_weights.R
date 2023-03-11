setwd("//media//kswada//MyFiles//R//mcycle")

packages <- c("dplyr", "lattice", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This tutorial is based on Chapter 7. Introductin GAMs from "Generalized Additive Models An Introduction with R 2nd Edition" (by Simon N. Wood)


# ------------------------------------------------------------------------------
# data:  mcycle
# ------------------------------------------------------------------------------

data(mcycle, package = "MASS")

str(mcycle)




# ------------------------------------------------------------------------------
# Experiment with weights
# ------------------------------------------------------------------------------

# Try giving the first 20 observations the same higher weight, alpha, while leaving the remaining observations with weight one.
# Adjust alpha so that the variance of the first 20 residuals matches that of the remaining residuals

k <- 40

mcw <- gam(accel ~ s(times, k = k), data = mcycle, weights = c(rep(400, 20), rep(1, 113)))

rsd <- residuals(mcw)

var(rsd[21:133])/var(rsd[1:20])




# ----------
par(mfrow=c(2, 2))

# residuals of penalized regression spline fit
plot(mcycle$times, residuals(mod_gam))


# gam fit with experiment with weights
plot(mcw, residuals = TRUE, pch = 1)


# its residuals
plot(mcycle$times, rsd)

