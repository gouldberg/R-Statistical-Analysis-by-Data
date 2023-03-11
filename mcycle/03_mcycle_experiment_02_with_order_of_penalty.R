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
# Experiment with order of penalty used in the smooth
# ------------------------------------------------------------------------------

k <- 40
m <- 10

mcwo <- gam(accel ~ s(times, k = k, m = m), data = mcycle, weights = c(rep(400, 20), rep(1, 113)))


rsdo <- residuals(mcwo)

var(rsdo[21:133])/var(rsdo[1:20])




# ----------
par(mfrow=c(2, 2))

# gam fit with experiment with weights
plot(mcw, residuals = TRUE, pch = 1)

# its residuals
plot(mcycle$times, rsd)


# gam fit with experiment with order of penalty
plot(mcwo, residuals = TRUE, pch = 1)

# its residuals
plot(mcycle$times, rsdo)
