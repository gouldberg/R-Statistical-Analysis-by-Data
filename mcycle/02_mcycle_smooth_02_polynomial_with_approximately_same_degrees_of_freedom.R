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
# Fit a polynomial to the data, with approximately the same degrees of freedom as was estimated by gam
# ------------------------------------------------------------------------------

mod_lm <- lm(accel ~ poly(times, degree = 11), data = mcycle)


summary(mod_lm)



# ----------
# Compare gam and polynomial model
par(mfrow=c(1,2))
plot(mod_gam, residuals = TRUE, se = FALSE, pch = 1)
termplot(mod_lm, partial.resid = TRUE)


# -->
# Note the substantially worse fit achieved by the polynomial, relative to the penalized regression spline fit