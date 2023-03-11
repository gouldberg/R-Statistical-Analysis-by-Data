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
# fit an un-penalized cubic regression spline
# ------------------------------------------------------------------------------

k <- 11
mod_gam3 <- gam(accel ~ s(times, k = k, fx = TRUE, bs = "cr"), data = mcycle)


summary(mod_gam3)


# -->
# edf = 10



# ----------
# plot the resulting smooth, with partial residuals, but without standard errors
par(mfrow=c(2,2))
plot(mod_gam, residuals = TRUE, se = FALSE, pch = 1)
termplot(mod_lm, partial.resid = TRUE)
plot(mod_gam2, residuals = TRUE, se = FALSE, pch = 1)
plot(mod_gam3, residuals = TRUE, se = FALSE, pch = 1)



