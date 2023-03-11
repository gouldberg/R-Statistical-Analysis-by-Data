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
# fit penalized regression spline
# ------------------------------------------------------------------------------

# gam to fit a univariate smooth to the data, selecting the smoothing parameter by GCV
# k of 30 to 40 is plenty for this example

k <- 40
mod_gam <- gam(accel ~ s(times, k = k), data = mcycle)


summary(mod_gam)


# -->
# edf = 11.23



# ----------
# plot the resulting smooth, with partial residuals, but without standard errors
plot(mod_gam, residuals = TRUE, se = FALSE, pch = 1)



# residuals of penalized regression spline fit
plot(mcycle$times, residuals(mod_gam))


