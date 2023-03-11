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
# Gaussian location-scale model
#   - accel ~ N(mu, sigma^2)
#   - mu = f1(time)
#   - log(sigma - beta) = f2(time)    b: small lower bound on sigma ensuring that we avoid singularities in the likelihood
# ------------------------------------------------------------------------------

# first specifying the response and the linear predictor for the muan and the next specifying the linear predictor for the log (shifted) standard deviation
# bs = "ad":  adaptive smooth
b <- gam(list(accel ~ s(times, bs = "ad"), ~s(times, bs = "ad")), family = gaulss, data = mcycle)


summary(b)



# ----------
fitted(b)


# -->
# resulting fitted values will now be a two column matrix
#   - the first column is the predicted mean (mu), while the second is the square root of the precision (i.e., 1 / sigma)



# ----------
predict(b, type = "response")



# ----------
# estimated smooth for the mean acceleration
plot(b, page = 2, scheme = 1)


# estimated smooth for the log(shifted) standard deviation
plot(b, page = 2, scheme = 1, ylim = c(-4,2))
