setwd("//media//kswada//MyFiles//R//co2s")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This tutorial is based on Chapter 7. Introductin GAMs from "Generalized Additive Models An Introduction with R 2nd Edition" (by Simon N. Wood)


# ------------------------------------------------------------------------------
# data:  co2s
# ------------------------------------------------------------------------------

data(co2s, package = "gamair")

str(co2s)




# ------------------------------------------------------------------------------
# Smoothing with cr basis (dimension = 300)
# ------------------------------------------------------------------------------

b <- gam(co2 ~ s(c.month, k = 300, bs = "cr"), data = co2s)


summary(b)


plot(b)



# ------------------------------------------------------------------------------
# Prediction with extrapolation
# ------------------------------------------------------------------------------
# obtain predicted CO2 for each month of the data, plus 36 months after the end of the data

pd <- data.frame(c.month = 1:(nrow(co2s) + 36))

fv <- predict(b, pd, se = TRUE)



# ----------
par(mfrow = c(1,1))

plot(pd$c.month, fv$fit, type = "l")
lines(pd$c.month, fv$fit + 2 * fv$se, col=2)
lines(pd$c.month, fv$fit - 2 * fv$se, col=2)



# -->
# The predictions in the last 36 months are incredible ...



