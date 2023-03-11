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
# Add smoother with cyclic
# ------------------------------------------------------------------------------

# cc: cyclic
# Need to use the knots argument of gam to ensure to wraps appropriately:  it is important to make sure that
# January is the same as January, not that December and January are the same !!
b2 <- gam(co2 ~ s(month, bs = "cc") + s(c.month, bs = "cr", k = 300), knots = list(month = seq(1, 13, length = 10)), data = co2s)


summary(b2)


plot(b2)



# ------------------------------------------------------------------------------
# Prediction with extrapolation
# ------------------------------------------------------------------------------
# obtain predicted CO2 for each month of the data, plus 36 months after the end of the data

pd2 <- data.frame(c.month = 1:(nrow(co2s) + 36), month = rep(1:12, length.out = nrow(co2s) + 36))

fv <- predict(b2, pd2, se = TRUE)



# ----------
plot(pd$c.month, fv$fit, type = "l")

lines(pd$c.month, fv$fit + 2 * fv$se, col = 2)

lines(pd$c.month, fv$fit - 2 * fv$se, col = 2)


# -->
# Prediction is more credible.
