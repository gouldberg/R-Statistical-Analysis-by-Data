setwd("//media//kswada//MyFiles//R//varve")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  varve
# ------------------------------------------------------------------------------

data(varve, package = "astsa")

str(varve)



# ------------------------------------------------------------------------------
# Smoothed periodogram  -->  Only next year has meaningful level...
# ------------------------------------------------------------------------------

k <- kernel("modified.daniell", c(3,3))

plot(k)



# ----------
mvspec(varve, kernel = k, taper = 0.1, log="yes")

mvspec(log(varve), kernel = k, taper = 0.1, log="yes")

mvspec(diff(log(varve)), kernel = k, taper = 0.1, log="yes")
