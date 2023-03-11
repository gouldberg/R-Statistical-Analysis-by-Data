setwd("//media//kswada//MyFiles//R//cmort")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# data:  cmort (Pollution, Temperature and Mortality)
# ------------------------------------------------------------------------------

data(cmort, package = "astsa")
data(tempr, package = "astsa")
data(part, package = "astsa")


str(cmort)
str(tempr)
str(part)


cmort
tempr
part



# ------------------------------------------------------------------------------
# Fit ARMAX model by sarima()
# ------------------------------------------------------------------------------

trend <- time(cmort) - mean(time(cmort))


# This model is different from previous model in that the mortality process is NOT detrended,
# but trend appears as an exogeneous variable

u <- ts.intersect(M = cmort, M1 = stats::lag(cmort, -1), M2 = stats::lag(cmort, -2),
                  T1 = stats::lag(tempr, -1), P = part, P4 = stats::lag(part, -4), trend)


# ----------
sarima(u[,1], 0, 0, 0, xreg = u[,2:7])

