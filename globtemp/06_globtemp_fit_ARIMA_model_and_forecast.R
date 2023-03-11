setwd("//media//kswada//MyFiles//R//globtemp")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  globtemp
# ------------------------------------------------------------------------------

data(globtemp, package = "astsa")

str(globtemp)



# ------------------------------------------------------------------------------
# Fit ARIMA(3,1,2) model
# ------------------------------------------------------------------------------


astsa::acf2(diff(globtemp), 48)


sarima(globtemp, p = 3, d = 1, q = 2)



# -->
# But AR(2) and MAcoefficients and constant is not significant ...
# There are several outliers.



# ----------
# note that constant = 0.007  (close to the drift term in random walk with drift)
sarima(globtemp, p = 2, d = 1, q = 2)



# if assume AR(1) --> AR(1) coefficient is 0.9571  --> close to random walk
sarima(globtemp, p = 1, d = 0, q = 0)





# ----------
# next 10 year forecast

sarima.for(globtemp, n.ahead = 10, p = 3, d = 1, q = 2)

