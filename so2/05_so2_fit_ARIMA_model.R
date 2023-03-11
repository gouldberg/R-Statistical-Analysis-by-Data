setwd("//media//kswada//MyFiles//R//so2")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  so2
# ------------------------------------------------------------------------------

data(so2, package = "astsa")

str(so2)

so2



# ------------------------------------------------------------------------------
# Fit ARIMA model to so2 data
# ------------------------------------------------------------------------------

astsa::acf2(diff(log(so2)), 52*4, main = "Sulfur Dioxide")



# fit ARIMA(7,1,3) with no.constant = FALSE (there is decreasing trend)
sarima(so2, p = 7, d = 1, q = 3, P = 0, D = 0, Q = 0, S = 0)



# ----------
# Dropping higher-order AR terms
sarima(so2, p = 4, d = 1, q = 3, P = 0, D = 0, Q = 0, S = 0)



# ----------
# Dropping higher-order AR terms more
sarima(so2, p = 2, d = 1, q = 3, P = 0, D = 0, Q = 0, S = 0)



# ----------
# Dropping constant  --> ARIMA(2,1,3)
sarima(so2, p = 2, d = 1, q = 3, P = 0, D = 0, Q = 0, S = 0, no.constant = TRUE)

