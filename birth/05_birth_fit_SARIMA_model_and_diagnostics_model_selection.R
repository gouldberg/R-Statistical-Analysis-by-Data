setwd("//media//kswada//MyFiles//R//birth")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  birth
# ------------------------------------------------------------------------------

data(birth, package = "astsa")

str(birth)

head(birth)



# ------------------------------------------------------------------------------
# ARIMA(1,1,2) * (1,1,1)(12) on the logged data
# ------------------------------------------------------------------------------

astsa::acf2(diff(log(birth)), 48)


sarima(log(birth), p = 1, d = 1, q = 2, P = 1, D = 1, Q = 1, S = 12)




# ----------
# Add higher-order AR terms

sarima(log(birth), p = 2, d = 1, q = 2, P = 1, D = 1, Q = 1, S = 12)



# ----------
# Drop SAR term

sarima(log(birth), p = 2, d = 1, q = 2, P = 0, D = 1, Q = 1, S = 12)



# ----------
# Add MA terms is better
sarima(log(birth), p = 2, d = 1, q = 3, P = 0, D = 1, Q = 1, S = 12)



