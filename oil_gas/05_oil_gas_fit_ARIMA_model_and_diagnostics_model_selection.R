setwd("//media//kswada//MyFiles//R//oil_gas")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Oil and Gas
# ------------------------------------------------------------------------------

data(oil, package = "astsa")
data(gas, package = "astsa")

str(oil)
str(gas)

oil
gas



# ------------------------------------------------------------------------------
# ARIMA(3,1,3) on the logged oil
# ------------------------------------------------------------------------------

astsa::acf2(diff(log(oil)), 48)


sarima(log(oil), p = 3, d = 1, q = 3)



# ----------
# no.constant = TRUE

sarima(log(oil), p = 3, d = 1, q = 3, no.constant = TRUE)



# ----------
# droppint MA terms
sarima(log(oil), p = 3, d = 1, q = 0, no.constant = TRUE)


# dropping AR terms
sarima(log(oil), p = 0, d = 1, q = 3, no.constant = TRUE)



# ----------
# Add more higher MA terms

sarima(log(oil), p = 0, d = 1, q = 10, no.constant = TRUE)


# -->
# CAN NOT GET SIGNIFICANT MODEL ...


