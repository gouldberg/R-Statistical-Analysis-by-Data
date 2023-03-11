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
# Forecasting by sarima.for
# ------------------------------------------------------------------------------

# next 12 months forecast using ARIMA(0,1,3) model

par(mfrow=c(1,1))

sarima.for(log(oil), n.ahead = 12, p = 0, d = 1, q = 3, no.constant = TRUE)
