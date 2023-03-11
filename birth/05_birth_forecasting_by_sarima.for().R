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
# Forecasting by sarima.for
# ------------------------------------------------------------------------------

# next 12 months forecast using ARIMA(2,1,3) * (0,1,1)(12) model on the logged data.

par(mfrow=c(1,1))

sarima.for(log(birth), n.ahead = 12, p = 2, d = 1, q = 3, P = 0, D = 1, Q = 1, S = 12)
