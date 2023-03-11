setwd("//media//kswada//MyFiles//R//air_passengers")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Air Passengers
#   - Monthly totals of international airline passengers, 1949 to 1960, taken from Box and Jenkins.
# ------------------------------------------------------------------------------

data(AirPassengers, package = "datasets")

str(AirPassengers)

head(AirPassengers)


# ----------
x <- AirPassengers

lx <- log(x)

dlx <- diff(lx)

ddlx <- diff(dlx, 12)



# ------------------------------------------------------------------------------
# Forecasting by sarima.for
# ------------------------------------------------------------------------------

# twelve month forecast using ARIMA(0,1,1) * (0,1,1)(12) model on the logged data.
sarima.for(lx, n.ahead = 12, p = 0, d = 1, q = 1, P = 0, D = 1, Q = 1, S = 12)

