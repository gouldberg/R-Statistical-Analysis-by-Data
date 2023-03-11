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
# Correlation analysis:  autocorrelation and partial autocorrelation
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow = c(3, 1))


# acf2:  produces a simultaneous plot (and a printout) of the sample ACF and PACF
# The zero lag value of the ACF is removed.

# The lags in terms of years
astsa::acf2(dlx, 50)


# -->
# Clearly yearly seasonal cycle



# ----------
astsa::acf2(ddlx, 50)


# -->
# Seasonal Components:  It appears that at the seasons, the ACF is cutting off a lag 1s (s = 12), whereas the PACF is tailing off at lags 1s, 2s, 3s, 4s, ...
# These results implies SMA(1), P = 0, Q = 1, in the season (s = 12)

# Non seasonal components:  Inspecting the sample ACF and PACF at the lower lags, it appears as though both are tailing off.
# This suggests an ARMA(1,1) within the seasons, p = q = 1

# Thus, we first try and ARIMA(1,1,1) * (0,1,1)(12) on the logged data.





