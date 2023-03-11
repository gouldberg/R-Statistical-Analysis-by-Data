setwd("//media//kswada//MyFiles//R//varve")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  varve
# ------------------------------------------------------------------------------

data(varve, package = "astsa")

str(varve)



# ------------------------------------------------------------------------------
# Data exploration:  sample autocorrelation function (ACF)
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))

acf(varve, lag.max = 500, plot=TRUE)



# ----------
par(mfrow=c(1,1))
forecast::Acf(varve, lag.max = 500)



# -->
# This time series has autocorrelation with very large lags



# ------------------------------------------------------------------------------
# Data exploration:  sample autocorrelation for 1st difference and 1st difference for logged varve
# ------------------------------------------------------------------------------

par(mfrow = c(2,2))

plot(diff(varve))
acf(diff(varve), lag.max = 500, plot=TRUE)

plot(diff(log(varve)))
acf(diff(log(varve)), lag.max = 500, plot=TRUE)


# -->
# Differencing the logged varve data produces a reasonably stationary series.



# ------------------------------------------------------------------------------
# ACF and PACF
# ------------------------------------------------------------------------------

par(cex = 1.4, mar = c(4,4,2,1), lwd = 1.6, mfrow=c(2,1))

Acf(diff(log(varve)), lag.max = 40)

Pacf(diff(log(varve)), lag.max = 40)


acf2(diff(log(varve)), max.lag = 80)


# -->
# The sample ACF and PACF confirm the tendency of diff(log(varve)) to behave as a first-order moving average process
# as the ACF has only a significant peak at lag one and the PACF decreases exponentially.

# This sample behavior fits that of the MA(1) very well.


