setwd("//media//kswada//MyFiles//R//globtemp")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  globtemp
# ------------------------------------------------------------------------------

data(globtemp, package = "astsa")

str(globtemp)




# ------------------------------------------------------------------------------
# Correlation analysis:  autocorrelation and partial autocorrelation
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow = c(1, 1))


# acf2:  produces a simultaneous plot (and a printout) of the sample ACF and PACF
# The zero lag value of the ACF is removed.

astsa::acf2(diff(globtemp), 48)




# ----------
par(mfrow = c(1, 1))

plot(diff(globtemp), type = "o")
( drift_est <- mean(diff(globtemp)) )
abline(h = drift_est, lty = 2, col = "blue")



# -->
# It appears that the differenced process shows minimal autocorrelation, which may imply the global temperature series
# is nearly a random walk with drift.
# It is interesting to note that if the series is a random walk with drift, the mean of the differenced series,
# which is an estimate of the drift,
# is about 0.008, or an increase of about one degree centigrade per 100 years.

# Also, the autocorrelation and partial autocorrelation suggests ARIMA(3,1,2)
