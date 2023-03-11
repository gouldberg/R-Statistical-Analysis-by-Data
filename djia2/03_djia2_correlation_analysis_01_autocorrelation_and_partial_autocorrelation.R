setwd("//media//kswada//MyFiles//R//djia2")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  djia2 (Dow JOnes Industrial Average)
# ------------------------------------------------------------------------------

data(djia, package = "astsa")

str(djia)

djia


# library(TTR)
# djia <- getYahooData("^DJI", start = 20060420, end = 20160420, freq = "daily")

djiar <- diff(log(djia$Close))[-1]



# ------------------------------------------------------------------------------
# Correlation analysis:  autocorrelation and partial autocorrelation
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow = c(1, 1))


# acf2:  produces a simultaneous plot (and a printout) of the sample ACF and PACF
# The zero lag value of the ACF is removed.

# The lags in terms of weeks
astsa::acf2(djiar, 100)



# -->
# ACF and PACF is quite similar to each other...
# Exihibit some autocorrelation:  ARIMA(2,1,3) ?  but not tailing off...



# ------------------------------------------------------------------------------
# Correlation analysis:  autocorrelation and partial autocorrelation for squared values
# ------------------------------------------------------------------------------

astsa::acf2(djiar^2, 20)




# ------------------------------------------------------------------------------
# Correlation analysis:  autocorrelation and partial autocorrelation for absolute raw value
# ------------------------------------------------------------------------------

astsa::acf2(abs(djiar), 200)


# -->
# Sample ACF of the absolute value of the NYSE returns indicates long memory

