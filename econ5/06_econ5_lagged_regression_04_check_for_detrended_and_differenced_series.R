setwd("//media//kswada//MyFiles//R//econ5")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  econ5
# ------------------------------------------------------------------------------

data(econ5, package = "astsa")


str(econ5)


car::some(econ5)




# ------------------------------------------------------------------------------
# Data transformation
# ------------------------------------------------------------------------------

# First logging and then removing the linear trend

U <- resid(lm(log(econ5$unemp) ~ time(log(econ5$unemp))))

G <- resid(lm(log(econ5$gnp) ~ time(log(econ5$gnp))))

C <- resid(lm(log(econ5$consum) ~ time(log(econ5$consum))))


x <- cbind(G, U, C)


MTSplot(x)




# ----------
Ud <- diff(econ5$unemp)

Gd <- diff(econ5$govinv)

Cd <- diff(econ5$consum)



dat <- cbind(Gd, Cd, Ud)




# ------------------------------------------------------------------------------
# Check the cross-correlations among differenced time series
# ------------------------------------------------------------------------------


# differenced
acf(dat, max.lag = 20, cex.main = 2)



# -->
# Gd agasint Ud has lag 5,7,8
# Cd against Ud has lag 0 (concurrent)



# detrended 
acf(x, max.lag = 20, cex.main = 2)



# -->
# G against U has lag up to 12
# C against U has long lags




# ------------------------------------------------------------------------------
# Check the autocorrelation for detrended and differenced series
# ------------------------------------------------------------------------------


MTSplot(x)


# differenced of detrended series

acf2(diff(U), max.lag = 40, cex.main = 2)


acf2(diff(G), max.lag = 40, cex.main = 2)


acf2(diff(C), max.lag = 40, cex.main = 2)



# -->
# U:  ARIMA(1,1,1)  or  ARIMA(1,1,2)   # some cyclic features are remained
# G:  ARIMA(1,1,2)
# C:  ARIMA(2,1,2)  #  but it is difficult ...


sarima(U, 1, 1, 2)

sarima(G, 1, 1, 2)

sarima(C, 2, 1, 2)


