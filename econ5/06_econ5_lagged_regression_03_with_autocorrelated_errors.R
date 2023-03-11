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
# Check the autocorrelation
# ------------------------------------------------------------------------------


acf2(Ud, max.lag = 20, cex.main = 2)


acf2(Gd, max.lag = 20, cex.main = 2)


acf2(Cd, max.lag = 20, cex.main = 2)



# -->
# Ud:  ARIMA(1,0,1)  or  ARIMA(1,0,2)
# Gd:  ARIMA(1,0,3)
# Cd:  ARIMA(3,0,3)


sarima(Ud, 1, 0, 2)

sarima(Gd, 1, 0, 3)

sarima(Cd, 3, 0, 3)




# ------------------------------------------------------------------------------
# ARIMAX model
# ------------------------------------------------------------------------------


( ts_al <- ts.intersect(Ud, Gd8 = stats::lag(Gd, 8), Cd, dframe = TRUE) )



sarima(ts_al[,1], 1, 0, 1, xreg = ts_al[,2:3])



# -->
# but not good fit ..
# Note that ACF of residuals:  still MA(4) residuals are remained ..




# ----------
# adding MA(4)

sarima(ts_al[,1], 1, 0, 4, xreg = ts_al[,2:3])




# ----------
# try dropping Cd and Gd8
sarima(ts_al[,1], 1, 0, 4, xreg = ts_al[,3])
sarima(ts_al[,1], 1, 0, 4, xreg = ts_al[,2])



# -->
# Still Gd8 is not significant



# -->
# current best model is:  Ud(t) = Uds ARIMA(1,0,4) + Cd
sarima(ts_al[,1], 1, 0, 4, xreg = ts_al[,3])



