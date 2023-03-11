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


acf2(Ud, max.lag = 40, cex.main = 2)


acf2(Gd, max.lag = 40, cex.main = 2)


acf2(Cd, max.lag = 40, cex.main = 2)



# -->
# Ud:  ARIMA(1,0,1)  or  ARIMA(1,0,2)
# Gd:  ARIMA(1,0,3)
# Cd:  ARIMA(3,0,3)


sarima(Ud, 1, 0, 2)

sarima(Gd, 1, 0, 3)

sarima(Cd, 3, 0, 3)




# ------------------------------------------------------------------------------
# Regression with lagged variables with align lagged series
# ------------------------------------------------------------------------------


# align the lagged series
( ts_al <- ts.intersect(Ud, Gd8 = stats::lag(Gd, 8), Cd, dframe = TRUE) )




# ----------
fit1 <- lm(Ud ~ Gd8 + Cd, data = ts_al)



summary(fit1)



# ----------
par(mfrow = c(2,2), mar = c(2,2,2,2))

plot(fit1)




# ----------
acf2(resid(fit1), max.lag = 10)



# -->
# still the residulas AR(1,1)




# ------------------------------------------------------------------------------
# Regression with lagged variables:  no need to align lagged series
# ------------------------------------------------------------------------------

# The headache of aligning the lagged series can be avoided by using dynlm
library(dynlm)


# dynlm model have time series attributes without any additional commands
fit2 <- dynlm(Ud ~ L(Gd, 8) + Cd)


summary(fit2)



# ----------
plot(fit2)
