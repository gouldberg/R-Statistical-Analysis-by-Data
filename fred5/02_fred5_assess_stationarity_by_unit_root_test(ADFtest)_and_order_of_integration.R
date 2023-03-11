# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/fred5")

packages <- c("dplyr", "AER", "tseries", "forecast", "dynlm", "stargazer")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  fred5
#  - Personal Consumtion and Personal Disposable Income already in logs over the period of 1986Q1 through 2015Q2
# ------------------------------------------------------------------------------
data("fred5", package = "POE5Rdata")


glimpse(fred5)

str(fred5)


# ----------
# convert to ts object
is.ts(fred5)

fred <- ts(fred5, start = c(1986, 1), end = c(2015, 2), frequency = 4)



# ------------------------------------------------------------------------------
# Correlogram
# ------------------------------------------------------------------------------
# Correlogram to study the stationarity properties of a time series
# Nonstationary series often display serial correlations for many lags

# forecast::Acf can be applied to ts object
graphics.off()
par(cex = 1.4, mar = c(4,4,2,1), lwd = 1.6, mfrow=c(2,1))
forecast::Acf(fred[,"consn"], main = "", xlab = "(a) Personal Consumption (log)", ylab = "")
forecast::Acf(diff(fred[,"consn"]), main = "", xlab = "(b) First Difference in Personal Consumption (log)", ylab = "")



# ----------
graphics.off()
par(cex = 1.4, mar = c(4,4,2,1), lwd = 1.6, mfrow=c(2,1))
forecast::Acf(fred[,"y"], main = "", xlab = "(a) Personal Disposable Income (log)", ylab = "")
forecast::Acf(diff(fred[,"y"]), main = "", xlab = "(b) First Difference in Personal Disposable Income (log)", ylab = "")



# ------------------------------------------------------------------------------
# Order of Integration and Unit Root Tests for Stationarity:  ADF test
#
# Dickey-Fuller test for stationarity based on an AR(1) process
#  - H0:  |rho| = 1,  HA:  |rho| < 1
#    a time series is nonstationary when rho = 1, which makes the AR(1) process a random walk
#
#  - delta Y(t) = alpha + gamma * y(t-1) + gmma * t + v(t)  <--  Transformed AR(1) equations for the purpose of the DF test
#    H0:  gamma = 0,  HA:  gamma < 0
#
#  - because of the unit root, this test statistic is not t or normally distributed, not even asymptotically
#  - the test statistic depends on whether we allow for a time trend in the regression
# 
# Augmented DF test includes several lags of the variable tests.
#  - ADF test for stationarity based on AR(p) process
#
# DF test can be of 3 types;
#  - with no constant and no trend
#  - with constant and no trend
#  - with constant and trend
#
# ##############################################################################################################
# adf.test function dows not require specifying whether the test should be conducted with constant or trend.
# performing an ADF test with automatically selecting the number of lags in delta y
# ------------------------------------------------------------------------------
# forecast::ndiffs(): determining the order of integration in a series 
# ------------------------------------------------------------------------------

# order of integration
ndiffs(fred[,"consn"])



# ----------
# take the 1st difference
df <- diff(fred[,"consn"])

graphics.off()
par(cex = 1.6, mar = c(4,4,2,2), lwd = 1.6, mfrow=c(2,1))
plot(df, ylab = "Difference in Personal Consumption (log)")
Acf(df, main = "")


# ----------
adf.test(fred[,"consn"])
adf.test(df)



# ----------
# order of integration
ndiffs(fred[,"y"])



# ----------
# take the 1st difference
df <- diff(fred[,"y"])

graphics.off()
par(cex = 1.6, mar = c(4,4,2,2), lwd = 1.6, mfrow=c(2,1))
plot(df, ylab = "Difference in Personal Disposable Income (log)")
Acf(df, main = "")


# ----------
adf.test(fred[,"y"])
adf.test(df)


# -->
# for consumption, Order of integration = 2, but for income, Order of integration = 1 !!!
# but Unit Root Test by ADF test indicate that both series are I(1).



# ------------------------------------------------------------------------------
# Other Unit Root Tests for Stationarity
# ------------------------------------------------------------------------------

library(fUnitRoots)

library(urca)


# ----------
# ADF test
# type="c":  random walk model with drift
# type="nc":  random walk model
# type="ct":  random walk model with drift and time trend
unitrootTest(fred[,"consn"], type = "c", lags=4)
unitrootTest(fred[,"y"], type = "c", lags=4)


# -->
# H0 is not rejected --> indicating both of consumption and income are not stationary (possibly unit root process)



# ----------
# Phillips-Perron Test
# model = "constant":  test based on model with constant term
# model = "trend":  test based on model with trend
# lags="long":  lags length is the seisuu-bubun of 4*(T/100)^(1/4)  (T is sample size)
# lags="short":  lags length is the seisuu-bubun of 12*(T/100)^(1/4)  (T is sample size)
summary(ur.pp(fred[,"consn"], type="Z-tau", model="trend", lags="long"))
summary(ur.pp(fred[,"y"], type="Z-tau", model="trend", lags="long"))




# ----------
# KPSS Test
# model = "mu":  test based on model with constant term
# model = "tau":  test based on model with trend
# lags="long":  lags length is the seisuu-bubun of 4*(T/100)^(1/4)  (T is sample size)
# lags="short":  lags length is the seisuu-bubun of 12*(T/100)^(1/4)  (T is sample size)
summary(ur.kpss(fred[,"consn"], type="tau", lags="long"))
summary(ur.kpss(fred[,"y"], type="tau", lags="long"))


# -->
# test statistics > critical value at 5pct --> H0 is rejected, indicating both of consumptiono and income are Unit Root Process


