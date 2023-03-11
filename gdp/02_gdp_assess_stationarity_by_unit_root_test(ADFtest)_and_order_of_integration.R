# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/gdp")

packages <- c("dplyr", "AER", "tseries", "forecast", "dynlm", "stargazer")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gdp
#  - GDP series for Australia and USA for the period 1970Q1 to 2000Q4
# ------------------------------------------------------------------------------
data("gdp", package = "POE5Rdata")


glimpse(gdp)
str(gdp)


# ----------
# convert to ts object
is.ts(gdp)

gdp <- ts(gdp, start = c(1970, 1), end = c(2000, 4), frequency = 4)



# ------------------------------------------------------------------------------
# Correlogram
# ------------------------------------------------------------------------------
# Correlogram to study the stationarity properties of a time series
# Nonstationary series often display serial correlations for many lags

# forecast::Acf can be applied to ts object
graphics.off()
par(cex = 1.4, mar = c(4,4,2,1), lwd = 1.6, mfrow=c(2,1))
forecast::Acf(gdp[,"usa"], main = "", xlab = "(a) US GDP", ylab = "")
forecast::Acf(diff(gdp[,"usa"]), main = "", xlab = "(b) First Difference in US GDP", ylab = "")



# ----------
graphics.off()
par(cex = 1.4, mar = c(4,4,2,1), lwd = 1.6, mfrow=c(2,1))
forecast::Acf(gdp[,"aus"], main = "", xlab = "(a) AUS GDP", ylab = "")
forecast::Acf(diff(gdp[,"aus"]), main = "", xlab = "(b) First Difference in AUS GDP", ylab = "")



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
ndiffs(gdp[,"usa"])



# ----------
# take the 1st difference
df <- diff(gdp[,"usa"])

graphics.off()
par(cex = 1.6, mar = c(4,4,2,2), lwd = 1.6, mfrow=c(2,1))
plot(df, ylab = "Difference in US GDP")
Acf(df, main = "")


# ----------
adf.test(gdp[,"usa"])
adf.test(df)


# ----------
# order of integration
ndiffs(gdp[,"aus"])



# ----------
# take the 1st difference
df <- diff(gdp[,"aus"])

graphics.off()
par(cex = 1.6, mar = c(4,4,2,2), lwd = 1.6, mfrow=c(2,1))
plot(df, ylab = "Difference in AUS GDP")
Acf(df, main = "")


# ----------
adf.test(gdp[,"aus"])
adf.test(df)


# -->
# for both of US and AUS GDP, Order of integration = 2,
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
unitrootTest(gdp[,"usa"], type = "c", lags=4)
unitrootTest(gdp[,"aus"], type = "c", lags=4)


# -->
# H0 is not rejected --> indicating both of USA gdp AUS gdp are not stationary (possibly unit root process)



# ----------
# Phillips-Perron Test
# model = "constant":  test based on model with constant term
# model = "trend":  test based on model with trend
# lags="long":  lags length is the seisuu-bubun of 4*(T/100)^(1/4)  (T is sample size)
# lags="short":  lags length is the seisuu-bubun of 12*(T/100)^(1/4)  (T is sample size)
summary(ur.pp(gdp[,"usa"], type="Z-tau", model="constant", lags="long"))
summary(ur.pp(gdp[,"aus"], type="Z-tau", model="constant", lags="long"))


# -->
# Z-tau > critical value at 5pct --> H0 is not rejected, indicating both of USA gdp and AUS gdp are not stationary (possibly unit root process)



# ----------
# KPSS Test
# model = "mu":  test based on model with constant term
# model = "tau":  test based on model with trend
# lags="long":  lags length is the seisuu-bubun of 4*(T/100)^(1/4)  (T is sample size)
# lags="short":  lags length is the seisuu-bubun of 12*(T/100)^(1/4)  (T is sample size)
summary(ur.kpss(gdp[,"usa"], type="tau", lags="long"))
summary(ur.kpss(gdp[,"aus"], type="tau", lags="long"))


# -->
# test statistics > critical value at 5pct --> H0 is rejected, indicating both of USA gdp and AUS gdp are Unit Root Process


