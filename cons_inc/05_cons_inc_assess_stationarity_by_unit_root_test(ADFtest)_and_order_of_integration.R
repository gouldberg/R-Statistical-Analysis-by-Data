# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/cons_inc")

packages <- c("dplyr", "AER", "lmtest", "stargazer", "dynlm", "broom", "knitr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  cons_inc
#    - y:  permanent income, an infinite stream of income
#    - cons:  consumption
# ------------------------------------------------------------------------------
data("cons_inc", package = "POE5Rdata")

data <- cons_inc

glimpse(data)

str(data)



# ----------
# convert to ts object
is.ts(data)
data.ts <- ts(data[,c(2,4)], start = c(1959, 3), end = c(2016, 3), frequency = 4)



# ------------------------------------------------------------------------------
# Correlogram
# ------------------------------------------------------------------------------
# Correlogram to study the stationarity properties of a time series
# Nonstationary series often display serial correlations for many lags

# forecast::Acf can be applied to ts object
graphics.off()
par(cex = 1.4, mar = c(4,4,2,1), lwd = 1.6, mfrow=c(2,1))
forecast::Acf(data.ts[,"cons"], main = "", xlab = "(a) Consumption", ylab = "")
forecast::Acf(diff(data.ts[,"cons"]), main = "", xlab = "(b) First Difference in Consumption", ylab = "")


graphics.off()
par(cex = 1.4, mar = c(4,4,2,1), lwd = 1.6, mfrow=c(2,1))
forecast::Acf(data.ts[,"y"], main = "", xlab = "(a) Permanent Income", ylab = "")
forecast::Acf(diff(data.ts[,"y"]), main = "", xlab = "(b) First Difference in Permanent Income", ylab = "")




# ------------------------------------------------------------------------------
# Order of Integration
#
# forecast::ndiffs(): determining the order of integration in a series 
# ------------------------------------------------------------------------------
# order of integration = 2
ndiffs(data.ts[,"cons"])
ndiffs(data.ts[,"y"])



# ------------------------------------------------------------------------------
# Unit Root Tests for Stationarity:  ADF test
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

# take the 1st difference of consumption
df <- diff(data.ts[,"cons"])

graphics.off()
par(cex = 1.6, mar = c(4,4,2,2), lwd = 1.6, mfrow=c(2,1))
plot(df, ylab = "Difference in cons")
Acf(df, main = "")


# ----------
adf.test(df)


# -->
# rejected  --> indicating even 1st difference is not a unit root ... ?



# ----------
# take the 1st difference of permanent income
df <- diff(data.ts[,"y"])

graphics.off()
par(cex = 1.6, mar = c(4,4,2,2), lwd = 1.6, mfrow=c(2,1))
plot(df, ylab = "Difference in permanent income")
Acf(df, main = "")


# ----------
adf.test(df)


# -->
# rejected  --> indicating even 1st difference is not a unit root ... ?


