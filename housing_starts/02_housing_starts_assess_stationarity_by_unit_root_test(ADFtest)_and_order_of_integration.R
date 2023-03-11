# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/housing_starts")

packages <- c("dplyr", "AER", "tseries", "forecast", "dynlm", "stargazer")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data: housing starts  
# ------------------------------------------------------------------------------

starts <- c(1252, 1313, 1463, 1610, 1529, 1473, 1165, 1292, 1508, 1467,
            1434, 2052, 2357, 2045, 1338, 1160, 1538, 1987, 2020, 1745,
            1292, 1084, 1062, 1703, 1750, 1742, 1805, 1621, 1488, 1376,
            1193, 1014, 1200, 1288, 1457, 1354, 1477, 1474, 1617, 1641,
            1569, 1603, 1705, 1848, 1956, 2068)

sales <- c(485, 656, 718, 634, 519, 549, 646, 819, 817, 709, 545,
           436, 412, 623, 639, 688, 750, 671, 676, 650, 534, 509,
           610, 666, 670, 667, 757, 804, 886, 880, 877, 908, 973, 1086, 1203, 1283)


# ----------
# Note that sales data is leading 10 years
length(starts)
length(sales)



# ----------
# add NA to first 10 years to sales
sales2 <- c(rep(NA, 10), sales)
length(sales2)



# ----------
data <- data.frame(starts = starts, sales2 = sales2)

data.ts <- ts(data, start = c(1960), end = c(2005), frequency = 1)



# ------------------------------------------------------------------------------
# Correlogram
# ------------------------------------------------------------------------------
# Correlogram to study the stationarity properties of a time series
# Nonstationary series often display serial correlations for many lags

# forecast::Acf can be applied to ts object and non-ts object
graphics.off()
par(cex = 1.4, mar = c(4,4,2,1), lwd = 1.6, mfrow=c(2,1))
forecast::Acf(data.ts[,"starts"], main = "", xlab = "(a) Starts", ylab = "")
forecast::Acf(diff(data.ts[,"starts"]), main = "", xlab = "(b) First Difference in Starts", ylab = "")


# ----------
# sales have NA values, so give original time series without NA value
graphics.off()
par(cex = 1.4, mar = c(4,4,2,1), lwd = 1.6, mfrow=c(2,1))
forecast::Acf(sales, main = "", xlab = "(a) Sales", ylab = "")
forecast::Acf(diff(sales), main = "", xlab = "(b) First Difference in Sales", ylab = "")



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

forecast::ndiffs(starts)


graphics.off()
par(cex = 1.6, mar = c(4,4,2,2), lwd = 1.6, mfrow=c(2,1))
ts.plot(starts)
forecast::Acf(starts, main = "")


adf.test(starts)



# -->
# starts:  Order of Integration = 0, but cannot reject the null hypothesis of nonstationarity



# ----------
forecast::ndiffs(sales)


graphics.off()
par(cex = 1.6, mar = c(4,4,2,2), lwd = 1.6, mfrow=c(2,1))
ts.plot(sales)
forecast::Acf(sales, main = "")


adf.test(sales)
adf.test(diff(sales))



# -->
# sales: Order of Integration = 1, and cannot reject the null hypothesis of nonstationarity
# 1st difference rejects the null hypothesis of nonstationarity



