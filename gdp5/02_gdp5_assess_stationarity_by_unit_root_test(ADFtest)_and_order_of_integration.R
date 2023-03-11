# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/gdp5")

packages <- c("dplyr", "AER", "tseries", "forecast", "dynlm", "stargazer")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  
#  - gdp5:  U.S. GDP over the period of 1984Q1 through 2016Q1
#  - usdata5:  monthly data on the variables inflation, federal funds rate, and bond rate in the U.S. over the period of August 1954 through December 2016
# ------------------------------------------------------------------------------
data("gdp5", package = "POE5Rdata")
data("usdata5", package = "POE5Rdata")


is.ts(gdp5)
is.ts(usdata5)


head(gdp5)
head(usdata5)



# ----------
# convert to ts object
# Note that gdp5 and usdata5 have different frequency (quarterly and monthly)
gdp.ts <- ts(gdp5$gdp, start = c(1984, 1), end = c(2016, 4), frequency = 4)
usa.ts <- ts(usdata5, start = c(1954, 8), end = c(2016, 12), frequency = 12)



# ----------
# create data.frame from ts object
usa.ts.df <- data.frame(b = usa.ts[,2], f = usa.ts[,3], inf = usa.ts[,4])



# ------------------------------------------------------------------------------
# Check the symptom of nonstationarity
# ------------------------------------------------------------------------------
# Check the symptom of nonstationarity:  a significant difference in means between two periods
kable(head(usa.ts.df), align = "c", col.names = c("Bond Rate", "Funds Rate", "Inflation Rate"), caption = "Head of the usdata5 Time Series Data")

mean(window(gdp.ts, from = c(1984,2), end=c(2000,3)))
mean(window(gdp.ts, from = c(2000,2), end=c(2016,4)))
mean(window(usa.ts[,"infn"], from = c(1954,8), end=c(1985,10)))
mean(window(usa.ts[,"infn"], from = c(1985,11), end=c(2016,12)))



# ------------------------------------------------------------------------------
# Correlogram
# ------------------------------------------------------------------------------
# Correlogram to study the stationarity properties of a time series
# Nonstationary series often display serial correlations for many lags

# forecast::Acf can be applied to ts object
graphics.off()
par(cex = 1.4, mar = c(4,4,2,1), lwd = 1.6, mfrow=c(2,1))
forecast::Acf(gdp.ts, main = "", xlab = "(a) GDP", ylab = "")
forecast::Acf(diff(gdp.ts), main = "", xlab = "(b) First Difference in GDP", yalb = "")



# ----------
# acf can be applied to non-ts object
par(cex = 1.4, mar = c(4,4,2,1), lwd = 1.6, mfrow=c(2,2))
apply(usdata5[,c("br","ffr","infn")], 2, acf)

par(cex = 1.4, mar = c(4,4,2,1), lwd = 1.6, mfrow=c(2,2))
tmp <- data.frame(br_diff = diff(usdata5$br), ffr_diff = diff(usdata5$ffr), infn_diff = diff(usdata5$infn))
apply(tmp[,c("br_diff","ffr_diff","infn_diff")], 2, acf)



# ------------------------------------------------------------------------------
# Unit Root Tests for Stationarity:  ADF test of federal funds rate with Intercept and No Trend
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

graphics.off()
par(cex = 1.6, mar = c(4,4,2,2), lwd = 1.6, mfrow=c(2,1))
ts.plot(usa.ts[, "ffr"])
forecast::Acf(usa.ts[,"ffr"], main = "")


# -->
# Time series plot indicates both intercept and trend for federal funds rate,
# while the correlogram sggests including 10 lags in the DF test euqation



# ----------
adf.test(usa.ts[,"ffr"], k = 10)


# -->
# The result of the tst is a p-value greater than 0.05;
# therefore, we cannot reject the null hypothesis of nonstationarity



# ------------------------------------------------------------------------------
# Unit Root Tests for Stationarity:  ADF test of bond rate with Intercept and No Trend
# ------------------------------------------------------------------------------
graphics.off()
par(cex = 1.6, mar = c(4,4,2,2), lwd = 1.6, mfrow=c(2,1))
ts.plot(usa.ts[, "br"])
forecast::Acf(usa.ts[,"br"], main = "")


adf.test(usa.ts[,"br"], k = 10)


# -->
# We can not reject the null hypothesis of nonstationarity  -->  3-year Bond Rate is nonstationary



# ------------------------------------------------------------------------------
# Unit Root Tests for Stationarity:  ADF test of gdp with Intercept and "Trend"
# ------------------------------------------------------------------------------
graphics.off()
par(cex = 1.6, mar = c(4,4,2,2), lwd = 1.6, mfrow=c(2,1))
ts.plot(gdp.ts)
forecast::Acf(gdp.ts, main = "")


# -->
# This fiture indicates that the gdp series could be modeled with trend and intercept.



# ----------
adf.test(gdp.ts)


# -->
# We can not reject the null hypothesis of nonstationarity  -->  GDP is nonstationary



# ------------------------------------------------------------------------------
# Order of Integration of federal funds rate
#
# forecast::ndiffs(): determining the order of integration in a series 
# ------------------------------------------------------------------------------
# order of integration
ndiffs(usa.ts.df$f)



# ----------
# take the 1st difference
# Federal Bond Rate is nonstationary in levels but stationary in its first difference
df <- diff(usa.ts.df$f)

graphics.off()
par(cex = 1.6, mar = c(4,4,2,2), lwd = 1.6, mfrow=c(2,1))
plot(df, ylab = "Difference in ffr")
Acf(df, main = "")


# ----------
adf.test(df, k = 2)


# -->
# Federal Bond Rate is nonstationary in levels but stationary in its first difference



# ------------------------------------------------------------------------------
# Order of Integration of bond rate
# ------------------------------------------------------------------------------
# order of integration
ndiffs(usa.ts.df$b)



# ----------
# take the 1st difference
db <- diff(usa.ts.df$b)

graphics.off()
par(cex = 1.6, mar = c(4,4,2,2), lwd = 1.6, mfrow=c(2,1))
plot(db, ylab = "Difference in br")
Acf(db, main = "")



# ----------
adf.test(db, k = 1)


# -->
# 3-year Bond Rate is nonstationary in levels but stationary in its first difference



# ------------------------------------------------------------------------------
# Order of Integration of inflation rate
# ------------------------------------------------------------------------------
# order of integration
ndiffs(usa.ts.df$inf)



# ----------
# take the 1st difference
db <- diff(usa.ts.df$inf)

graphics.off()
par(cex = 1.6, mar = c(4,4,2,2), lwd = 1.6, mfrow=c(2,1))
plot(db, ylab = "Difference in inf")
Acf(db, main = "")



# ----------
adf.test(db, k = 1)


# -->
# Inflation Rate is nonstationary in levels but stationary in its first difference



# ----------
apply(data.frame(gdp5[,"gdp"]), 2, forecast::ndiffs)
apply(usa.ts.df, 2, forecast::ndiffs)


