# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "Hmisc", "rms", "caret", "corrplot", "lattice", "ggplot2", "AER", "foreign", "car", "lmtest", "stargazer", "broom", "knitr", "plm", "systemfit", "tseries")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# data:  gandc  (Japan's seasonality-adjusted quarterly data over the period of 1980Q1 through 2009Q3)
#  - gdp:  real GDP
#  - cons:  real private-sector final consumption
# ------------------------------------------------------------------------------
data <- read.csv("./Rによる計量経済分析/gandc.csv", header=T, stringsAsFactors=F)
str(data)

is.ts(data)

gdp <- ts(data$gdp, start = c(1980, 1), end = c(2009, 3), frequency = 4)
cons <- ts(data$cons, start = c(1980, 1), end = c(2009, 3), frequency = 4)


# Order of Integration
forecast::ndiffs(gdp)
forecast::ndiffs(cons)

graphics.off()
par(cex = 1.4, mar = c(4,4,2,1), lwd = 1.6, mfrow=c(2,3))
ts.plot(gdp, xlab = "(a) Real GDP (billion Yen)", ylab = "")
ts.plot(diff(gdp, 1), xlab = "(b) Change in GDP", ylab = "")
ts.plot(diff(gdp, 2), xlab = "(b2) 2-time Differencing in GDP", ylab = "")
ts.plot(cons, xlab = "(c) Real Consumption (billion Yen)", ylab = "")
ts.plot(diff(cons, 1), xlab = "(d) Change in Consumption", ylab = "")
ts.plot(diff(cons, 2), xlab = "(d2) 2-time Differencing in CONS", ylab = "")


# 2 lines on 1 chart
graphics.off()
par(cex = 1.4, mar = c(4,4,2,1), lwd = 1.6, mfrow=c(1,1))
plot(ts.union(gdp, cons), plot.type="single", pch=1:2, type="o", col=c(1,2), yaxt = "n", ylab = "billion Yen")
legend("topleft", legend = c("GDP", "CONS"), lty=c(1:2), pch=c(1,2), col=c(1,2))
z <- seq(200000, 600000, by = 100000)
axis(side = 2, at = z, labels=sprintf("%10.0f", z), las=1)


# ------------------------------------------------------------------------------
# Check the symptom of nonstationarity
# ------------------------------------------------------------------------------
# Check the symptom of nonstationarity:  a significant difference in means between two periods
mean(window(gdp, from = c(1980,1), end=c(1990,4)))
mean(window(gdp, from = c(1991,1), end=c(2000,4)))

mean(window(cons, from = c(1980,1), end=c(1990,4)))
mean(window(cons, from = c(1991,1), end=c(2000,4)))


# Correlogram to study the stationarity properties of a time series
# Nonstationary series often display serial correlations for many lags
graphics.off()
par(cex = 1.4, mar = c(4,4,2,1), lwd = 1.6, mfrow=c(3,1))
forecast::Acf(gdp, main = "", xlab = "(a) GDP", ylab = "")
forecast::Acf(diff(gdp), main = "", xlab = "(b) First Difference in GDP", ylab = "")
forecast::Acf(diff(gdp,2), main = "", xlab = "(b2) Second Difference in GDP", ylab = "")

forecast::Acf(cons, main = "", xlab = "(a) CONS", ylab = "")
forecast::Acf(diff(cons), main = "", xlab = "(b) First Difference in CONS", ylab = "")
forecast::Acf(diff(cons,2), main = "", xlab = "(b2) Second Difference in CONS", ylab = "")


# ------------------------------------------------------------------------------
# Unit Root Tests for Stationarity
# ------------------------------------------------------------------------------
library(fUnitRoots)
library(urca)

# ADF test
# type="c":  random walk model with drift
# type="nc":  random walk model
# type="tc":  random walk model with drift and time trend
# H0 is not rejected --> indicating both of gdp and cons are not stationary (possibly unit root process)
unitrootTest(gdp, type = "c", lags=8)
unitrootTest(cons, type = "c", lags=3)


# Phillips-Perron Test
# model = "constant":  test based on model with constant term
# model = "trend":  test based on model with trend
# lags="long":  lags length is the seisuu-bubun of 4*(T/100)^(1/4)  (T is sample size)
# lags="short":  lags length is the seisuu-bubun of 12*(T/100)^(1/4)  (T is sample size)
# Z-tau > critical value at 5pct --> H0 is not rejected, indicating both of gdp and cons are not stationary (possibly unit root process)
summary(ur.pp(gdp, type="Z-tau", model="constant", lags="long"))
summary(ur.pp(cons, type="Z-tau", model="constant", lags="long"))


# KPSS Test
# model = "mu":  test based on model with constant term
# model = "tau":  test based on model with trend
# lags="long":  lags length is the seisuu-bubun of 4*(T/100)^(1/4)  (T is sample size)
# lags="short":  lags length is the seisuu-bubun of 12*(T/100)^(1/4)  (T is sample size)
# test statistics > critical value at 5pct --> H0 is rejected, indicating both of gdp and cons are Unit Root Process
summary(ur.kpss(gdp, type="mu", lags="long"))
summary(ur.kpss(cons, type="mu", lags="long"))


library(dynlm)
g.dyn <- dynlm(d(gdp) ~ L(gdp) + L(d(gdp)) + L(d(gdp), 2))
kable(tidy(g.dyn), digis = 4, align = "c", caption = "Checking GDP for Stationarity")

# The model with the coef of gdp(t-1) and the unit-root term --> insignificant
gdp.ct <- dynlm(d(gdp) ~ trend(gdp) + L(gdp) + L(d(gdp, 1)) + L(d(gdp), 2))
kable(tidy(gdp.ct), digis = 3, align = "c", caption = "GDP Model with Intercept and Trend")



# ------------------------------------------------------------------------------
# Cointegration Test
#  - test whether the residuals from regressin one series on the other one are stationary  (Dickey Fuller stationarity test on residuals)
#  - Phillips-Ouliaris test (po.test)
# ------------------------------------------------------------------------------
# it seems that the regression is significant 
mod <- glm(cons ~ gdp, family = "gaussian")
summary(mod)


library(urca)

# Phillips-Ouliaris Test
# cons(t) = beta0 + beta1 * gdp(t) + u(t)
# demean = "const":  test based on model with drift
# demean = "none":  test based on random walk model
# demean = "trend":  test based on random walk model with drift and time trend
# test statistic < critical value at 5pct --> H0 is not rejected, indicating NO COINTEGRATION RELATIONSHIP between gdp and cons --> the regression is not significant
summary(ca.po(ts.union(cons,gdp), demean="const"))


# But the delta cons(t) = beta0 + beta1 + delta gdp(t) + u(t)  is COINTEGRATED
summary(ca.po(ts.union(diff(cons,1),diff(gdp,1)), demean="const"))

graphics.off()
par(cex = 1.6, mar = c(4,4,2,2), lwd = 1.6, mfrow=c(2,1))
ts.plot(diff(cons,1))
ts.plot(diff(gdp,1))


# We reject the H0 that residuals have unit roots, therefore the series are conintegrated
cg.dyn <- dynlm(diff(cons,1) ~ diff(gdp,1))
# cg.dyn <- dynlm(cons ~ gdp)
ehat.cg <- resid(cg.dyn)
output <- dynlm(d(ehat.cg) ~ L(ehat.cg) + L(d(ehat.cg)) + L(d(ehat.cg), 2) - 1)
kable(tidy(output), digits = 4, alighn = "c", caption = "Cointegration Test Between delta CONS and delta GDP")

