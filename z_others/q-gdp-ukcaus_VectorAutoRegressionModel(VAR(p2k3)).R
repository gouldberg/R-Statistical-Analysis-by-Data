# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
rm(list=ls())
gc();  gc();
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "AER", "stargazer", "broom", "knitr", "tseries", "vars", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# data:  q-dgdp-ukcaus
#  - The quarterly growth rates, in percentages, of real gross domescic product (GDP) of UK, Canada, and USA
#    from the 2nd quarter of 1980 to the 2nd quarter of 2011.
#  - seasonally adjusted
#  - The GDP in millions of local currency and the growth rate denotes the differenced series of log GDP
# ------------------------------------------------------------------------------
da <- read.table("./RefData/MultivariateTimeSeriesAnalysisWithRAndFinancialApplications/q-gdp-ukcaus.txt", header = T)
str(da)


# Percentage of GDP Growth Rate
gdp <- log(da[,3:5])
z <- apply(gdp, 2, diff) * 100
z

graphics.off()
par(cex = 1.4, mar = c(4,4,2,1), lwd = 1.6, mfrow=c(2,1))
ts.plot(z[,"uk"], z[,"ca"], z[,"us"], type = "l", lty = c(1,2,3), col = c(1,2,3))
ts.plot(diff(z[,"uk"]), diff(z[,"ca"]), diff(z[,"us"]), type = "l", lty = c(1,2,3), col = c(1,2,3))

MTS::MTSplot(z)
MTS::MTSplot(diff(z))

  
# ------------------------------------------------------------------------------
# Order of Integration
# ------------------------------------------------------------------------------
# Order of Integration is zero
forecast::ndiffs(z[,"uk"])
forecast::ndiffs(z[,"ca"])
forecast::ndiffs(z[,"us"])


# ------------------------------------------------------------------------------
# Correlogram to study the stationarity properties of a time series
#  - Nonstationary series often display serial correlations for many lags
# ------------------------------------------------------------------------------
library(forecast)

graphics.off()
par(cex = 1.4, mar = c(4,4,2,1), lwd = 1.6, mfrow=c(3,2))
Acf(z[,"uk"], main = "", xlab = "(a1) UK Percentage of GDP Growth Rate", ylab = "")
Pacf(z[,"uk"], main = "", xlab = "(a2) UK Percentage of GDP Growth Rate", ylab = "")

Acf(z[,"ca"], main = "", xlab = "(b1) Canada Percentage of GDP Growth Rate", ylab = "")
Pacf(z[,"ca"], main = "", xlab = "(b2) Canada Percentage of GDP Growth Rate", ylab = "")

Acf(z[,"us"], main = "", xlab = "(c1) US Percentage of GDP Growth Rate", ylab = "")
Pacf(z[,"us"], main = "", xlab = "(c2) US Percentage of GDP Growth Rate", ylab = "")



# ------------------------------------------------------------------------------
# Unit Root Tests for Stationarity
# ------------------------------------------------------------------------------
# ADF test with Intercept and No Trend
# We rejected the null hypothesis of nonstationarity  -->  Not unit root process
adf.test(z[,"uk"])
adf.test(z[,"ca"])
adf.test(z[,"us"])


library(fUnitRoots)
library(urca)


# ADF test
# type="c":  random walk model with drift
# type="nc":  random walk model
# type="tc":  random walk model with drift and time trend
# H0 is rejected --> NOT unit root process
unitrootTest(z[,"uk"], type = "nc", lags=4)
unitrootTest(z[,"ca"], type = "nc", lags=4)
unitrootTest(z[,"us"], type = "nc", lags=4)


# Phillips-Perron Test
# model = "constant":  test based on model with constant term
# model = "trend":  test based on model with trend
# lags="long":  lags length is the seisuu-bubun of 4*(T/100)^(1/4)  (T is sample size)
# lags="short":  lags length is the seisuu-bubun of 12*(T/100)^(1/4)  (T is sample size)
# NOT REJECTED
summary(ur.pp(z[,"uk"], type="Z-tau", model="constant", lags="long"))
summary(ur.pp(z[,"ca"], type="Z-tau", model="constant", lags="long"))
summary(ur.pp(z[,"us"], type="Z-tau", model="constant", lags="long"))


# KPSS Test
# model = "mu":  test based on model with constant term
# model = "tau":  test based on model with trend
# lags="long":  lags length is the seisuu-bubun of 4*(T/100)^(1/4)  (T is sample size)
# lags="short":  lags length is the seisuu-bubun of 12*(T/100)^(1/4)  (T is sample size)
# H0 is NOT rejected, indicating NOT UNIT ROOT PROCESS
summary(ur.kpss(z[,"uk"], type="mu", lags="long"))
summary(ur.kpss(z[,"uk"], type="mu", lags="long"))
summary(ur.kpss(z[,"uk"], type="mu", lags="long"))


# ------------------------------------------------------------------------------
# Vector Autoregression Model (VAR)
# ------------------------------------------------------------------------------
varmat <- as.matrix(z)


# determine optimal lag length according to AIC, HQ, SC, FPE(final prediction error) of an empirical VAR(p) process
# MTS::VARorder also calculates the M-statistics
vars::VARselect(varmat, lag.max = 10, type = "const")
MTS::VARorder(z/100)


# we choose p = 2
varfit <- vars::VAR(varmat, p = 2, type = "const")
summary(varfit)

varfit_MTS <- MTS::VAR(z, p = 2)



# ------------------------------------------------------------------------------
# Model Checking
# ------------------------------------------------------------------------------
# fit and residuals, ACF residuals, PACF residuals
plot(varfit)


# Multivariate Pormanteau Test by adjusting the degrees of freedom
# p: AR(p)   k: dimension of vectors (3 time-series)
# The fitted VAR(2) model has largely removed the dynamic dependence in the GDP growth rates, except for some minor violation at m = 4
p <- 2;  k <- 3;
mq(varfit_MTS$residuals, adj = p * k^2)


# MTSdiag checks 24 lags of cross-correlation matrix in default
# p-value plot of individual ccm
# plot of p-values of the Qk(m) statistics
# residual plots
MTSdiag(varfit_MTS, adj = p * k^2)


# check stability: stable model has all roots < 1
vars::roots(varfit)



# ------------------------------------------------------------------------------
# Model Simplification
#  - Multivariate time series models may contain many parameters if the dimension k is moderate or large.
#   In practice, often observe that some of the parameters are not statistically significant at a given sifnificant level.
# ------------------------------------------------------------------------------
# Testing zero parameters
# It turns  out that there are 10 and 8 possible zero parameters for these 2 choices of alpha
# The chi-square test for all 10 targeted parameters being 0 is 31.69 and p-value 0.0005.
# We can simplify the fitted VAR(2) model by letting the 8 parameters with smallest t-ratio (in absolute) to zero.

MTS::VARchi(z, p = 2)  # alpha = 0.1 and critical values are 1.645 (default)
MTS::VARchi(z, p = 2, thres = 1.96)  # alpha = 0.05


# Model refinement
# The simplified VAR(2) model has 12 parameters, instead of 21.
# Also AIC is smaller than unconstrained model (varfit)
varfit_MTS_ref <- MTS::refVAR(varfit_MTS, thres = 1.96)

MTSdiag(varfit_MTS_ref, adj = 12)


# -->  Base on mimimum Qk(m) = 4,  We may consider some further improvement, such as using a VAR(4) model.




# ------------------------------------------------------------------------------
# Granger causality testing
# ------------------------------------------------------------------------------
# UK granger-causes and instantaneously causes (weak) CA and US
causality(varfit, cause = "uk")


# CA do not granger-causes but instantaneously causes UK and US
causality(varfit, cause = "ca")


# US granger-causes and instantaneously causes UK and CA
causality(varfit, cause = "us")



# ------------------------------------------------------------------------------
# Impulse Responses and Variance Decompositions
# ------------------------------------------------------------------------------
impresp <- irf(varfit)


# an impulse (shock) at time zero has large effects the next period, but the effects become smaller and smaller at the time passes
# NOTE that the impulse response function decay to 0 quickly, which is expected for a stationary series.
plot(impresp, plot.type = "m", names = "uk", main = "", mar=c(gap=0.4, 5.1, gap = 0.4, 2.1), lwd = 1.6)
plot(impresp, plot.type = "m", names = "ca", main = "", mar=c(gap=0.4, 5.1, gap = 0.4, 2.1), lwd = 1.6)
plot(impresp, plot.type = "m", names = "us", main = "", mar=c(gap=0.4, 5.1, gap = 0.4, 2.1), lwd = 1.6)


# origianl and Orthogonal innovations (also accumulated effect)
# Change in US rate at time t affects the Canadian rate at time t+1, which in turn affects the UK rate at time t+2  --> marginal effect (not conditional effect)
MTS::VARirf(varfit_MTS_ref$Phi, varfit_MTS_ref$Sigma, orth = F)
MTS::VARirf(varfit_MTS_ref$Phi, varfit_MTS_ref$Sigma)


# Forecast variance decomposition estimates the contribution of a shock in each variable to the reponse in both variables.
plot(vars::fevd(varfit))

FEVdec(varfit_MTS_ref$Phi, Theta = NULL, varfit_MTS_ref$Sigma, lag = 5)



# ------------------------------------------------------------------------------
# Prediction
# ------------------------------------------------------------------------------
# Out-of-sample forecasting of VAR models and fan-chart
var.pred <- predict(varfit, n.ahead = 10, ci = 0.95)
plot(var.pred)


fanchart(var.pred)


# refinement model
VARpred(varfit_MTS_ref, h = 10)
