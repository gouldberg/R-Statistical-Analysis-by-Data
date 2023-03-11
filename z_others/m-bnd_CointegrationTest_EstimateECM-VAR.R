# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
rm(list=ls())
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "lattice", "ggplot2", "AER", "car", "lmtest", "stargazer", "broom", "knitr", "tseries", "forecast", "MTS", "vars")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# data:  m-bnd
#  - Monthly yields of Moody's seasoned corporate bonds from July 1954 to Feb. 2005 for 609 observations
# ------------------------------------------------------------------------------
data <- read.table("./RefData/MultivariateTimeSeriesAnalysisWithRAndFinancialApplications/m-bnd.txt", header=F, stringsAsFactors=F)
str(data)

data <- data[,4:5]
colnames(data) <- c("Aaa", "Baa")

# Order of Integration
apply(data, 2, ndiffs)

graphics.off()
MTSplot(data)


# ------------------------------------------------------------------------------
# Check the symptom of nonstationarity
# ------------------------------------------------------------------------------
# Correlogram to study the stationarity properties of a time series
# Nonstationary series often display serial correlations for many lags
graphics.off()
par(cex = 1.4, mar = c(4,4,2,1), lwd = 1.6, mfrow=c(2,2))
apply(data, 2, Acf)
apply(data, 2, Pacf)


# ------------------------------------------------------------------------------
# Select VAR orders
# ------------------------------------------------------------------------------
varmat <- as.matrix(data)
vars::VARselect(varmat, lag.max = 10, type = "const")

MTS::VARorder(data)

# --> we choose p = 3


# ------------------------------------------------------------------------------
# Unit Root Tests for Stationarity
# ------------------------------------------------------------------------------
library(fUnitRoots)
library(urca)

# ADF test
# type="c":  random walk model with drift
# type="nc":  random walk model
# type="tc":  random walk model with drift and time trend
# H0 is not rejected --> indicating NOT stationary
unitrootTest(data[,1], type = "c", lags=3)
unitrootTest(data[,2], type = "c", lags=3)


# KPSS Test
# model = "mu":  test based on model with constant term
# model = "tau":  test based on model with trend
# lags="long":  lags length is the seisuu-bubun of 4*(T/100)^(1/4)  (T is sample size)
# lags="short":  lags length is the seisuu-bubun of 12*(T/100)^(1/4)  (T is sample size)
# test statistics > critical value at 5pct --> H0 is rejected, indicating UNIT ROOT PROCESS
summary(ur.kpss(data[,1], type="mu", lags="long"))
summary(ur.kpss(data[,1], type="mu", lags="long"))


# ADF Test
fUnitRoots::adfTest(data[,1], lags = 3, type = "c")
fUnitRoots::adfTest(data[,2], lags = 2, type = "c")


# ------------------------------------------------------------------------------
# Cointegration Test
#  - Johansen's cointegration test
# ------------------------------------------------------------------------------
library(urca)

# Johansen's cointegration test
# Choose Error Correction Model:  spec = c("transitory") or spec = c("longrun")
# Choose Test Statistics:  type = c("trace") or type = c("eigen")
# Order p is denoted by K
# Deterministic term c(t):  ecdet = c("none"), "const" or "trend"

# use maximum eigenvalue and use z(t-k) in ECM
# eigenvalues are: 0.055 and 0.005
# we reject r = 0 but we cannot reject r = 1 --> the matrix in the ECM is of rank 1 and there is a cointegrating vector.
summary(ca.jo(data, K = 2, ecdet = c("none")))

# use the z(t-1) in ECM
summary(ca.jo(data, K = 2, ecdet = c("none"), spec = c("transitory")))

# use trace test sttistic
# we reject r = 0 but we cannot reject r = 1 --> the matrix in the ECM is of rank 1 and there is a cointegrating vector.
summary(ca.jo(data, K = 2, ecdet = c("none"), spec = c("transitory"), type = c("trace")))


# The cointegration series
wt <- data[,1] - 0.886 * data[,2]
tmp <- cbind(data, wt)
MTSplot(tmp)


# Reject Unit Root in wt
fUnitRoots::adfTest(wt, lags = 3, type = "c")
summary(ur.kpss(wt, type="mu", lags="long"))


# ------------------------------------------------------------------------------
# Cointegration Test
#  - Phillips-Ouliaris test:  ca.po(),  po.test()
# ------------------------------------------------------------------------------
# Phillips-Ouliaris Test
# cons(t) = beta0 + beta1 * gdp(t) + u(t)
# demean = "const":  test based on model with drift
# demean = "none":  test based on random walk model
# demean = "trend":  test based on random walk model with drift and time trend
# test statistic > critical value at 5pct --> H0 is rejected, indicating COINTEGRATION RELATIONSHIP
summary(ca.po(data, demean="none"))
summary(ca.po(data, demean="const"))


# Phillips-Ouliaris test:  if H0 of no cointegration is rejected  -->  COINTEGRATION RELATIONSHIP
varmat <- as.matrix(data, demean = FALSE)
tseries::po.test(varmat)


# ------------------------------------------------------------------------------
# Estimation of ECM-VAR models
# ------------------------------------------------------------------------------
# Again, the cointegration series
wt <- data[,1] - 0.886 * data[,2]
tmp <- cbind(data, wt)
MTSplot(tmp)


# Estimate ECM-VAR models
# original time series, p of VAR(p), and Co-integrated series is given
ecm <- ECMvar1(data, p = 3, wt)


# Reine the model
ecm_ref <- refECMvar1(ecm)


# Joint estimation
beta <- c(1, -0.886)
ecm3 <- ECMvar(data, p = 3, ibeta = beta, include.const=F)


# Model Checking
MTSdiag(ecm3)

