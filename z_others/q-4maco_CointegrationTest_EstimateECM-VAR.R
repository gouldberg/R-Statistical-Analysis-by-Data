# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
rm(list=ls())
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "lattice", "ggplot2", "AER", "car", "lmtest", "stargazer", "broom", "knitr", "tseries", "forecast", "MTS", "vars")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# data:  q-4macro
#  - rgnp:  US real gross national product (GNP) measured in billions of chained 2005 dollars and seasonally adjusted
#  - tb3m:  Rate of US 3-month treasury bills obtained by simple average of the monthly rates within a quarter
#  - m1sk:  US M1 money stock obtained from the average of monthly data, which are measured in billions of dollars and seasonally adjusted
#  - gs10:  Rate of US 10-year constant maturity interest rate obtained by simple average of the monthly rates within a quarter
#  - The period is from the 1st Q of 1959 to the 2nd Q of 2012 for 214 observations
# ------------------------------------------------------------------------------
data <- read.table("./RefData/MultivariateTimeSeriesAnalysisWithRAndFinancialApplications/q-4macro.txt", header=T, stringsAsFactors=F)
str(data)

graphics.off()
MTSplot(data[,c(4:7)])


# take log for RGNP and M1 money stock
data <- cbind(log(data$rgnp), data$tb3m, log(data$m1sk), data$gs10)
colnames(data) <- c("rgnp", "tb3m", "lnm1", "gs10")

graphics.off()
MTSplot(data[,c(1:4)])


# Order of Integration
apply(data, 2, ndiffs)


# ------------------------------------------------------------------------------
# Check the symptom of nonstationarity
# ------------------------------------------------------------------------------
# Correlogram to study the stationarity properties of a time series
# Nonstationary series often display serial correlations for many lags
graphics.off()
par(cex = 1.4, mar = c(4,4,2,1), lwd = 1.6, mfrow=c(3,3))
apply(data, 2, Acf)
apply(data, 2, Pacf)


# ------------------------------------------------------------------------------
# Select VAR orders
# ------------------------------------------------------------------------------
varmat <- as.matrix(data)
vars::VARselect(varmat, lag.max = 10, type = "const")
# vars::VARselect(varmat, lag.max = 10, type = "trend")

MTS::VARorderI(data)

# --> we choose p = 2, p = 6, or p = 5 (reasons explained later ?)


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
unitrootTest(data[,1], type = "nc", lags=6)
unitrootTest(data[,2], type = "c", lags=6)
unitrootTest(data[,3], type = "nc", lags=6)
unitrootTest(data[,4], type = "c", lags=6)


# KPSS Test
# model = "mu":  test based on model with constant term
# model = "tau":  test based on model with trend
# lags="long":  lags length is the seisuu-bubun of 4*(T/100)^(1/4)  (T is sample size)
# lags="short":  lags length is the seisuu-bubun of 12*(T/100)^(1/4)  (T is sample size)
# test statistics > critical value at 5pct --> H0 is rejected, indicating UNIT ROOT PROCESS
summary(ur.kpss(data[,1], type="tau", lags="long")) # --> rejected
summary(ur.kpss(data[,2], type="mu", lags="long")) # --> NOT rejected
summary(ur.kpss(data[,3], type="tau", lags="long")) # --> rejected
summary(ur.kpss(data[,4], type="mu", lags="long")) # --> NOT rejected


# ADF Test --> not rejected
fUnitRoots::adfTest(data[,1], lags = 6, type = "c")
fUnitRoots::adfTest(data[,2], lags = 6, type = "nc")
fUnitRoots::adfTest(data[,3], lags = 6, type = "c")
fUnitRoots::adfTest(data[,4], lags = 6, type = "nc")


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

# eigenvalues are: 0.1922, 0.1357, 0.0400, 0.0201
# we cannot reject r <= 2, the matrix in the ECM is of rank 2 and there is 2 cointegrating vectors.
# 2 cointegrating normalized vectors are g1 = (1, -0.282, -0.792, 0.313) and g2 = (1, -0.789, -0.673, 0.773)
summary(ca.jo(data, K = 5, ecdet = c("const"), spec = c("transitory")))
# summary(ca.jo(data, K = 6, ecdet = c("const"), spec = c("transitory")))


# use trace test sttistic
summary(ca.jo(data, K = 5, ecdet = c("const"), spec = c("transitory"), type = c("trace")))


# The cointegration series
w1t <- data[,1] - 0.282 * data[,2] - 0.792 * data[,3] + 0.313 * data[,4]
w2t <- data[,1] - 0.780 * data[,2] - 0.673 * data[,3] + 0.773 * data[,4]
tmp <- cbind(data, w1t, w2t)
MTSplot(tmp)


# Reject Unit Root in wt
fUnitRoots::adfTest(w1t, lags = 6, type = "c")
fUnitRoots::adfTest(w2t, lags = 6, type = "c")
summary(ur.kpss(w1t, type="mu", lags="long"))
summary(ur.kpss(w2t, type="mu", lags="long"))


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


# Phillips-Ouliaris test:  if H0 of no cointegration is NOT rejected  -->  NOT COINTEGRATION RELATIONSHIP
varmat <- as.matrix(data, demean = FALSE)
tseries::po.test(varmat)


# ------------------------------------------------------------------------------
# Estimation of ECM-VAR models
# ------------------------------------------------------------------------------
wt <- cbind(w1t, w2t)

# Estimate ECM-VAR models
# original time series, p of VAR(p), and Co-integrated series is given
ecm <- ECMvar1(data, p = 6, wt, include.const=T)


# Model Checking
# The latter plots suggest that the volatility of the residuals is likely to be time-varying (conditioanl heteroscedasticity)
MTSdiag(ecm)


# Reine the model
# For simplicity, we simply remove those parameters with t-ratio less than 0.8 in modulus. (arbitrarily selected)
ecm_ref <- refECMvar1(ecm, thres = 0.8)


# --> NOTE that alpha shows that delta(log(rgnp)) and delta(log(m1sk)) depend on w1t(t-1) but not on w2t(t-1)
# also that delta(tb3m) and delta(gs10) depend on w2t(t-1) but not on w1t(t-1)
# since formers are macroeconomic variables whereas latters are interest rate series, 
# we see that macroeconomic variables are interest rates require different adjustments in the error-correction representation.
