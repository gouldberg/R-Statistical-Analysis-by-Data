# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "Hmisc", "rms", "caret", "corrplot", "lattice", "ggplot2", "AER", "foreign", "car", "lmtest", "stargazer", "dynlm")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# data:  phillips
#
# Test for serial autocorrelation (AR(1) serial correlation)
#  - manual test,  Breusch-Godfrey test (Lagrange Multiplier test), and Durbin-Watson test
# ------------------------------------------------------------------------------
data <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/phillips.dta")
dim(data)
str(data)
describe(data)


# define yearly time series beginning in 1948
tsdata <- ts(data, start = 1948)
tsdata


# inf, unem
plot(data$inf, type = "l", lwd = 3, ylim = c(-1,15), xaxt="n", xlab = "", ylab="", main = "inf and unem", las = 1)
par(new=T);  plot(data$unem, type = "l", lwd = 3, lty = 1, col = "blue", ylim = c(-1,15), xaxt="n", xlab = "", ylab = "", yaxt="n")
axis(side = 1, at = 1:nrow(data), labels = seq(1948,1996,by=1))

with(data, plot(unem, inf))


# Estimation of static Phillips curve
# inf(t) = beta0 + beta1 * unem(t) + u(t)
reg.s <- dynlm(inf ~ unem, data = tsdata, end = 1996)
lmtest::coeftest(reg.s)


# residuals and AR(1) test
# There is strong evidence for autocorrelation in the static equation
residuals.s <- resid(reg.s)
coeftest(dynlm(residuals.s ~ L(residuals.s)))


# Same with exepctions-augmented Phillips curve
# delta inf(t) = beta0 + beta1 * unem(t) + u(t)
# The null hypothesis of no autocorrelation cannot be rejected
reg.ea <- dynlm(d(inf) ~ unem, data = tsdata, end = 1996)
residuals.ea <- resid(reg.ea)
coeftest(dynlm(residuals.ea ~ L(residuals.ea)))


# This class of tests can also be performed
# by Breusch-Godfrey test (Lagrange multiplier test) for Serial Correlation of the error term
lmtest::bgtest(reg.s, order = 1)
lmtest::bgtest(reg.ea, order = 1)


# Durbin-Watson test for AR(1) serial correlation
# DW statistic is ranged from 0 to 4,  if close to 2, not autocorrelation, if close to 0 or 4, indicates autocorrelation
lmtest::dwtest(reg.s)
lmtest::dwtest(reg.ea)


# ------------------------------------------------------------------------------
# data:  barium
#
# Effects of Antidumping Filings
#  - monthly data on barium chloride imports from China between Feb 1978 and Dec. 1988.
#
# Seasonality
# Test for serial autocorrelation (AR(3) serial correlation)
# ------------------------------------------------------------------------------
data <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/barium.dta")
dim(data)
str(data)
describe(data)


tsdata <- ts(data, start = c(1978,2), frequency = 12)
tsdata


# chnimp
plot(data$chnimp, type = "l", lwd = 3, xaxt="n", xlab = "", ylab="", main = "chnimp", las = 1)


# Check seasonality
# Monthly dummies do not have significant coeffs except the dummy for April, marginally significant.
mod_season <- dynlm(log(chnimp) ~ log(chempi) + log(gas) + log(rtwex) + befile6 + affile6 + afdec6 + season(tsdata), data = tsdata)
lmtest::coeftest(mod_season)

acf(log(data$chnimp))


# Pedestrian test
reg <- dynlm(log(chnimp) ~ log(chempi) + log(gas) + log(rtwex) + befile6 + affile6 + afdec6, data = tsdata)
lmtest::coeftest(reg)

residual <- resid(reg)
resreg <- dynlm(residual ~ L(residual) + L(residual, 2) + L(residual, 3) + log(chempi) + log(gas) + log(rtwex) + befile6 + affile6 + afdec6, data = tsdata)
linearHypothesis(resreg, c("L(residual)", "L(residual, 2)", "L(residual, 3)"))


# by F test for Serial Correlation of the error term --> indicating AR(3) serial autocorrelation
lmtest::bgtest(reg, order = 3, type = "F")


# Durbin-Watson test for AR(1) serial correlation
lmtest::dwtest(reg)



# ------------------------------------------------------------------------------
# data:  barium
#
# FGLS (Feasible Generalized Least Squares) Estimation by Cochrane-Orcutt estimation
# ------------------------------------------------------------------------------
# OLS estimation
olsres <- dynlm(log(chnimp) ~ log(chempi) + log(gas) + log(rtwex) + befile6 + affile6 + afdec6, data = tsdata)
lmtest::coeftest(olsres)


# Cochrane-Orcutt estimation
library(orcutt)
orcutt::cochrane.orcutt(olsres)



# ------------------------------------------------------------------------------
# data:  barium
#
# Serial Correlation-Robust Inference with OLS:  Newey-West standard errors
# ------------------------------------------------------------------------------
reg <- dynlm(log(chnimp) ~ log(chempi) + log(gas) + log(rtwex) + befile6 + affile6 + afdec6, data = tsdata)
lmtest::coeftest(reg)

# Newey-West HAC (Heteroskedasticity and Autocorrelation Consistent) standard errors
lmtest::coeftest(reg, vcovHAC)



