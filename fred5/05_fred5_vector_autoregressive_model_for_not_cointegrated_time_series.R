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
# Vector Autoregression Model (VAR)
#   - IF BOTH SERIES ARE I(1) and TWO TIME SERIES ARE NOT COINTEGRATED, the model is
#     y(t) = beta10 + beta11 * y(t-1) + beta12 * x(t-1) + vt(y)
#     x(t) = beta20 + beta21 * y(t-1) + beta22 * x(t-1) + vt(x)
#
#   - but in differences, as specified in
#     d(y(t)) = beta11 * d(y(t-1)) + beta12 * d(x(t-1)) + vt(d(y))
#     d(x(t)) = beta21 * d(y(t-1)) + beta22 * d(x(t-1)) + vt(d(x))
# ------------------------------------------------------------------------------

# Since both time series are I(1), take difference
Dc <- diff(fred[,"consn"])
Dy <- diff(fred[,"y"])

varmat <- as.matrix(cbind(Dc, Dy))


# ----------
library(MTS)
data_dif <- data.frame(Dc = Dc, Dy = Dy)
MTSplot(data_dif)




# ------------------------------------------------------------------------------
# Vector Autoregression Model (VAR):  determine optimal lag length
# ------------------------------------------------------------------------------
# determine optimal lag length according to AIC, HQ, SC, FPE(final prediction error) of an empirical VAR(p) process
library(vars)
VARselect(varmat, lag.max = 10, type = "trend")



# ----------
# determine optimal lag length according to AIC, BIC, HQ
MTS::VARorder(data_dif)



# -->
# we choose p = 3


# ------------------------------------------------------------------------------
# Vector Autoregression Model (VAR)
# ------------------------------------------------------------------------------
# Check the time series order:  we order by Dc --> Dy
names(data_dif)



# type = "trend"
# varfit <- VAR(varmat)
varfit <- VAR(varmat, p = 3, type = "trend")
summary(varfit)




# ----------
varfit_MTS <- MTS::VAR(cbind(data_dif), p = 3)



# ------------------------------------------------------------------------------
# Model Simplification (Refinement)
# ------------------------------------------------------------------------------
# refVAR carry out model simplification of a fitted VAR model.
# The command uses a threshold to select target parameters for removal and computes information criteria of the simplified model for validation.
# The default threshold is 1.00
# The command also allows the user to specify zero parameters with a subcommand fixed.
varfit_MTS_ref <- refVAR(varfit_MTS, thres = 1.96)


varfit_MTS$aic;  varfit_MTS$bic;
varfit_MTS_ref$aic;  varfit_MTS_ref$bic;


# -->
# Note that refined model has lower AIC



# ------------------------------------------------------------------------------
# Model Checking
# ------------------------------------------------------------------------------
# fit and residuals, ACF residuals, PACF residuals
graphics.off()
plot(varfit)



# ----------
# Multivariate Pormanteau statistics, adjusted by degrees of freedom
# AR(p) and k is number of vectors (time-series)
# If not varfit_MTS but varfit_MTS_ref, decrease degrees of freedom by zero parameters
p <- 3;  k <- 2;
mq(varfit_MTS$residuals, adj = p * k^2)



# MTSdiag checks 24 lags of cross-correlation matrix in default
# p-value plot of individual ccm
# plot of p-values of the Qk(m) statistics
# residual plots
# If not varfit_MTS but varfit_MTS_ref, decrease degrees of freedom by zero parameters
MTSdiag(varfit_MTS, adj = p * k^2)



# check stability: stable model has all roots < 1
vars::roots(varfit)





