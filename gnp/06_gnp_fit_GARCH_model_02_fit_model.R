setwd("//media//kswada//MyFiles//R//gnp")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  GNP data
# ------------------------------------------------------------------------------

data(gnp, package = "astsa")

str(gnp)

head(gnp)



# ------------------------------------------------------------------------------
# Fit GARCH model
# ------------------------------------------------------------------------------


library(fGarch)


# garch(1,0) specifies an ARCH(1)
summary(gfit <- garchFit(~ arma(1, 0) + garch(1, 0), gnpgr))



# ----------
par(mfrow = c(3,3))
plot(gfit, which = "all")



# -->
# Note that p-values given in the estimation paragraph are two-sided,
# so they should be halved when considering the ARCH parameters.

# ----------
# Here, we obtain phi0 = 0.005 (called mu in the output) and phi1 = 0.367 (called ar1) for the AR(1) parameter estimates.
# Based on previous analysis, the values were 0.005 and 0.347, respectively.

# ----------
# The ARCH(1) parameter estimates are alpha0 (callded omega) for the constant and
# alpha1 = 0.194, which is significant with a p-value of about 0.02.

# ----------
# There are a number of tests that are performed on the residuals [R] or the squared residuals [R^2].
# For example, the Jarque-Bera statistic tests the residuals of the fit for normality based on the observed skewness and kurtosis,
# and it appears that the residuals have some non-normal skewness and kurtosis.
# The Shapiro-Wilk statistic tests the residuals of the fit for normality based on the empirical order statistics.
# The other tests, primarily based on the Q-statistic, are used on the residuals and their squares.




# ----------
# GARCH one-step-ahead predictions of the DJIA volatility
gfit@sigma.t

par(mfrow = c(1,1))
plot(gnpgr, lty = 1, lwd = 1, type = "l")
lines(ts(gfit@sigma.t, start = c(1947, 2), frequency = 4), lty = 1, lwd = 1, col = "blue")

