setwd("//media//kswada//MyFiles//R//globtemp")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  globtemp
# ------------------------------------------------------------------------------

data(globtemp, package = "astsa")


str(globtemp)




# ------------------------------------------------------------------------------
# Data exploration:  testing unit roots for stationarity
# ADF test (Augmented Dickey Fuller test):
#   - the default numbber of AR components included in the model, say k, is [[(n-1)^(1/3)]],
#     which corresponds to the suggested upper bound on the rate at which the number of lags, k, should be made to grow with
#     the sample size for the general ARMA(p, q) setup.
#   - Null hypothesis:  time series has a unit root
# ------------------------------------------------------------------------------

# Note that in each of DF, ADF and PP tests, the general regression equation incorporates a constant and a linear trend.


# k: the lag order to calculate the test statistic

# DF test:  reject the null hypothesis that this time series has a unit root

tseries::adf.test(globtemp, k = 0)



# ----------
# ADF test:  here, we do NOT reject !!

tseries::adf.test(globtemp)




# ------------------------------------------------------------------------------
# Data exploration:  testing unit roots for stationarity
# PP test (Phillips-Perron test):
#   - the default value of k is [[0.04 * n^(1/4)]]
#   - differs from the ADF tests mainly in how to deal with serial correaltion and heteroskedasticity in the errors
#   - Null hypothesis:  time series has a unit root
# ------------------------------------------------------------------------------

tseries::pp.test(globtemp)



# -->
# We reject the null hypothesis that this time series has a unit root




# ------------------------------------------------------------------------------
# Data exploration:  testing unit roots for stationarity
# KPSS test:  Null hypothesis is stationarity
# ------------------------------------------------------------------------------


library(urca)


# we need "summary" for details
# type:  deterministic part, mu for a constant and tau for a constant with linear trend

summary(ur.kpss(globtemp, type = "mu"))


summary(ur.kpss(globtemp, type = "tau"))



# -->
# both tests are rejected 



