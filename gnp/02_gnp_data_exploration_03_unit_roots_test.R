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
# Data exploration:  testing unit roots
# ADF test (Augmented Dickey Fuller test):
#   - the default numbber of AR components included in the model, say k, is [[(n-1)^(1/3)]],
#     which corresponds to the suggested upper bound on the rate at which the number of lags, k, should be made to grow with
#     the sample size for the general ARMA(p, q) setup.
#   - Null hypothesis:  time series has a unit root
# ------------------------------------------------------------------------------

# k: the lag order to calculate the test statistic

tseries::adf.test(gnp, k = 0)


tseries::adf.test(gnp)



# -->
# We do NOT reject the null hypothesis that gnp series has a unit root



# ------------------------------------------------------------------------------
# Data exploration:  testing unit roots for stationarity
# PP test (Phillips-Perron test):
#   - the default value of k is [[0.04 * n^(1/4)]]
#   - differs from the ADF tests mainly in how to deal with serial correaltion and heteroskedasticity in the errors
#   - Null hypothesis:  time series has a unit root
# ------------------------------------------------------------------------------

tseries::pp.test(gnp)



# -->
# We do NOT reject the null hypothesis that gnp series has a unit root


