setwd("//media//kswada//MyFiles//R//arf")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  arf
# ------------------------------------------------------------------------------

data(arf, package = "astsa")

str(arf)

head(arf)



# ------------------------------------------------------------------------------
# Data exploration:  testing unit roots for stationarity
# ADF test (Augmented Dickey Fuller test):
#   - the default numbber of AR components included in the model, say k, is [[(n-1)^(1/3)]],
#     which corresponds to the suggested upper bound on the rate at which the number of lags, k, should be made to grow with
#     the sample size for the general ARMA(p, q) setup.
#   - Null hypothesis:  time series has a unit root
# ------------------------------------------------------------------------------

# k: the lag order to calculate the test statistic

tseries::adf.test(arf, k = 0)


tseries::adf.test(arf)



# -->
# We reject the null hypothesis that arf series has a unit root



# ------------------------------------------------------------------------------
# Data exploration:  testing unit roots for stationarity
# PP test (Phillips-Perron test):
#   - the default value of k is [[0.04 * n^(1/4)]]
#   - differs from the ADF tests mainly in how to deal with serial correaltion and heteroskedasticity in the errors
#   - Null hypothesis:  time series has a unit root
# ------------------------------------------------------------------------------

tseries::pp.test(arf)



# -->
# We reject the null hypothesis that arf series has a unit root


# -->
# The conclusin of these tests supports that the arf series is long memory rather than integrated.


