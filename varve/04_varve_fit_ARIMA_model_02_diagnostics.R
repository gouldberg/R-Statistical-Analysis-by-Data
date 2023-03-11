setwd("//media//kswada//MyFiles//R//varve")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  varve
# ------------------------------------------------------------------------------

data(varve, package = "astsa")

str(varve)



# ------------------------------------------------------------------------------
# Diagnostics for ARIMA(0,1,1) model
# ------------------------------------------------------------------------------

# no.constant = TRUE:  we do not fit drift in the differenced, logged varve series.

astsa::sarima(log(varve), 0, 1, 1, no.constant = TRUE)


# -->
# Ljung-Box-Pierce Q-statistic for ARIMA(0,1,1) are all significant.
# There appears to be a small amount of autocorrelation left in the residuals



# ------------------------------------------------------------------------------
# Adjust by ARIMA(1,1,1) model
# ------------------------------------------------------------------------------

astsa::sarima(log(varve), 1, 1, 1, no.constant = TRUE)



# -->
# We obtained the estimates:  ar1 = 0.23  theta(ma1) = -0.89
# AR term is significant.

# Ljung-Box-Pierce Q-statistic for ARIMA(0,1,1) are all not significant, and
# this model fits the data well.

