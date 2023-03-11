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
# Fit ARIMA model
# ------------------------------------------------------------------------------

sarima(arf, p = 1, d = 1, q = 1)

sarima(arf, p = 0, d = 1, q = 1)


# This model is close to ARFIMA(1,1,0)
# but phi = 0.17, much different from phi = 0.75
sarima(arf, p = 1, d = 1, q = 0)



# -->
# Note that there remains some autocorrelation for residuals at lag 13