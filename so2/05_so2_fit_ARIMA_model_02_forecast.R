setwd("//media//kswada//MyFiles//R//so2")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  so2
# ------------------------------------------------------------------------------

data(so2, package = "astsa")

str(so2)

so2



# ------------------------------------------------------------------------------
# Forecast by sarima.for
# ------------------------------------------------------------------------------

# forecast data into the future fourt time periods ahead (about one month)

sarima.for(so2, n.ahead = 4, p = 2, d = 1, q = 3, P = 0, D = 0, Q = 0, S = 0, no.constant = TRUE)

