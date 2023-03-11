setwd("//media//kswada//MyFiles//R//oil_gas")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Oil and Gas
# ------------------------------------------------------------------------------

data(oil, package = "astsa")

data(gas, package = "astsa")

str(oil)

str(gas)

oil

gas




# ------------------------------------------------------------------------------
# data exploration:  Data transformation (differencing logged series)
#   - In economics, it is often the percentage change in price (termed growth rate or return), rather than the absolute price change,
#     that is important.
# ------------------------------------------------------------------------------

# Differencing logged time series will be stationary
forecast::ndiffs(log(oil))

forecast::ndiffs(log(gas))



# ----------
par(mfrow = c(2, 1))

plot(diff(log(oil)), type = "l", main = "Oil: difference of logged")
plot(diff(log(gas)), type = "l", main = "Gas: difference of logged")



