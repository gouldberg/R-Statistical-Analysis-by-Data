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
# data exploration:  time series plot
# ------------------------------------------------------------------------------

par(mfrow = c(2, 2))

plot(oil, type = "l", main = "Oil")
plot(gas, type = "l", main = "Gas")

plot(diff(log(oil)), type = "l", main = "Oil")
plot(diff(log(gas)), type = "l", main = "Gas")



# -->
# IT seeems that those time series are not stationary ...



# ----------
forecast::ndiffs(oil)

forecast::ndiffs(gas)

