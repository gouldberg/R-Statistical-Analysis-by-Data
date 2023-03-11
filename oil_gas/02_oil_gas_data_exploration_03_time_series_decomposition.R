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
# data exploration:  time series decomposition by stats::decompose
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))

dec <- decompose(oil, type = "multiplicative")

plot(dec)



# ----------
dec <- decompose(oil, type = "additive")

plot(dec)



# ----------
dec <- decompose(gas, type = "multiplicative")

plot(dec)


dec <- decompose(gas, type = "additive")

plot(dec)




# ----------
monthplot(oil)
