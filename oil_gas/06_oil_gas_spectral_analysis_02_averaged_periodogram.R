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
# Spectral analysis:  averaged periodogram
# ------------------------------------------------------------------------------

L <- 9
L <- 3

m <- (L-1)/2


par(mfrow=c(2,1))


# To compute averaged periodograms, use the Daniell kernel, and specify m, where L = 2m + 1
oil.ave <- astsa::mvspec(oil, kernel("daniell", m), log = "no")

gas.ave <- astsa::mvspec(gas, kernel("daniell", m), log = "no")



# ----------
oil.ave <- astsa::mvspec(diff(log(oil)), kernel("daniell", m), log = "no")

gas.ave <- astsa::mvspec(diff(log(gas)), kernel("daniell", m), log = "no")



# -->
# Oil and Gas has different spectrum for each other

