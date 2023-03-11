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
# data exploration:  time series decomposition by stats::decompose
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))

dec <- decompose(so2, type = "multiplicative")

plot(dec)



# ----------
dec <- decompose(so2, type = "additive")

plot(dec)




# ----------

monthplot(so2)