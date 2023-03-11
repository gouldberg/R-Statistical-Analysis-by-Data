setwd("//media//kswada//MyFiles//R//jj")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Johnson & Johnson Quarterly Earnings
# ------------------------------------------------------------------------------

data(jj, package = "astsa")

str(jj)

jj



# ------------------------------------------------------------------------------
# data exploration:  time series decomposition by stats::decompose
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))

dec <- decompose(jj, type = "multiplicative")

plot(dec)



# ----------
dec <- decompose(jj, type = "additive")

plot(dec)


