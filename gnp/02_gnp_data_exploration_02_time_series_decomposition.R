setwd("//media//kswada//MyFiles//R//gnp")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  GNP data
# ------------------------------------------------------------------------------

data(gnp, package = "astsa")

str(gnp)

head(gnp)



# ------------------------------------------------------------------------------
# data exploration:  time series decomposition by stats::decompose
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))

dec <- decompose(gnp, type = "multiplicative")

plot(dec)



# ----------
dec <- decompose(gnp, type = "additive")

plot(dec)


