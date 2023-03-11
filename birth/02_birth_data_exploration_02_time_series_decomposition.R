setwd("//media//kswada//MyFiles//R//birth")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  birth
# ------------------------------------------------------------------------------

data(birth, package = "astsa")

str(birth)

head(birth)



# ------------------------------------------------------------------------------
# data exploration:  time series decomposition by stats::decompose
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))

dec <- decompose(birth, type = "multiplicative")

plot(dec)



# ----------
dec <- decompose(birth, type = "additive")

plot(dec)
