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
# Model choice
# ------------------------------------------------------------------------------

mod_ma[c("AIC", "AICc", "BIC")]

mod_ar[c("AIC", "AICc", "BIC")]



# -->
# AIC and AICc both prefer the MA(2) fit, whereas the BIC prefers the simpler AR(1) model.
# It is often the case that the BIC will select a model of smaller order than the AIC or AICc.
# In either case, it is not unreasonable to retain the AR(1) because pure autoregressive models are easier to work with.
