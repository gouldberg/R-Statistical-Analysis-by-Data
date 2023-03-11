rm(list=ls())

packages <- c("dplyr", "dlm", "KFAS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Nile
# ------------------------------------------------------------------------------

data(Nile)


dim(Nile)


str(Nile)



# ------------------------------------------------------------------------------
# Fitting ARIMA model
# ------------------------------------------------------------------------------

library(astsa)

sarima(Nile, p = 2, d = 1, q = 1)



sarima.for(Nile, n.ahead = 10, p = 2, d = 1, q = 1)
