rm(list=ls())

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  so2
#   - Sulfur dioxide series.
#     Sulfur dioxide is one of the pollutants monitored in the mortality study
# ------------------------------------------------------------------------------


data(so2, package = "astsa")


str(so2)


so2



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

