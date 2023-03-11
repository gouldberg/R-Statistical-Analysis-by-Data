setwd("//media//kswada//MyFiles//R//econ5")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  econ5
#   - quarterly U.S. unemployment, GNP, consumption, and government and private investment from
#     1948 - III to 1988 - II.
#     The seasonal component has been removed from the data.
# ------------------------------------------------------------------------------

data(econ5, package = "astsa")

str(econ5)


car::some(econ5)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

