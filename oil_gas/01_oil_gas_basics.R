
packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Oil and Gas
#   - The oil series is in dollars per barrel, the gas series is in cents per gallon
# ------------------------------------------------------------------------------

data(oil, package = "astsa")

data(gas, package = "astsa")

str(oil)

str(gas)



oil

gas



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

