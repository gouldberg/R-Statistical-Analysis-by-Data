setwd("//media//kswada//MyFiles//R//air_passengers")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Air Passengers
#   - Monthly totals of international airline passengers, 1949 to 1960, taken from Box and Jenkins.
# ------------------------------------------------------------------------------

data(AirPassengers, package = "datasets")

str(AirPassengers)

head(AirPassengers)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

