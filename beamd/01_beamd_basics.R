setwd("//media//kswada//MyFiles//R//beamd")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  beamd
#   - Infrasonic or low-frequency acoustic signal from a nuclear explosion
#     detonated 25 km south of Christmas Island and the delayed average or beam.
#     The time scale is 10 points per second.
#   - The signal is observed on a small triangular array of N = 3 acoustic sensors.
# ------------------------------------------------------------------------------

data(beamd, package = "astsa")

str(beamd)

head(beamd)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

