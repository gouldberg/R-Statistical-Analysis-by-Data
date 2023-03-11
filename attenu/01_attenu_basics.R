setwd("//media//kswada//MyFiles//R//attenu")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  attenu
#   - data on peak accelerations measure at various observation stations for 23 earthquakes in California.
#     The data has been used by various workers to estimate the attenuating effect of distance on ground acceleration
#   - variables:
#       - event:  Event number
#       - mag:  moment magnutude
#       - station:  station number
#       - dist:  station-hypocenter distance (km)
#       - accel:  peak acceleration (g)
# ------------------------------------------------------------------------------

data("attenu")


str(attenu)

dim(attenu)


car::some(attenu)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

