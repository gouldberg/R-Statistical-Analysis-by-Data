setwd("//media//kswada//MyFiles//R//globtemp")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  globtemp
#   - global temperature series. The data are the global mean land-ocean temperature index from 1880 to 2015, with the base period 1951 - 1980.
#     In particular, the data are deviations, measured in degrees centigrade, from the 1951 - 1980 average, and are an update of Hansen et al.
# ------------------------------------------------------------------------------

data(globtemp, package = "astsa")

str(globtemp)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------


plot(globtemp, type = "o", ylab = "Global Temperature Deviations")




# -->
# We note an apparent upward trend in the series during the latter part of the twentieth century
# that has been used as an argument for the global warming hypothesis.
# Note also the leveling off at about 1935 and then another rather shaper upward trend at about 1970.

# The question of interest for global warming proponents and opponents is whether the overall trend is natural or 
# whether it is caused by some human-induced interface.
