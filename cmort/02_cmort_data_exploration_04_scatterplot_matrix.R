setwd("//media//kswada//MyFiles//R//cmort")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# data:  cmort (Pollution, Temperature and Mortality)
# ------------------------------------------------------------------------------

data(cmort, package = "astsa")
data(tempr, package = "astsa")
data(part, package = "astsa")


str(cmort)
str(tempr)
str(part)


cmort
tempr
part



# ------------------------------------------------------------------------------
# data exploration:  scatterplot matrix
# ------------------------------------------------------------------------------

pairs(cbind(Temperature = tempr,  Particulates = part, Mortality = cmort))


psych::pairs.panels(cbind(Temperature = tempr,  Particulates = part, Mortality = cmort))


# spearman
psych::pairs.panels(cbind(Temperature = tempr,  Particulates = part, Mortality = cmort), method = "spearman")



# -->
# Scatterplot matrix indicates a possible linear relation between mortality and the pollutant particulates
# and a possible relation to temperature.

# Note the curvilinear shape of the temperature mortality curve, indicating that higher temperatures
# as well as lower temperatures are associated with increases in cardiovascular mortality.