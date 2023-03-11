setwd("//media//kswada//MyFiles//R//cmort")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  cmort (Pollution, Temperature and Mortality)
#   - Data is extracted series from a study by Shumway et al. of the possible effects of temperature and pollution on weekly mortality in
#     Los Angeles County.
# ------------------------------------------------------------------------------

data(cmort, package = "astsa")


str(cmort)


cmort




# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

