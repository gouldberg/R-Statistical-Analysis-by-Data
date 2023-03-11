setwd("//media//kswada//MyFiles//R//co2s")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This tutorial is based on Chapter 7. Introductin GAMs from "Generalized Additive Models An Introduction with R 2nd Edition" (by Simon N. Wood)


# ------------------------------------------------------------------------------
# data:  co2s
#   - Atmospheric CO2 at south pole from January 1957 onwards.
#   - Variables:
#       - co2:  atmospheric CO2 concentration in parts per million
#       - c.month:  cumulative number of months since Jan 1957
#       - month:  month of year
#   - There are missing co2 observations in some months
# ------------------------------------------------------------------------------

data(co2s, package = "gamair")

str(co2s)




# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

plot(co2 ~ c.month, data = co2s, type = "l")


