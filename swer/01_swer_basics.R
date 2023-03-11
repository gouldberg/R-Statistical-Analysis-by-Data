setwd("//media//kswada//MyFiles//R//swer")

packages <- c("dplyr", "lattice", "gamair")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This tutorial is based on
# Chapter 7. GAMs in Practive: mgcv from "Generalized Additive Models An Introduction with R 2nd Edition" (by Simon N. Wood)


# ------------------------------------------------------------------------------
# data:  Extreme rainfall in Switzerland
#   - The data contains the highest rainfall recorded in any 12-hour period each year for 65 Swiss weather stations, with covariates, from 1981 to 2015.
#   - Variable
#       - year:  the year of observation
#       - exra:  the highest rainfall observed in any 12 hour period in that year, in mm.
#       - nao:  Annual North Atlantic Oscillation Index, based on the difference of normalized sea level pressure (SLP)
#               Positive values are generally associated with wetter and milder weather over Western Europe.
#       - location:  the measuring station location name.
#       - code:  three letter code identifying the station
#       - elevation:  metres above sea level
#       - climate.region:  one of 12 distinct climate regions
#       - N:  Degrees north  E:  Degrees east
# ------------------------------------------------------------------------------

data(swer, package = "gamair")

str(swer)


head(swer)

car::some(swer)




# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

