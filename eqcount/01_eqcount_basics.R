setwd("//media//kswada//MyFiles//R//eqcount")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  eqcount
#   - Time series of annual counts of major earthquakes (magnitude 7 and above), which were discussed in Zucchini and MacDonald.
# ------------------------------------------------------------------------------

data(EQcount, package = "astsa")


str(EQcount)


EQcount



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------


psych::describe(EQcount)

