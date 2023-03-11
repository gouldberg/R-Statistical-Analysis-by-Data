setwd("//media//kswada//MyFiles//R//tileries")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Tileries
#  - Data on output and labor and capital inputs for 25 tileries in two regions of Egypt,
#    concerning the weekly production of cement floor tiles for 25 Egyptian small-scale tileries in 1982 - 1983.
#    The data are observed over 66 weeks in total, and aggregated on periods of three weeks.
#    The number of observations for each firm ranges from 12 to 22
# ------------------------------------------------------------------------------

data("Tileries", package = "pder")


str(Tileries)


dim(Tileries)


car::some(Tileries)

  

# ------------------------------------------------------------------------------
# basics:  
# ------------------------------------------------------------------------------


