setwd("//media//kswada//MyFiles//R//crimes")

packages <- c("dplyr", "smacof", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  crimes
#  - The correlations of different crimes in 50 US states.
#    THe correlations show, for example, that if there are many cases of Assault in a state, then there are also many cases of Murder (r = 0.81)
# ------------------------------------------------------------------------------
data("crimes", package = "smacof")

str(crimes)

car::some(crimes)



# ------------------------------------------------------------------------------
# basics:  
# ------------------------------------------------------------------------------

