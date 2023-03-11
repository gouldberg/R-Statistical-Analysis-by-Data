setwd("//media//kswada//MyFiles//R//grip")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  grip
#  - The data are anlyzed by Cohen et al.
#    The response variable is hand grip (HG) strength, with explanatory variables gender and age,
#    in English schoolchildren
#  - Variables:
#       - age:  age of the participant
#       - grip:  hand grip strength
# ------------------------------------------------------------------------------

data("grip", package = "gamlss.data")


str(grip)

car::some(grip)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------


lattice::xyplot(grip ~ age, data = grip, type = c("g", "p", "smooth"), col.line = "black", pch = 20, col = gray(0.8))



# -->
# Since the age range is very narrow, there is no need to power transform age in these data.