setwd("//media//kswada//MyFiles//R//zareki")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  zareki
# ------------------------------------------------------------------------------

data("zareki", package = "MPsychoR")

str(zareki)



# using all binary ZAREKI addition and subtraction items
itzareki <- zareki[, 1:16]




# ------------------------------------------------------------------------------
# Item Characteristic Surface (ICS)
# ------------------------------------------------------------------------------

itemplot(zar2d, 3, main = "ICS addit3", rot = list(xaxis = -70, yazis = 50, zaxis = 10))



