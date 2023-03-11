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
# Rotate solutions
# ------------------------------------------------------------------------------

# We can also request to apply an orthogonal or non-orthogonal rotation for better interpretability
summary(zar2d, rotate = "varimax")



summary(zar2d, rotate = "oblimin")


