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
# Compute M2 statistic
# ------------------------------------------------------------------------------

# M2 statistic including the CFA/SEM fit indices

M2(zar2d)



# -->
# We get a low RMSEA (here we use 0.05/2 as cutoff since we have binary items) and a high CFI
# and the M2 p-value is not significant.

# The 2D model fits well.



