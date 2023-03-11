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
# Itemfit statistics
# ------------------------------------------------------------------------------

ifit2D2pl <- mirt::itemfit(zar2d)


ifit2D2pl



# ----------
# misfitting items
ifit2D2pl[ifit2D2pl[, 5] < 0.05, ]



# -->
# subtr4 item could be eliminated since it shows some misfit, and then the model needs to be refitted again.
# However, since the global model fit suggested a well-fitting solution, let us keep it.

