setwd("//media//kswada//MyFiles//R//zareki")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  zareki
# ------------------------------------------------------------------------------

data("zareki", package = "MPsychoR")

str(zareki)



# We consider eight binary subtraction items only
zarsub <- zareki[, grep("subtr", colnames(zareki))]

str(zarsub)



# ------------------------------------------------------------------------------
# Assess unidimensionality
#   - fitting a 2-dimensional Princals in order to get a picture of item associations in a 2D space.
# ------------------------------------------------------------------------------

library(Gifi)

prinzar <- princals(zarsub)

prinzar



# ----------
# plot loadings
plot(prinzar, main = "Zareki Loadings")



# -->
# If the items were unidimensional, the arrows should approximately point in the same direction.
# We see that subtr5 is somewhat separated fomr the rest, whereas the remaining ones look fairly homogeneous.
# This plot suggested that unidimensionality might be violated due to subtr5
