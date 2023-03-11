setwd("//media//kswada//MyFiles//R//ceaq")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  CEAQ
# ------------------------------------------------------------------------------

data("CEAQ", package = "MPsychoR")


str(CEAQ)


car::some(CEAQ)



# ----------
# set the lowest category to zero in order to make it eRm compatible
itceaq <- CEAQ[,1:16] - 1




# ------------------------------------------------------------------------------
# Assess unidimensionality
#   - fitting a 2-dimensional Princals in order to get a picture of item associations in a 2D space.
# ------------------------------------------------------------------------------

library(Gifi)


prin <- princals(itceaq)


prin



# ----------
# plot loadings
plot(prin, main = "CEAQ Loadings")



# -->
# If the items were unidimensional, the arrows should approximately point in the same direction.
# This plot suggested that unidimensionality might be violated


# Component 1 for ceaq10 and ceaq15 have quite different values compared to others
#             for ceaq14 is smaller than others

# Component 2 for ceaq14 and ceaq9 are almost close to zero

