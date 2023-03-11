setwd("//media//kswada//MyFiles//R//wilpat")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  WilPat
# ------------------------------------------------------------------------------

data("WilPat", package = "MPsychoR")

str(WilPat)



# ----------
wpit15 <- WilPat[,1:15]

names(wpit15)



# ------------------------------------------------------------------------------
# Assess unidimensionality
#   - fitting a 2-dimensional Princals in order to get a picture of item associations in a 2D space.
# ------------------------------------------------------------------------------

library(Gifi)

wpiprin <- princals(wpit15, ordinal = FALSE)

wpiprin



# ----------
# plot loadings
plot(wpiprin, main = "WilPat15 Loadings")



# -->
# If the items were unidimensional, the arrows should approximately point in the same direction.
# 4 items are different from others:  "Nationalism", "Patriotism", "ChurchAuthority", "Obedience" 



# ----------
summary(wpiprin)



# -->
# The first dimension discriminates between responses 1 and 2
# The second dimension is mostly determined by the 0 responses.


