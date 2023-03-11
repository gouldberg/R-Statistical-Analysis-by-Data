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



# ----------
library(psych)

polcor <- polychoric(itceaq)


polcor

polcor$rho



# ------------------------------------------------------------------------------
# Determining the number of factors by nfactors()
#   - nfactors() fits a sequence of EFA models with varying the number of factors
#     and prints out several criteria.
# ------------------------------------------------------------------------------

resnf <- psych::nfactors(itceaq, n = 8, fm = "ml", cor = "poly")


resnf



