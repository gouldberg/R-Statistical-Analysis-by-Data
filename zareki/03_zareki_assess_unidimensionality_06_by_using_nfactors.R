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



# ----------
# compute tetrachoric correlation matrix

library(psych)

tetcor <- tetrachoric(zarsub)

tetcor

tetcor$rho



# ------------------------------------------------------------------------------
# Determining the number of factors by nfactors()
#   - nfactors() fits a sequence of EFA models with varying p and prints out several criteria.
# ------------------------------------------------------------------------------

resnf <- psych::nfactors(zarsub, n = 8, fm = "ml", cor = "tet")


resnf



