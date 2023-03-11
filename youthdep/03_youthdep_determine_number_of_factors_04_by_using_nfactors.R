setwd("//media//kswada//MyFiles//R//youthdep")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  YouthDep
# ------------------------------------------------------------------------------

data("YouthDep", package = "MPsychoR")

str(YouthDep)

dim(YouthDep)



# ------------------------------------------------------------------------------
# Compute polychoric correlation matrix
# ------------------------------------------------------------------------------

# Using all the CDI items of the yourth depression dataset
DepItems <- YouthDep[,1:26]


# Convert to numeric
Depnum <- data.matrix(DepItems) - 1  



# ----------
# polychoric correation
Rdep <- polychoric(Depnum)

Rdep



# polychoric correation matrix
Rdep$rho



# ------------------------------------------------------------------------------
# Determining the number of factors by nfactors()
#   - nfactors() fits a sequence of EFA models with varying p and prints out several criteria.
#     Apply the very simple structure, MAP, and other criteria to determine the appropriate number of factors
# ------------------------------------------------------------------------------

resnf <- psych::nfactors(Depnum, n = 8, fm = "ml", cor = "poly")


resnf



