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
# Determining the number of factors:  very simple structure (VSS)
#   - The higher the value of VSS, the better the simple loadings structure represents our data.
# ------------------------------------------------------------------------------

resvss <- VSS(Rdep$rho, fm = "ml", n.obs = nrow(Depnum), plot = FALSE)


resvss



# -->
# c: complexity, denoting the amount of non-zero loadings of a variable in the model fit.
# By default the VSS function prints out the maximum VSS values for c = 1 and c = 2
# For c = 1 it suggests a one-factor solution, whereas for c = 2 a two-factor solution is a good choice.

# MAP: minimum average partial, the basic idea is to partial out the factors/components from the input correlation matrix R.
#      The smaller the partial correlations (and therefore the sum-of-squares), the smaller the MAP and the better fit.
# RMSR: root mean squared residual, the smaller the RMSR, the better the fit.

# RMSEA: root mean squared error of approximation, which is based on the discrepancy function of the ML estimation.
#        values smaller than 0.05 indicate good fit, 0.06-0.08 fair fit, and values larger than 0.10 poor fit


plot(resvss)


