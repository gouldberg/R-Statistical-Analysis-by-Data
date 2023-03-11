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
# Determining the number of factors:  very simple structure (VSS)
#   - The higher the value of VSS, the better the simple loadings structure represents our data.
# ------------------------------------------------------------------------------

resvss <- VSS(tetcor$rho, fm = "ml", n.obs = nrow(zarsub), plot = FALSE)


resvss



# -->
# c: complexity, denoting the amount of non-zero loadings of a variable in the model fit.
# By default the VSS function prints out the maximum VSS values for c = 1 and c = 2
# For c = 1 and c = 2, it suggests a two-factor solution is a good choice.

# MAP: minimum average partial, the basic idea is to partial out the factors/components from the input correlation matrix R.
#      The smaller the partial correlations (and therefore the sum-of-squares), the smaller the MAP and the better fit.
# RMSR: root mean squared residual, the smaller the RMSR, the better the fit.

# RMSEA: root mean squared error of approximation, which is based on the discrepancy function of the ML estimation.
#        values smaller than 0.05 indicate good fit, 0.06-0.08 fair fit, and values larger than 0.10 poor fit


plot(resvss)



# -->
# MAP suggests 1 factor
# VSS suggests 2 factors
# BIC suggests 3 factors !!!
