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
# compute tetrachoric correlation matrix
# ------------------------------------------------------------------------------

library(psych)

tetcor <- tetrachoric(zarsub)

tetcor

tetcor$rho



# ------------------------------------------------------------------------------
# Determining the number of factors:  eigenvalue decomposition (this means that we are fitting a PCA)
# ------------------------------------------------------------------------------

( evals <- eigen(tetcor$rho)$values )



# ----------
# eigenvalues tell us how much variance is explained by each factor (or component by PCA)
scree(tetcor$rho, factors = FALSE)


round((cumsum(evals) / sum(evals))[1:5], 5)



# -->
# Elbow:  we hope to see a clear cut point which separates the systematic structure (rock) from the random structure (scree).
# We have a strongly dominating first factor

# Kaiser (1960) suggests that eignevalues hould be larger than 1.
# The rationale behind it is that if a factor is associated with an eigenvalue smaller than 1, it accounts for less variability than a single variable does.
# Here, We should retain 2 factors.
