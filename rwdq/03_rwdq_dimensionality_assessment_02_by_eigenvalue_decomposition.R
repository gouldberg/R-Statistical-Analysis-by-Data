setwd("//media//kswada//MyFiles//R//rwdq")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  RWDQ (Work Design Questionnaire R Package Authors)
# ------------------------------------------------------------------------------

data("RWDQ", package = "MPsychoR")

str(RWDQ)


car::some(RWDQ)



# ------------------------------------------------------------------------------
# compute tetrachoric correlation matrix
# ------------------------------------------------------------------------------

library(psych)

tetcor <- tetrachoric(RWDQ)

tetcor

tetcor$rho



# ------------------------------------------------------------------------------
# Determining the number of factors:  eigenvalue decomposition (this means that we are fitting a PCA)
# ------------------------------------------------------------------------------

( evals <- eigen(tetcor$rho)$values )



# ----------
# eigenvalues tell us how much variance is explained by each factor (or component by PCA)
scree(tetcor$rho, factors = FALSE)


round((cumsum(evals) / sum(evals))[1:10], 5)



# -->
# Elbow:  we hope to see a clear cut point which separates the systematic structure (rock) from the random structure (scree).
# We have a strongly dominating first factor

# Kaiser (1960) suggests that eignevalues should be larger than 1.
# The rationale behind it is that if a factor is associated with an eigenvalue smaller than 1, it accounts for less variability than a single variable does.
# Here, We should retain 2 factors.
