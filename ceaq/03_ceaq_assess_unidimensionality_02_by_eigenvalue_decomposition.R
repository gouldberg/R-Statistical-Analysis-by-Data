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
# compute polychoric correlation matrix
# ------------------------------------------------------------------------------

library(psych)

polcor <- polychoric(itceaq)


polcor

polcor$rho




# ------------------------------------------------------------------------------
# Determining the number of factors:  eigenvalue decomposition (this means that we are fitting a PCA)
# ------------------------------------------------------------------------------

( evals <- eigen(polcor$rho)$values )



# ----------
# eigenvalues tell us how much variance is explained by each factor (or component by PCA)
scree(polcor$rho, factors = FALSE)


round((cumsum(evals) / sum(evals))[1:5], 5)



# -->
# Elbow:  we hope to see a clear cut point which separates the systematic structure (rock) from the random structure (scree).
# We have a strongly dominating first factor

# Kaiser (1960) suggests that eignevalues hould be larger than 1.
# The rationale behind it is that if a factor is associated with an eigenvalue smaller than 1, it accounts for less variability than a single variable does.
# Here, We should retain 2 factors.
