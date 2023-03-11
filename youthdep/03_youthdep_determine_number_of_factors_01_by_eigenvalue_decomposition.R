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
# Determining the number of factors:  eigenvalue decomposition (this means that we are fitting a PCA)
# ------------------------------------------------------------------------------

# eigenvalues
( evals <- eigen(Rdep$rho)$values )


# eigenvalues tell us how much variance is explained by each factor (or component by PCA)
scree(Rdep$rho, factors = FALSE)



# -->
# Elbow:  we hope to see a clear cut point which separates the systematic structure (rock) from the random structure (scree).
# We have a strongly dominating first factor

# Kaiser (1960) suggests that eignevalues should be larger than 1.
# The rationale behind it is that if a factor is associated with an eigenvalue smaller than 1, it accounts for less variability than a single variable does.
# Here, We should retain 4 factors.


# ----------
round((evals / sum(evals))[1:10], 5)


# The first factor already explains around 41.1% of the variance in the data, the second factor additional 6.7%

