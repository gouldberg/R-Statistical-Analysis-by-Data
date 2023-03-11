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
# tetrachoric correlation and estimate cut points (tau) for depression degree
# ------------------------------------------------------------------------------
# We pick two items
#   - "I am sad all the time" (item1) and "I look ugly" (item2)
#   - The items are on a three-point scale with values 0, 1, and 2.

# And for simplicity, we dichotomize the items by merging categories 1 and 2 into a single category.
item1 <- YouthDep[, 1]
levels(item1) <- c("0", "1", "1")

item2 <- YouthDep[,14]
levels(item2) <- c("0", "1", "1")


head(cbind(item1, item2))



# ----------
library(psych)


# compute the tetrachoric correlation
tetcor <- tetrachoric(cbind(item1, item2))


# ----------
tetcor


# -->
# The threshold parameters are tau1 = 1.162 and tau2 = 0.361


# ----------
# all eigenvalues are positive
tetcor$rho
eigen(tetcor$rho)$values



# ------------------------------------------------------------------------------
# polychoric correlation and estimate cut points (tau) for depression degree
# ------------------------------------------------------------------------------

item1 <- YouthDep[, 1]
item2 <- YouthDep[,14]


head(cbind(item1, item2))



# ----------
polcor <- polychoric(cbind(item1, item2))

polcor



# -->
# polychoric correlation finds more cut-points:  item1 1.16 and 2.3,  item2 0.36 and 1.2



# ------------------------------------------------------------------------------
# Using all the CDI items of the yourth depression dataset
# ------------------------------------------------------------------------------

DepItems <- YouthDep[,1:26]

head(DepItems)



# ----------
# Convert to numeric
Depnum <- data.matrix(DepItems) - 1  


head(Depnum)



# ----------
Rdep <- polychoric(Depnum)

Rdep



# -->
# Especially having small samples and/or many categories, some cells become 0, in such cases a continuity correction is applied.
# (by default the 0's are replaced by 0.5)
# Note that the dataset is fairly large (n = 2290) and only 7 cells were 0.
# In such a case, we can assume that the solution is fairly stable.


# In the area of 6, 7, or more categories, differences between Pearson and polychoric become negligible.



# ----------
# all eigenvalues are positive
Rdep$rho
eigen(Rdep$rho)
eigen(Rdep$rho)$values

