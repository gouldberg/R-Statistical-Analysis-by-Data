setwd("//media//kswada//MyFiles//R//youthdep")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  YouthDep
# ------------------------------------------------------------------------------

data("YouthDep", package = "MPsychoR")

str(YouthDep)

dim(YouthDep)



# ----------
# item15: "I am bad all the time"
# item21: "I never have fun at school"
# each of them scored on three categories.
cdisub <- YouthDep[,c("CDI15r", "CDI21r", "race")]



# ------------------------------------------------------------------------------
# Create Burt matrix
#   - Burt matrix is symmetric and contains all possible 2-way cross classifications of the m variables involved.
#     Burt matrix can be treated as a standard contingency table, and we can compute the residual matrix S.
#     Later we can decompose S by either an eigenvalue decomposition or an SVD. Since S is symmetric, both approaches lead to the same results.
# ------------------------------------------------------------------------------

# Create Burt matrix by anacor package
# In total, we have ten categories, and Burt matrix (B) will be of dimension 10 * 10
B <- anacor::burtTable(cdisub)

dim(B)

B



# ------------------------------------------------------------------------------
# Fit a Multiple Correspondence Analysis based on Burt matrix in order to obtain the category scores
# ------------------------------------------------------------------------------

library(ca)


# ca package create internally Burt matrix.
fit_mca <- mjca(cdisub, lambda = "Burt")

summary(fit_mca)



# -->
# By considering two dimensions, we explain 42.64% of the inertia.



# ----------
# Symmetric map
plot(fit_mca, xlim = c(-0.5, 0.5))


# -->
# Regarding the race variable, we see a separation along dimension 1 between Latinos and Black kids on the one hand
# and Asian and White kids on the other hand.

# The order of the response categories of the CDI items along the first dimension is retained.
# Asian/White kids tend to score zero on these items, whereas Latino/Black kids tend to score 1

# As in simple CA, we have to be careful with making such inter-variable interpretations based on this plot.



