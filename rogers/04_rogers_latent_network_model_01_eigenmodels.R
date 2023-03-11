setwd("//media//kswada//MyFiles//R//rogers")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Rogers
# ------------------------------------------------------------------------------

data("Rogers", package = "MPsychoR")

str(Rogers)

dim(Rogers)

car::some(Rogers)



# ------------------------------------------------------------------------------
# Latent Network Models:  eigenmodels
# ------------------------------------------------------------------------------

library(eigenmodel)


# NA diagonals required
diag(cormat) <- NA



# Here no need to blank out low correlations
# fit p = 2 dimensional model and use 1000 MCMC iterations with a burn-in of 200 steps
fitEM <- eigenmodel_mcmc(cormat, R = 2, S = 1000, burn = 200, seed = 123)



# ------------------------------------------------------------------------------
# Posterior means of eigenvalues
# ------------------------------------------------------------------------------

evals <- colMeans(fitEM$L_postsamp)

# posterior means of the eigenvalues
evals



# -->
# Unlike in PCA, the eigenvalues are not of decreasing order since they were estimated using MCMC.
# We see that the second eigenvalue dominates the solution.
# Thus, the second latent variable (or dimension) is relatively more important than the first one.



# ------------------------------------------------------------------------------
# Plot the solution
# ------------------------------------------------------------------------------

evecs <- eigen(fitEM$ULU_postmean)$vec[,1:2]

plot(evecs, type = "n", xlab = "Dimension 1", ylab = "Dimension 2", xlim = c(-0.30, 0),
     main = "Depression / OCD Eigenmodel Network")


# correlation threshold
corthresh <- 0.2

addlines(evecs, abs(cormat) > corthresh, col = "gray")

ind <- c(rep(1, 16), rep(2, 10))


cols <- c("coral", "cadetblue")
text(evecs, labels = rownames(cormat), col = cols[ind], cex = 0.8)

legend("topright", legend = c("Depression", "OCD"), col = cols, pch = 19)



# -->
# We see the clear separation between OCD and depression itemsets along dimension 2.
# This confirms what the eigenvalue above told us: the second dimension is the more important one.
# Dimension 1 mostly discriminates between the "core" depression items and the "periphery" depression items,
# as already identified in the correlation networks above.






