setwd("//media//kswada//MyFiles//R//privacy")

packages <- c("dplyr", "MPsychoR", "corrplot", "BayesFM")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Privacy
# ------------------------------------------------------------------------------

data("Privacy", package = "MPsychoR")


str(Privacy)



# ----------
Privstd <- scale(Privacy)

head(Privstd)



# ------------------------------------------------------------------------------
# Robust version of PCA
# ------------------------------------------------------------------------------

# If our input data contain outliers, a robust version of PCA can be considered.
# We can use the princomp function with a robust covariance matrix

set.seed(123)

pcarob <- princomp(covmat = MASS::cov.rob(Privacy), cor = TRUE)

summary(pcarob)

pcarob$loadings[,1:3]



# -->
# Since this dataset did not have any strong outliers, the results are approximately the same as for the regular PCA fit.
# Cumulative Proportion up to PC3 is 0.66 (higher than regular PCA fit 0.59)



# ------------------------------------------------------------------------------
# Sparse PCA
#   - For a fixed p, sparse PCA shrinks small loadings to 0 using the lasso principle
# ------------------------------------------------------------------------------

library("elasticnet")


# Through para argument, we can tell the algorithm how many non-zero loadings we want to have on each component.
# In this case, we want to have 3 non-zero loadings on PC1 and PC3 and 4 non-zero loadings on PC2.
# Alternatively, one could also specify a vector of penalty parameters.
spcaPriv <- spca(scale(Privacy), K = 3, sparse = "varnum", para = c(3 ,4 ,3))

spcaPriv



# -->
# Compared to the standard PCA fit from above, sparse PCA leads to a more restricted solution.
# In total, we explain 55.01% of the variance. This is only slightly less than the 59% from the standard PCA fit above,
# but the solution is much easier to interpret.






