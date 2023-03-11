setwd("//media//kswada//MyFiles//R//brain")

packages <- c("dplyr", "lattice", "gamair", "mgcv")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This tutorial is based on
# Chapter 7. GAMs in Practive: mgcv from "Generalized Additive Models An Introduction with R 2nd Edition" (by Simon N. Wood)


# ------------------------------------------------------------------------------
# data:  brain
# ------------------------------------------------------------------------------
data(brain, package = "gamair")

str(brain)



# 2 voxels appear problematic, these voxels have medFPQ values recorded as 3 * 10^-6 and 4 * 10^-7, while the remaining 1565 voxels have values in the range 0.003 to 20.
# Residual plots from all attempts to model the data set including these two voxels consistently show them as grotesque outliers.

# exclude 2 outliers.
brain <- brain[brain$medFPQ > 5e-3,]



# ------------------------------------------------------------------------------
# Suppose that we are really interested in, for example, the difference between the linear predictor values at the points in the two regions.
# ------------------------------------------------------------------------------

pd <- data.frame(X = c(80.1, 68.3), Y = c(41.8, 41.8))



# ----------
# obtain prediction matrix (lpmatrix) for 2 points and predicted values
Xp <- predict(m2, newdata = pd, type = "lpmatrix")
fv <- Xp %*% coef(m2)
fv



# ----------
# The difference of predicted values on the linear scale
d <- t(c(1, -1))
d %*% fv


# also the variance of the difference
# If Xp has many rows and d was some corresponding weight vector then it is important to ensure careful use of brackets to avoid costly calculations.
(d %*% Xp) %*% vcov(m2) %*% (t(Xp) %*% t(d))



# ------------------------------------------------------------------------------
# Obtain diagonal of covariance matrix
# ------------------------------------------------------------------------------

diag(Xp %*% vcov(m2) %*% t(Xp))


