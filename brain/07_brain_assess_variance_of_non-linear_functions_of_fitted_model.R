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
# Variances of non-linear functions of the fitted model
# ------------------------------------------------------------------------------

ind <- brain$region == 1 & ! is.na(brain$region)

Xp <- predict(m2, newdata = brain[ind,], type = "lpmatrix")



# ----------
# large number of replicate parameter sets are simulated from the posterior distribution of beta, using rmvn function
# mgcv::rmvn() generates multivariate normal random deviates.
# Each column of the matrix br is a replicate parameter vector, drawn from the approximate posterior distribution of beta.
set.seed(8)


br <- rmvn(n = 1000, coef(m2), vcov(m2))



# ----------
# obtain the linear predictor implied by each replicate and the required averages
mean.FPQ <- rep(0, 1000)

for(i in 1:1000){
  lp <- Xp %*% br[i,]
  mean.FPQ[i] <- mean(exp(lp))
}


# more efficiently but less readably
mean.FPQ <- colMeans(exp(Xp %*% t(br)))


# ----------
hist(mean.FPQ)



# -->
# This simulation approach is very computationally efficient in comparison to bootstrapping.
# One disadvantage is that the smoothing parameters are treated as fixed at their estimates, rather than as uncertain.



# ------------------------------------------------------------------------------
# Use smoothing parameter uncertainty correction
# (not treated as fixed)
# ------------------------------------------------------------------------------

# unconditional = TRUE:  smoothing parameter uncertainty correction
br2 <- rmvn(n = 1000, coef(m2), vcov(m2, unconditional = TRUE))



# ----------
# obtain the linear predictor implied by each replicate and the required averages
mean.FPQ2 <- rep(0, 1000)

for(i in 1:1000){
  lp <- Xp %*% br2[i,]
  mean.FPQ2[i] <- mean(exp(lp))
}


# more efficiently but less readably
mean.FPQ2 <- colMeans(exp(Xp %*% t(br2)))



# ----------
par(mfrow=c(1,2))
hist(mean.FPQ)
hist(mean.FPQ2)


