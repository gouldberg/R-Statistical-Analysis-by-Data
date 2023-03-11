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
# Isotropic or tensor product smooth
#   - Isotropic smooth:  good choice when the covariates of the smooth are naturally on the same scale
#   - Tensor product smooth:  scale invariant
#
#   - Such smooths are computationally rather efficient
#   - They also provide another way of comparing an additive model to a bivariate smooth model via the ti interaction term construction
# ------------------------------------------------------------------------------

# single tensor product smooth
tm <- gam(medFPQ ~ te(Y, X, k = 10), data = brain, family = Gamma(link = log))



# ----------
# additive plus interaction term
# single s terms could have been replaced by terms ti(X, k = 10) resulting in the same model fit.
tm1 <- gam(medFPQ ~ s(Y, k = 10, bs = "cr") + s(X, bs = "cr", k = 10) + ti(X, Y, k = 10), data = brain, family = Gamma(link = log))



# ----------
AIC(m2, tm, tm1)



# -->
# AIC comparison slightly favours the main effects plus interaction model over the single tensor product smooth.
# For tm and tm1, they have the same smoothing basis, but simply partitioned differently.
# tm1 has extra separate penalties on the part of the basis representing the smooth main effects f1(Yi) + f2(Xi)


# ----------
anova(tm1)


#-->
# The p-value for ti(X, Y) clearly suggests that the smooth term is significantly non zero:
# The interaction is needed and an additive structure alone is insufficient.



# ----------
vis.gam(tm1, plot.type = "contour", too.far = 0.03, color = "gray", n.grid = 60, zlim = c(-1, 2))












