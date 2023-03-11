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
# Test whether the underlying activity levels are symmetric about the mean X value of 64.5 ?
# ------------------------------------------------------------------------------

brain$Xc <- abs(brain$X - 64.5)

brain$right <- as.numeric(brain$X < 64.5)



# ----------
# symmetry model
m.sy <- gam(medFPQ ~ s(Y, Xc, k = 100), data = brain, family = Gamma(link = log))


# asymmetry model
m.as <- gam(medFPQ ~ s(Y, Xc, k = 100) + s(Y, Xc, k = 100, by = right), data = brain, family = Gamma(link = log))



# ----------
m.sy

m.as


AIC(m.sy, m.as)


# -->
# GCV scores suggest that the asymmetric model is better, and the asymmetric model AIC is also 97 lower than the symmetric model


# ----------
anova(m.as)



# -->
# If the symmetric model was adequate then the s(Y, Yc):right term should not be significantly differnt from zero,
# which is far from being the case here.


# the less well-justified approach ..
anova(m.sy, m.as)



# ----------
par(mfrow=c(1,3))
vis.gam(m.sy, plot.type = "contour", view = c("Xc", "Y"), too.far = 0.03, color = "gray", n.grid = 60, zlim = c(-1, 2), main = "both sides")
vis.gam(m.as, plot.type = "contour", view = c("Xc", "Y"), too.far = 0.03, color = "gray", n.grid = 60, zlim = c(-1, 2), main = "left side", cond = list(right = 0))
vis.gam(m.as, plot.type = "contour", view = c("Xc", "Y"), too.far = 0.03, color = "gray", n.grid = 60, zlim = c(-1, 2), main = "right side", cond = list(right = 1))


# -->
# Plots of different model predictions help to show why symmetry is so clearly rejected.


