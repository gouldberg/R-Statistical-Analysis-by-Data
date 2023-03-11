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
# Would an additive structure be better (more parsimnious ?)
# ------------------------------------------------------------------------------

m3 <- gam(medFPQ ~ s(Y, k = 30) + s(X, k = 30), data = brain, family = Gamma(link=log))

m3



summary(m2)
summary(m3)



# -->
# The GCV score is higher for this model,
# suggesting that it is not an improvement, and a comparison of explained deviances is substantially worse.



# ----------
# compare bivariate smooth adn additive model
AIC(m2, m3)



# ----------
vis.gam(m3, plot.type = "contour", too.far = 0.03, color = "gray", n.grid = 60, zlim = c(-1, 2))


# -->
# The additive structure produces horizontal and vertical "stripes" in the plot that have no real support from the data.


