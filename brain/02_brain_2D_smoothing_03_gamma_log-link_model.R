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
# Gamma model (use a log link)
# ------------------------------------------------------------------------------

m2 <- gam(medFPQ ~ s(Y, X, k = 100), data = brain, family = Gamma(link=log))


gam.check(m2)


# -->
# The informal basis dimension test still gives a low p-value, but since the EDF is some way below the basis dimension,
# and increasing the basis dimension barely changes the fitted values, let's stick with k = 100 here.

# The lower right plot, of response against fitted values, shows a positive linear relationship with a good deal of scatter:  nothing problematic.




# ------------------------------------------------------------------------------
# Compare m1 and m2
# ------------------------------------------------------------------------------

mean(fitted(m1)^4)
mean(fitted(m2))
mean(brain$medFPQ)



# -->
# m1 tends to under-estimate and m2 substantially better.

# The major difference between m1 and m2 is in their biasedness on different scales.
# The model of the transformed data is approximately unbiased on the 4th root of the response scale.
# This means that it is biased downwards on the response scale itself.

# The log-gamma model is approximately unbiased on the response scale
# (only approximately because maximum penalized likelihood estimation is not generally unbiased, but is consistent)

# Clearly, if the response scale is the scale of prime interest then the gamma model is to be preferred to the model based on normality of the transformed data.



# ------------------------------------------------------------------------------
# Examine m2
# ------------------------------------------------------------------------------

m2

summary(m2)


# -->
# A relatively complex fitted surface has been estimated, with 62 (total 61.61) degrees of freedom.



# ----------
# vis.gam() provides quite usefule facilities for plotting predictions from a gam fit against pairs of covariates,
# either as coloured perspective plots, or as coloured (or grey scale) contour plots

# too.fat:  plot grid nodes that are too far from the points defined by the variables given in view can be excluded from the plot.
# too.far determines what is too far.
# The grid is scaled into the unit square along with the view variables and then grid nodes more than too.far from the predictor variables are excluded.
par(mfrow=c(1,1))
vis.gam(m2, plot.type = "contour", too.far = 0.03, color = "gray", n.grid = 60, zlim = c(-1, 2))
