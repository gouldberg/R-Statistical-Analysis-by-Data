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
# Less extreme 4th root transform
# ------------------------------------------------------------------------------

# Alternatively the apparent mean-variance relationship suggests using a log transform of medFPQ to stabilize the variance,
# but in practice a less extreme 4th root transform produces better residual plots.

m1 <- gam(medFPQ^.25 ~ s(Y, X, k = 100), data = brain)


# log transformation
m1_2 <- gam(log(medFPQ) ~ s(Y, X, k = 100), data = brain)



# -----------
summary(m1)
summary(m1_2)



# ----------
graphics.off()
par(mfrow = c(2,2))

gam.check(m1)

gam.check(m1_2)



# -->
# m1:  Now we can see better residual plots
# edf = 64.5
