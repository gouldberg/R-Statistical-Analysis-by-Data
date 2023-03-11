setwd("//media//kswada//MyFiles//R//chicago")

packages <- c("dplyr", "lattice", "gamair")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This tutorial is based on
# Chapter 7. GAMs in Practive: mgcv from "Generalized Additive Models An Introduction with R 2nd Edition" (by Simon N. Wood)


# ------------------------------------------------------------------------------
# data:  chicago
# ------------------------------------------------------------------------------
data(chicago, package = "gamair")

str(chicago)



# ------------------------------------------------------------------------------
# Poission GAM
#  - A conventional approach to modelling these data would be to assume that the observed numbers of deaths are Poisson random variables,
#    with an underlying mean that is the product of a basic, time varying, death rate, modified through multiplication by pollution dependent effects.
# ------------------------------------------------------------------------------

# A cubic regression spline is ussed for f to spped up computation a little.
ap0 <- gam(death ~ s(time, bs = "cr", k = 200) + pm10median + so2median + o3median + tmpd, data = chicago, family = poisson)



# basic model check
gam.check(ap0)


# -->
# For Poisson data with moderately high means, the distribution of the standardized residuals should be quite close to normal,
# so that the QQ-plot is obviously problematic.
# As all the plots make clear, there are a few gross outliers that are very problematic in this fit.


summary(ap0)
summary(mod_glm)


# -->
# here so2median is not significant



# ------------------------------------------------------------------------------
# Check outliers 
# ------------------------------------------------------------------------------

# plot the estimated smooth with and without partial residuals emphasises the size of the outliers
par(mfrow = c(2, 1), mar = c(2,2,2,2))
plot(ap0, n = 1000)
plot(ap0, residuals = TRUE, n = 1000, pch = 2)


# -->
# 4 gross outliers, in close proximity to each other, are clearly visible.


# ----------
chicago$death[3111:3125]


# -->
# Outliers are the 4 highest daily death rates occuring in the data. They occurred on consecutive days.


