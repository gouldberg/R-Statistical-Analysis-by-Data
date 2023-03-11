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
# Poission GLM
#  - A conventional approach to modelling these data would be to assume that the observed numbers of deaths are Poisson random variables,
#    with an underlying mean that is the product of a basic, time varying, death rate, modified through multiplication by pollution dependent effects.
# ------------------------------------------------------------------------------

mod_glm <- glm(death ~ time + pm10median + so2median + o3median + tmpd, data = chicago, family = poisson)


summary(mod_glm)



# ------------------------------------------------------------------------------
# Check outliers 
# ------------------------------------------------------------------------------

par(mfrow = c(1, 1))
plot(mod_glm)


# -->
# 4 gross outliers, in close proximity to each other, are clearly visible.


# ----------
chicago$death[3111:3125]


# -->
# Outliers are the 4 highest daily death rates occuring in the data. They occurred on consecutive days.
