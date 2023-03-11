setwd("//media//kswada//MyFiles//R//swer")

packages <- c("dplyr", "lattice", "gamair")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This tutorial is based on
# Chapter 7. GAMs in Practive: mgcv from "Generalized Additive Models An Introduction with R 2nd Edition" (by Simon N. Wood)


# ------------------------------------------------------------------------------
# data:  Extreme rainfall in Switzerland
# ------------------------------------------------------------------------------

data(swer, package = "gamair")

str(swer)


head(swer)

car::some(swer)



# ------------------------------------------------------------------------------
# Data Exploration:  data transformation for "exra"
# ------------------------------------------------------------------------------

# check density
car::densityPlot( ~ exra, data = swer)



# transforming for symmetry
car::symbox(~ exra, data = swer)



# check Box-Cox power family transformation
# Rounded Pwr is the first value among {1, 0, -1, 0.5, 0.33, -0.5, -0.33, 2, -2} that is included in the confidence interval for lambda
# The test for the log transformation has a very large p-value, indicating that the log transformation is consistent with the data,
# while the tiny p-value for lambda = 1 indicates that leaving "body" untransformed is inconsistent with the goal of making the variabel normally distributed.
p1 <- car::powerTransform(exra ~ 1, data = swer, family = "bcnPower")

summary(p1)



# should not use lambda = -0.65
car::testTransform(p1, lambda = -0.65)



# should use lambda = 0.33  ??
car::testTransform(p1, lambda = 0.33)



# ----------
tmp <- swer %>% mutate(exra2 = exra ^ (-0.65))

car::densityPlot( ~ exra2, data = tmp)



