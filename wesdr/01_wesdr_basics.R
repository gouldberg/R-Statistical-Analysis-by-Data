setwd("//media//kswada//MyFiles//R//wesdr")

packages <- c("dplyr", "lattice", "gamair")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This tutorial is based on
# Chapter 7. GAMs in Practive: mgcv from "Generalized Additive Models An Introduction with R 2nd Edition" (by Simon N. Wood)


# ------------------------------------------------------------------------------
# data:  wesdr
#   - Clinical data on whether diabetic patients suffer from diabetic retinopathy or not
#   - 3 possible predictors:
#       - duration of disease (years)
#       - body mass index (mass in kg divided by square of height in metres)
#       - percent glycosylated hemoglobin in the blood
#          - the percentage of hemoglobin bound to glucose, which reflects long term average blood glucose levels and should be below 6% for non-diabetics.
# ------------------------------------------------------------------------------
data(wesdr, package = "gamair")

str(wesdr)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

par(mfrow=c(1, 3))
boxplot(dur ~ ret, data = wesdr)
boxplot(gly ~ ret, data = wesdr)
boxplot(bmi ~ ret, data = wesdr)


# -->
# The relationship of risk to the covariates is somewhat unclear.
