setwd("//media//kswada//MyFiles//R//sitka")

packages <- c("dplyr", "lattice", "SemiPar")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This tutorial is based on
# Chapter 7. GAMs in Practive: mgcv from "Generalized Additive Models An Introduction with R 2nd Edition" (by Simon N. Wood)


# ------------------------------------------------------------------------------
# data:  sitka
# ------------------------------------------------------------------------------
data(sitka, package = "SemiPar")


str(sitka)


car::some(sitka)



# ------------------------------------------------------------------------------
# Separate random curve for each tree, in addition to the main average growth curve
#   - These random curves would have no un-penalized null space, instead being shrunk towards zero.
# ------------------------------------------------------------------------------

sitka$id.num <- as.factor(sitka$id.num)



# ----------
# "fs" basis provides random curves: one for each level of a factor
# rank 5 cubic spline (default) for each level of id.num
# splines all share the same 3 smoothing parameters: one for the usual spline penalty, and one for each term in the penalty null space.
b <- gamm(log.size ~ s(days) + ozone + ozone:days + s(days, id.num, bs = "fs", k = 5), data = sitka)




# ----------
plot(b$gam, pages = 1)


# -->
# Left:  smooth mean Sitka growth curve with 95% credible interval
# Right:  estimated random departures from mean growth trajectory for each tree

# There is a significant negative slope for enhanced ozone





