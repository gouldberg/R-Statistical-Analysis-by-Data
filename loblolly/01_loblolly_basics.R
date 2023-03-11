setwd("//media//kswada//MyFiles//R//loblolloy")

packages <- c("dplyr", "lattice", "gamair")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This tutorial is based on
# Chapter 2. GAMs in Practive: mgcv from "Generalized Additive Models An Introduction with R 2nd Edition" (by Simon N. Wood)


# ------------------------------------------------------------------------------
# data:  Loblloly
#   - Contains growth data on Loblolly pine trees, height, in feet (data from the US), and age, in years, are recorded for 14 individual trees.
#     A factor variable Seed, with 14 levels, indicates the identity of individual trees.
# ------------------------------------------------------------------------------

data(Loblolly, package = "gamair")


# Note that this is "nfnGroupedData" class
str(Loblolly)


head(Loblolly)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------


