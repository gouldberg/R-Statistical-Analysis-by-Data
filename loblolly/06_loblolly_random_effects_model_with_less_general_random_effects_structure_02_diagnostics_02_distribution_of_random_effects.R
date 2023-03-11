setwd("//media//kswada//MyFiles//R//loblolloy")

packages <- c("dplyr", "lattice", "gamair")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This tutorial is based on
# Chapter 2. GAMs in Practice: mgcv from "Generalized Additive Models An Introduction with R 2nd Edition" (by Simon N. Wood)


# ------------------------------------------------------------------------------
# data:  Loblloly
# ------------------------------------------------------------------------------

data(Loblolly, package = "gamair")


# Note that this is "nfnGroupedData" class
str(Loblolly)


head(Loblolly)



# ----------
# Seed was "ordered factor" --> changed to non-ordered factor
Loblolly$Seed <- as.factor(as.character(Loblolly$Seed))


# Without centering, polynomial terms can become highly correlated which can cause numerical difficulties
Loblolly$age <- Loblolly$age - mean(Loblolly$age)




# ------------------------------------------------------------------------------
# Normal QQ plots for the predicted random effects
# ------------------------------------------------------------------------------

qqnorm(m2, ~ ranef(.))

qqnorm(m5, ~ ranef(.))


