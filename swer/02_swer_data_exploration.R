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
# data exploration
# ------------------------------------------------------------------------------

summary(swer)


psych::describe(swer)


Hmisc::describe(swer)



# -->
# Note that "nao" only have 35 distinct values, corresponding to year



# ----------
xtabs(~ year + code, data = swer)

xtabs(~ code, data = swer)

xtabs(~ year, data = swer)


# -->
# Maby stations are recorded every year from 1981 to 2015 (35 years) but some stations have missing in some years

