setwd("//media//kswada//MyFiles//R//mcycle")

packages <- c("dplyr", "lattice", "MASS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This tutorial is based on Chapter 7. Introductin GAMs from "Generalized Additive Models An Introduction with R 2nd Edition" (by Simon N. Wood)


# ------------------------------------------------------------------------------
# data:  mcycle
#   - The data measure the acceleration of the rider's head, against time, in a simulated motorcycle crash
#   - This is classic dataset in univariate smoothing, introduced in Silverman
#   - times:  in milliseconds after impact   accel:  in g.
# ------------------------------------------------------------------------------

data(mcycle, package = "MASS")

str(mcycle)




# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

plot(accel ~ times, data = mcycle)


