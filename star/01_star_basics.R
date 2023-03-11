setwd("//media//kswada//MyFiles//R//star")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  star
#   - Magnitude of a star taken at midnight for 600 consecutive days.
#     The data are taken from the classic text, The Calculus of Observations, a Treatise on Numerical Mathematics, by E.T. Whittaker and G. Robinson (1923).
# ------------------------------------------------------------------------------

data(star, package = "astsa")


str(star)

head(star)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

