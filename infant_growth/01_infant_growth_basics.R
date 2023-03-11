setwd("//media//kswada//MyFiles//R//infant_growth")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  infant growth
#   - Data of length of the tibia of a newborn infant, measured by Dr. Michael Harmanussen with an error of the order of 0.1 millimeters,
#     over its first 40 days.
# ------------------------------------------------------------------------------

data("infantGrowth", package = "fda")


dim(infantGrowth)


head(infantGrowth)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

