setwd("//media//kswada//MyFiles//R//accident")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS", "gpairs")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Accident
#  - 4-way (5*2*4*2) table of frequencies of traffic accident victims in France in 1958, which is used by Bertin to
#    illustrate his scheme for classifying data sets by numerous variables, each of which could have various types and coud be assigned to various visual attributes
# ------------------------------------------------------------------------------

data("Accident", package = "vcdExtra")

dim(Accident)

str(Accident)


car::some(Accident)





