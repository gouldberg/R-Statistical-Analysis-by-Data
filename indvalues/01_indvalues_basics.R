setwd("//media//kswada//MyFiles//R//indvalues")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  indvalues
#   - dataset analyzed in Borg et al. (2017). 
#     Their research question was whether the value circle exists within persons and not only across persons.
#     The instrument theory used to measure the values was the Schwartz Value Survery (SVS; Schwartz et al., 2000).
#     In total, the dataset has 327 persons and 10 variables representing value scores (dissimilarities): power, achievement, hedonism, stimulation,
#     self-direction, universalism, benevolence, tradition, conformity, and security.
# ------------------------------------------------------------------------------

data("indvalues", package = "smacof")

str(indvalues)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

psych::describe(indvalues)

