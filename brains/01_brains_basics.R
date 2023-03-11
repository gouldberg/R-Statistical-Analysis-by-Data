setwd("//media//kswada//MyFiles//R//brains")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  brains
#   - The average brain size and body weight were recorded for 28 species of land animals.
#   - Variables
#       - brain:  brain weight in g.
#       - body:  body weight in kg.
# ------------------------------------------------------------------------------

data(brains, package = "gamlss.mx")

str(brains)


car::some(brains)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

