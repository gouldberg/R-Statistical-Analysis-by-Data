setwd("//media//kswada//MyFiles//R//milk")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  milk
#   - Information about the composition of milk across primate species, as well as some facts about those species, like body mass and brain size.
#   - A popular hypothesis has it that primates with larger brains produce more energetic milk, so that brains can grow quickly.
#   - The variables we'll consider for now are:
#        - kcal.per.g: Kilocalories of energy per gram of milk
#        - mass: Average female body mass, in kilograms
#        - neocortex.perc: The percent of total brain mass that is neocortex mass
#   - The question here is to what extent energy congten of milk, measured here by kilocalories, is related to the percent of the brain mass that is neocortex
#     Neocortex is the gray, outer part of the brain that is particularly elaborated in mammals and especially primates.
# ------------------------------------------------------------------------------
data("milk", package = "rethinking")

d <- milk

dim(d)

str(d)


