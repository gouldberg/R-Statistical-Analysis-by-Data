setwd("//media//kswada//MyFiles//R//glasses")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  glasses
#   - The age at which participants in the Blue Mountains Eye Study reported that they started wearing reading glasses.
#   - Variables:
#        - age:  age of participants
#        - sex:  1: male  2: female
#        - ageread:  age at which participant started wearing reading glasses
# ------------------------------------------------------------------------------
data("glasses", package = "gamlss.data")


str(glasses)

car::some(glasses)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
