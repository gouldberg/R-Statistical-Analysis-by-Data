setwd("//media//kswada//MyFiles//R//chimpanzees")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  prosocial chimpanzees
#   - The data for this exmaples come from an experiment aimed at evaluating the prosocial tendencies of chimpanzees.
#     (detailed in "Statistical Rethinking p.292)
#   - The motivating question is whether a focal chimpanzee behaves similarly, choosing the prosocial option more often when another animal is present.
#   - In terms of linear models, we want to estimate the interaction between condition (presence or absence of another animal) and option (which side is prosocial)
# ------------------------------------------------------------------------------
data("chimpanzees", package = "rethinking")

d <- chimpanzees

dim(d)

str(d)


# -->
# This is the single-trial cases data (such that the outcome variable can only take values 0 and 1)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
# pulled_left: 0 or 1 indicator that the focal animal pulled the lef-hand lever
# prosoc_left: 0 or 1 indicator that the left-hand lever was (1) or was not (0) attached to the prosocial option, the side with two pieces of food
# condition: 0 or 1 indicator, with value 1 for the partner condition and value 0 for the control condition
addmargins(xtabs(~ pulled_left + prosoc_left + condition, data = d))
