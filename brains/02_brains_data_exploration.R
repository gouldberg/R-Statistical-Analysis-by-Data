setwd("//media//kswada//MyFiles//R//brains")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  brains
# ------------------------------------------------------------------------------

data(brains, package = "gamlss.mx")

str(brains)


car::some(brains)



# ------------------------------------------------------------------------------
# data exploration
# ------------------------------------------------------------------------------

summary(brains)


psych::describe(brains)



# -->
# distribution is really skewed.

