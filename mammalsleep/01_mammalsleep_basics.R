setwd("//media//kswada//MyFiles//R//mammalsleep")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  mammalsleep
#   - In Allison and Cicchetti (1976), data on the sleep behavior of 62 mammals is presented.
#     Suppose we are interested in modeling the proportion of sleep spent dreaming as a function of the other predictors:
#       - the weight of the body and the brain
#       - the gestation period
#       - the lifespan
#       - the three constrcted indices measuring vulnerability to predation, exposure while sleeping and overall danger
# ------------------------------------------------------------------------------

data("mammalsleep", package = "faraway")


str(mammalsleep)


dim(mammalsleep)


car::some(mammalsleep)



# ----------
# Calculate dream / sleep
mammalsleep$pdr <- with(mammalsleep, dream / sleep)


head(mammalsleep)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

