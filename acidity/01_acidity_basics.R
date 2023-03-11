setwd("//media//kswada//MyFiles//R//acidity")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  acidity
#   - This data set was analyzed as a mixture of Gaussian distributions on the log scale by Crawford.
#   - Variables
#       - y:  acidity index fo 155 lakes in the northeastern United States
# ------------------------------------------------------------------------------
data(acidity, package = "gamlss.data")

str(acidity)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

psych::describe(acidity)


hist(acidity)