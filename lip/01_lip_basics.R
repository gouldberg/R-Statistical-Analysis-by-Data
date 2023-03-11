setwd("//media//kswada//MyFiles//R//lip")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  lip
#   - This data come from an experimental enzymology research project which attempted to develop a generic food spoilage model.
#   - Variable:
#        - y:  a matrix with 3 columns, that is a Surv() object indicating the start, the finish and censored indicator as defined in function Surv() of the package survival
#        - name:  a factor with levels for the different experiments
#        - Tem:  the temperature
#        - pH:  the PH
#        - aw:  the water acidity
#        - X0.d:X39.d:  vectors indicating whether enzyme reacted at specific days
# ------------------------------------------------------------------------------
data("lip", package = "gamlss.cens")


str(lip)

car::some(lip)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

head(lip$y, 10)


# -->
# The value 1- indicates an interval (1, inf) not including 1, while [11, 18] indicates the interval (11, 18] not incuding 11 but including 18.
# This specific data set does not have left-censored observations.
# If it did then, for example, a value of -5 would indicate the interval (-inf, 5] including 5.
# For a continuous distribution the likelihood is unaffected by whether interval endpoints are included or not, but for a discrete distribution 
# this is very important.


