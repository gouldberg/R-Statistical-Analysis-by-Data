setwd("//media//kswada//MyFiles//R//rent")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  rent
# ------------------------------------------------------------------------------
data("rent", package = "gamlss.data")


str(rent)

car::some(rent)



# ------------------------------------------------------------------------------
# Fit mars / earth smoothers interfacing with earth():  the ma() function
#   - The earth package in R builds regression models using the techniques in the papaer "Multivariate Adaptive Regression Splines", known as MARS.
#   - Earth is an acronym for "Enhanced Adaptive Regression Through Hinges"
#   - To allow for interactions, the degree parameter controls the maximum degree of interactions to be considered.
#     The default is degree = 1 which builds an additive model (i.e., no interactions)
#
#   - One of the advantages of the ma() function is the "built-in" model selection technique.
#     Final model will only include the selected predictors.
# ------------------------------------------------------------------------------


# -- gamlss.add2 is not available ...
library(gamlss.add2)


ma1 <- gamlss(R ~ ma(~ Fl + A + loc + H, control = ma.control(degree = 2, pmethod = "exhaustive")), data = rent, family = GA, 
              method = CG(20))



# ----------
# get the earch model for the mu parameter
ma.earth = getSmo(ma1, parameter = "mu")


summary(ma.earth)



# ----------
# plot the earth model for the mu parameter
plotmo(ma.earth)


