setwd("//media//kswada//MyFiles//R//ricefarms")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  RiceFarms
# ------------------------------------------------------------------------------

data("RiceFarms", package = "splm")


str(RiceFarms)


dim(RiceFarms)



# ------------------------------------------------------------------------------
# Breusch-Pagan tests:  individual and/or time effects
#   - The Breusch and Pagan (1980) test is a Lagrange multipliers test based on the OLS residuals.
#     It is based on the score vector, i.e., the vector of partial derivatives of the log-likelihood function from the restricted model.
# ------------------------------------------------------------------------------

# the argument is OLS model or its formula - data pair
# by default in plmtest, Honda (1985) version is computed.

plmtest(rice.p)


# -->
# Unsurprisingly, the absence of individual effects is strongly rejected.



# ----------
# The test of the null hypothesis of no individual and no time effects

plmtest(rice.p, effect = "twoways")

# plmtest(rice.p, effect = "kw")
# plmtest(rice.p, effect = "ghm")



# ----------
# The test of the null hypothesis of no time effects allowing for the presence of individual effects

plmtest(rice.p, effect = "time")




