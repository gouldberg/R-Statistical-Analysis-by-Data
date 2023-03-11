setwd("//media//kswada//MyFiles//R//houseprices_us")

packages <- c("dplyr", "plm")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  House Prices US
# ------------------------------------------------------------------------------

data("HousePricesUS", package = "pder")


str(HousePricesUS)


dim(HousePricesUS)



# ------------------------------------------------------------------------------
# Breusch-Pagan tests:  individual and/or time effects
#   - The Breusch and Pagan (1980) test is a Lagrange multipliers test based on the OLS residuals.
#     It is based on the score vector, i.e., the vector of partial derivatives of the log-likelihood function from the restricted model.
# ------------------------------------------------------------------------------

# the argument is OLS model or its formula - data pair
# by default in plmtest, Honda (1985) version is computed.

plmtest(php.p)


# -->
# Unsurprisingly, the absence of individual effects is strongly rejected.



# ----------
# The test of the null hypothesis of no individual and no time effects

plmtest(php.p, effect = "twoways")

# plmtest(php.p, effect = "kw")
# plmtest(php.p, effect = "ghm")



# ----------
# The test of the null hypothesis of no time effects allowing for the presence of individual effects

plmtest(php.p, effect = "time")




