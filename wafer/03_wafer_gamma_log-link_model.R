setwd("//media//kswada//MyFiles//R//wafer")

packages <- c("dplyr", "faraway")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  wafer
# ------------------------------------------------------------------------------

data(wafer, package="faraway")

str(wafer)

car::some(wafer)



# ------------------------------------------------------------------------------
# Fit gamma GLM and select the model using the AIC criterion
#   - Gamma GLM is useful where we may be willing to speculate on the relationship between the mean and variance of the response
#     but are not sure about the distribution.
#   - For some data, we might expect the standard deviaion to increase linearly with the response:
#     SD( Y / E(Y) ) is constant but var(Y) is proportional to E(Y)^2
#     For example, measurements of larger objects do tend to have more error than smaller ones.
#     If we wanted to apply a Gaussian linear model, the log transform is indicated. This would imply a lognormal distribution for the original response.
#     Alternatively, if Y ~ gamma, then var Y is proportional to E(Y)^2, so a gamma GLM is also appropriate in this situation
# ------------------------------------------------------------------------------

gmdl <- glm(resist ~ .^2, family = Gamma(link = log), wafer)

summary(gmdl)



# ----------
rgmdl <- step(gmdl, direction = "both")

summary(rgmdl)

summary(rlmdl)


# -->
# In this case, we see that the coefficients are remarkably similar to the linear model with the logged response.
# Even the standard errors are almost identical and the square root of the dispersion corresponds to the residual standard error of the linear model

summary(rgmdl)$dispersion
sqrt(summary(rgmdl)$dispersion)



# ----------
# The maximum likelihood estimate of dispersion parameter
library(MASS)
gamma.dispersion(rgmdl)


# -->
# this gives a substantially smaller estimate, which would suggest smaller standard errors.



# ----------
# shape parameter of gamma = 1 / dispersion parameter = 221
1 / 0.004524942


# -->
# this is large, the gamma distribution is well approximated by a normal, and gamma GLM is not much differnt from lognormal model


