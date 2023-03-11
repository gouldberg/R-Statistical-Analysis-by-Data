setwd("//media//kswada//MyFiles//R//respInf")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  respInf
# ------------------------------------------------------------------------------
data("respInf", package = "gamlss.data")


str(respInf)

car::some(respInf)



# ------------------------------------------------------------------------------
# Binary response with normal random intercept
# ------------------------------------------------------------------------------

library(gamlss.mx)


# mixture = "gq":  for normal random intercept models using Gaussian quadrature
# K:  number of Gaussian quadrature points, at least K = 10 for reasobale accuracy
# Gaussian quadrature is a numerical integration method in which the integral is approximated by a summation.
# Gaussian quadrature replaces the continuous distribution with an approximating discrete distribution

m1 <- gamlssNP(time ~ age + female + cosine + height, random = ~ 1 | id, K = 20, mixture = "gq", data = respInf, family = BI)


m1



# ----------
# the fitted model is
# logit(mu(ij)) = -2.564 - 0.033 * age(j) - 0.464 * (female = 1)(j) - 0.551 * cosine(j) - 0.054 * height(j) + N(0, 0.0836^2)
# Note that the estimate of sigma_b is given by the coefficient "z".

# i:  time variable
# j:  subject

# fitted (marginal) global deviance is 671.843.
# (marginal) AIC is 683.843 where an additional penalty 2 was added for parameter sigma_b




# ----------
plot(m1)


