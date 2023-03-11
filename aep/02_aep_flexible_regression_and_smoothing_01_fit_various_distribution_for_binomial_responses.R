setwd("//media//kswada//MyFiles//R//aep")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  aep
# ------------------------------------------------------------------------------

data("aep", package = "gamlss.data")


str(aep)

car::some(aep)



# ------------------------------------------------------------------------------
# Fit various distributions for binomial response
# ------------------------------------------------------------------------------

# m1 is the final model of Gange et al.
m1 <- gamlss(y ~ ward + year + loglos, sigma.fo = ~year, family = BB, data = aep, trace = FALSE)

m2 <- gamlss(y ~ ward + year + loglos, sigma.fo = ~year + ward, family = BB, data = aep, trace = FALSE)

m3 <- gamlss(y ~ ward + year + cs(loglos, df = 1), sigma.fo = ~year + ward, family = BB, data = aep, trace = FALSE)

m4 <- gamlss(y ~ ward + year + cs(loglos, df = 1) + cs(age, df = 1), sigma.fo = ~year + ward, family = BB, data = aep, trace = FALSE)



# ----------
# the global deviance (k = 0)
GAIC(m1, m2, m3, m4, k = 0)


# SBC
GAIC(m1, m2, m3, m4, k = log(length(aep)))



# -->
# There is a strong support for the inclusin of a smoothing term for loglos



# ------------------------------------------------------------------------------
# Change link for model 4
# ------------------------------------------------------------------------------

# changing the logistic link for the mean to a probit link
m41 <- gamlss(y ~ ward + year + cs(loglos, 1) + cs(age, 1), sigma.fo = ~year + ward, family = BB(mu.link = "probit"), data = aep, trace = FALSE)


GAIC(m1, m2, m3, m4, m41, k = log(length(aep)))



# -->
# Note also the model 4 can also be improved marginally by changing the logistic link for the mean to a probit link



# ------------------------------------------------------------------------------
# fitted function
# ------------------------------------------------------------------------------

term.plot(m41, pages = 1)

term.plot(m41, "sigma", pages = 1)



# ------------------------------------------------------------------------------
# Normalized randomized quantile residuals
# ------------------------------------------------------------------------------

# six instances of the normalized randomized quantile residulas
# residuals seem to be reasonably (though not entirely) satisfactory
rqres.plot(m41)

rqres.plot(m1)
