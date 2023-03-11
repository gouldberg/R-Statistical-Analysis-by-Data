setwd("//media//kswada//MyFiles//R//abdom")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  abdom
# ------------------------------------------------------------------------------
data("abdom", package = "gamlss.data")


str(abdom)

car::some(abdom)



# ------------------------------------------------------------------------------
# The confint() function
#   - The generic function confint() provides Wald standard error-based confidence intervals for the coefficients (i.e. parameters) in the predictor
#     of a distribution parameter mu, sigma, nu, tau.
#     The Wald standard error-based confidence intervals are generally much less reliable than the profile likelihood confidence intervals,
#     if the model is correct, but may be prefered (with robust = TRUE) if the model is incorrect.
# ------------------------------------------------------------------------------

# t-distribution
h <- gamlss(y ~ pb(x), sigma.fo = ~pb(x), family = TF, data = abdom)

h2 <- gamlss(y ~ pb(x, df = h$mu.nl.df), sigma.fo = ~ pb(x, df = h$sigma.nl.df), family = TF, data = abdom)



# ----------
confint(h)
confint(h2)



# ------------------------------------------------------------------------------
# The prof.dev() function
#   - The function prof.dev() produces a profile deviance plot for any of the distribution parameters mu, sigma, nu, or tau, and is useful for
#     checking the reliability of models in which one (or more) of the parameters in the distribution are constant
#     (and therefore have not been modelled as functions of explanatory variablesS)
#   - The prof.dev() also provides a 100(1- alpha) % profile likelihood confidence interval for the parameter which is, in general, much more reliable
#     than a (Wald) standard error-based confidence interval if the model is correct.
# ------------------------------------------------------------------------------

# First attempt:
# By default the global deviance is evaluated at 7 values of nu equally spaced from min = 5 to max = 50
pd1 <- prof.dev(h2, "nu", min = 5, max = 50)

pd1


# -->
# The Maximum Likelihood Estimator is 13.88
# A 95% Confidence Interval is 7.343 - 40.725



# ----------
# Final attept:
pd2 <- prof.dev(h2, "nu", min = 5, max = 50, length = 20)

pd2


# -->
# more accurate
# The Maximum Likelihood Estimator is 11.61
# A 95% Confidence Interval is 6.179 - 42.042



# ----------
# at nu = 34, the Global Deviance is
pd2$fun(34)



# ----------
# Profile deviance for nu from a t-family fitted model h2
curve(pd2$fun(x), 5, 50)










