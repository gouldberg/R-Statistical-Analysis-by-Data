setwd("//media//kswada//MyFiles//R//aids")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  aids
# ------------------------------------------------------------------------------
data("aids", package = "gamlss.data")


str(aids)

car::some(aids)



# ------------------------------------------------------------------------------
# The confint() function
#   - The generic function confint() provides Wald standard error-based confidence intervals for the coefficients (i.e. parameters) in the predictor
#     of a distribution parameter mu, sigma, nu, tau.
#     The Wald standard error-based confidence intervals are generally much less reliable than the profile likelihood confidence intervals,
#     if the model is correct, but may be prefered (with robust = TRUE) if the model is incorrect.
# ------------------------------------------------------------------------------

m100 <- gamlss(y ~ x + qrt, data = aids, family = NBI)



# ----------
summary(m100)


# -->
# The coefficient for x has the value of 0.0874 and a t-value of 13.3773, which indicates that
# it is highly significantly different from zero.



# ----------
# An approximate (Wald) standard error-based 95% confidence interval for this parameter can be obtained using the function confint()
confint(m100)



# ------------------------------------------------------------------------------
# The prof.term() function
#   - This can provide a profile deviance for any parameter in the model, not just for a constant distribution parameter.
#   - While prof.dev() can be applied to profile a (constant) parameter of the distribution, prof.term() can be applied to any parameter in the predictor model
#     for mu, sigma, nu, or tau.
# ------------------------------------------------------------------------------

# prof.term() to find, a (profile likelihood) 95% confidence interval (which is probably more accurate) for the linear term parameter for x.
# The parameter of interest is denoted "this" in the model formula and is the slope parameter of x in the predictor for mu.
# For a fixed value of "this", the term "this * x" is fixed and known and so is offset in the predictor for mu:

mod <- quote(gamlss(y ~ offset(this * x) + qrt, data = aids, family = NBI))

prof.term(mod, min = 0.06, max = 0.11, length = 20)



# -->
# the profile deviance looks quadratic, so it is not a surprise that the approximate (Wald) standard error-based 95% confidence interval
# and the 95% profile interval are almost identical.
# In general this will not be the case if the likelihood is not nearly quadratic at the maximum.



# ----------
# to obtain a 99% interval:
prof.term(mod, min = 0.06, max = 0.11, length = 20, perc = 99)

