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
# Flexible regression and smoothing
# ------------------------------------------------------------------------------

abd10 <- gamlss(y ~ pb(x), sigma.fo = ~pb(x), data = abdom, family = BCT, trace = FALSE)


summary(abd10)



# ------------------------------------------------------------------------------
# Diagnostics: detrended transformed Owen's plot (DTOP)
#   - This plot is applied to the fitted normalized (randomized) quantile residuals of the fitted model
#   - Jager and Wellner corrected the approximation fomula given by Owen for obtaining the 95% and 99% empirical confidence intervals.
#     dtop() has both formulae as options
# ------------------------------------------------------------------------------

dtop(abd10, xvar = abdom$x, n.inter = 9)



# -->
# Since the horizontal line of each DTOP plot lies within the 95% confidence bands,
# we conclude that the normalized residuals could have come from a normal distribution.

