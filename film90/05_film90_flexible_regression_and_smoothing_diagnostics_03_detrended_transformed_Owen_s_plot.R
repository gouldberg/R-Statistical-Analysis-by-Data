setwd("//media//kswada//MyFiles//R//film90")

packages <- c("dplyr", "gamlss", "gamlss.add")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  film90
# ------------------------------------------------------------------------------
data("film90", package = "gamlss.data")


str(film90)

car::some(film90)



# ------------------------------------------------------------------------------
# Diagnostics: detrended transformed Owen's plot (DTOP)
#   - This plot is applied to the fitted normalized (randomized) quantile residuals of the fitted model
#   - Jager and Wellner corrected the approximation fomula given by Owen for obtaining the 95% and 99% empirical confidence intervals.
#     dtop() has both formulae as options
# ------------------------------------------------------------------------------

dtop(m3, xvar = film90$lboopen, n.inter = 7)

dtop(m6, xvar = film90$lboopen, n.inter = 7)


