setwd("//media//kswada//MyFiles//R//fabric")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  fabric
# ------------------------------------------------------------------------------
data("fabric", package = "gamlss.data")


str(fabric)

car::some(fabric)



# ------------------------------------------------------------------------------
# Diagnostics: detrended transformed Owen's plot (DTOP)
#   - This plot is applied to the fitted normalized (randomized) quantile residuals of the fitted model
#   - Jager and Wellner corrected the approximation fomula given by Owen for obtaining the 95% and 99% empirical confidence intervals.
#     dtop() has both formulae as options
# ------------------------------------------------------------------------------

dtop(mPO2, xvar = fabric$x, n.inter = 4)


