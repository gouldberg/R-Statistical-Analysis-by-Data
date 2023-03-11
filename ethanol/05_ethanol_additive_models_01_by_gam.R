setwd("//media//kswada//MyFiles//R//ethanol")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ethanol
# ------------------------------------------------------------------------------

data(ethanol, package="lattice")

str(ethanol)



# ------------------------------------------------------------------------------
# Additive model by gam package
#   - gam package originates from the work of Hastie and Tibshirani (1900)
#     The gam package allows more choice in the smoothers used
#     while the mgcv package has an automatic choice in the amount of smoothing as well as wider functionality
#   - gam packages use backfitting algorithm for fitting.
#     The algorithm is iterated until convergence. Hastie and Tibshirani show that convergence is assured under some rather loose conditions.
# ------------------------------------------------------------------------------

library(gam)


# We fit an additive model using a Gaussian response as the default
# lo:  loess smoother
# s:  spline smoother
amgam <- gam(NOx ~ lo(C) + lo(E), data = ethanol, trace=TRUE)

summary(amgam)



# ----------
# R2 = 1 - Residual Deviance / Null Deviance  --> very small ...
1 - amgam$deviance / amgam$null.deviance

