setwd("//media//kswada//MyFiles//R//minima_maxima")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  minima and maxima
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Residual diagnostics
# ------------------------------------------------------------------------------

wp(m1, ylim.all = 1)

wp(m2, ylim.all = 1)

wp(m3, ylim.all = 1)



# -->
# Only normal distribution (m3) fits well ....

