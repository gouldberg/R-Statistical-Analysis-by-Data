rm(list=ls())

packages <- c("dplyr", "dlm", "KFAS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Nile
# ------------------------------------------------------------------------------

data(Nile)


dim(Nile)


str(Nile)



# ------------------------------------------------------------------------------
# Correlation analysis:  autocorrelation and partial autocorrelation
#   - partial autocorrelation:
#     regressed the values of the time series at all shorter lags
#     It contrasts with the autocorrelation function, which does not control for other lags
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow = c(1, 1))


# acf2:  produces a simultaneous plot (and a printout) of the sample ACF and PACF
# The zero lag value of the ACF is removed.

astsa::acf2(Nile, 48, main = "Nile")

astsa::acf2(diff(Nile), 48, main = "Nile")




# ----------
library(astsa)

sarima(Nile, p = 2, d = 1, q = 1)



