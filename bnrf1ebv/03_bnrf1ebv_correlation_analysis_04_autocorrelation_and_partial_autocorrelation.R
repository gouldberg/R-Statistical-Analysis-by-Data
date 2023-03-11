setwd("//media//kswada//MyFiles//R//bnrf1ebv")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  bnrf1ebv
# ------------------------------------------------------------------------------

data(bnrf1ebv, package = "astsa")

bnrf1ebv



# ------------------------------------------------------------------------------
# Correlation analysis:  autocorrelation and partial autocorrelation of Recruitment
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow = c(1, 1))


# acf2:  produces a simultaneous plot (and a printout) of the sample ACF and PACF
# The zero lag value of the ACF is removed.
astsa::acf2(bnrf1ebv, 48, main = "BNRF1 gene from the Epstein-Barr virus")



# -->
# We could see only slight pattern at lag 1 and ACF and PACF..



