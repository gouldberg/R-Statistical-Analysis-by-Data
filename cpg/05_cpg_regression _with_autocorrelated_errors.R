setwd("//media//kswada//MyFiles//R//cpg")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  cpg
# ------------------------------------------------------------------------------

data(cpg, package = "astsa")

str(cpg)

cpg



# ------------------------------------------------------------------------------
# Regression with autocorrelated errors AR(1)
# ------------------------------------------------------------------------------

astsa::acf2(log(cpg), 20)

astsa::acf2(resid(fit), 20)



# ----------
graphics.off()
sarima(log(cpg), p = 1, d = 0, q = 0, xreg = cbind(time(cpg)))



# ----------
# Try ARMA(1,0,2), but the result is that MA term is not required ..
sarima(log(cpg), p = 1, d = 0, q = 2, xreg = cbind(time(cpg)))

