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
# Forecasting by sarima.for
# ------------------------------------------------------------------------------

# next 3 years forecast

par(mfrow=c(1,1))

sarima.for(log(cpg), n.ahead = 3, p = 0, d = 1, q = 0, newxreg = cbind(time(cpg)))

