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
# Correlation analysis:  lagged scatterplot matrices
#   - to check for nonlinear relations
# ------------------------------------------------------------------------------

graphics.off()

astsa::lag1.plot(log(cpg), max.lag = 20)

Acf(log(cpg), lag.max = 20, plot=FALSE)


