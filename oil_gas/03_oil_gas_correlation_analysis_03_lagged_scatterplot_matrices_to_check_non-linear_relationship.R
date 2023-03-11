setwd("//media//kswada//MyFiles//R//oil_gas")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Oil and Gas
# ------------------------------------------------------------------------------

data(oil, package = "astsa")

data(gas, package = "astsa")

str(oil)

str(gas)

oil

gas




# ------------------------------------------------------------------------------
# Correlation analysis:  lagged scatterplot matrices
#   - to check for nonlinear relations
# ------------------------------------------------------------------------------

graphics.off()

astsa::lag1.plot(diff(log(oil)), max.lag = 12)
Acf(diff(log(oil)), lag.max = 12, plot=FALSE)



# -->
# The sample autocorrelations are displayed in the upper right-hand corner and
# superimposed on the scatterplots are locally weighted scatterplot
# smoothing (lowess) lines can be used to help discover any nonlinearities.



# ----------
astsa::lag1.plot(diff(log(gas)), max.lag = 24)
Acf(diff(log(gas)), lag.max = 24, plot=FALSE)



# ----------
# Exhibit scatterplots of the oil and gas growth rate series for up to 3 weeks of lead time of oil prices;
# including a nonparametric smoother in each plot
astsa::lag2.plot(diff(log(gas)), diff(log(oil)), max.lag = 3)

Ccf(diff(log(gas)), diff(log(oil)), lag.max = 3, plot=FALSE)



# -->
# In lag 0 to 2 plot, there is some outliers and the relationship is somewhat nonlinear,
# while in lag 3, the relationship is linear including outliers ...
