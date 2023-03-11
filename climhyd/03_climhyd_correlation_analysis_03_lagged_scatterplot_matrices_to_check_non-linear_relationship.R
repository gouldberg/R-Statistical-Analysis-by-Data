setwd("//media//kswada//MyFiles//R//climhyd")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  climhyd
# ------------------------------------------------------------------------------

data(climhyd, package = "astsa")


str(climhyd)

head(climhyd)


inf <- log(climhyd$Inflow)

prec <- sqrt(climhyd$Precip)



# ------------------------------------------------------------------------------
# Correlation analysis:  lagged scatterplot matrices
#   - to check for nonlinear relations
# ------------------------------------------------------------------------------

# Because we might wish to predict the logged Inflow series from current or past values of the squared root of precipitation series,
# it would be worthwhile to examine the scatterplot matrix.
astsa::lag2.plot(prec, inf, max.lag = 20)

  forecast::Ccf(prec, inf, lag.max = 20, plot=FALSE)



# -->
# Note that there are many zero values for precipitation ...