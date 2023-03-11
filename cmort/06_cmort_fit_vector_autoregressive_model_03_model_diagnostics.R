setwd("//media//kswada//MyFiles//R//cmort")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# data:  cmort (Pollution, Temperature and Mortality)
# ------------------------------------------------------------------------------

data(cmort, package = "astsa")
data(tempr, package = "astsa")
data(part, package = "astsa")


str(cmort)
str(tempr)
str(part)


cmort
tempr
part



# ------------------------------------------------------------------------------
# Examine the residuals:  cross-correlations of the residulas
# ------------------------------------------------------------------------------

summary(fit <- VAR(x, p = 2, type = "both"))



# ----------
# Acf2 can not be used for multivariate time series ...

acf(resid(fit), lag.max = 52)


acf(resid(fit), 52)$acf



# -->
# ACFs of the individual residual series along the diagonal.
# The off diagonals display the CCFs between pairs of residual series.
# Note that second named series is the one that leads.

# Notice that most of the correlations in the residual series are negliible,
# however, the zero-order correlation of mortality with temperature residuals is about 0.22
# and mortality with particualte residuals is about 0.28

# This means that the AR model is not capturing the concurrent effect of temperature and pollution on mortality
# (recall the data evolves over a week).




# ------------------------------------------------------------------------------
# Examine the multivariate version of the Q-test
# ------------------------------------------------------------------------------

vars::serial.test(fit, lags.pt = 12, type = "PT.adjusted")



# -->
# Thus, not unexpectedly, the Q-test rejects the null hypothesis that the noise is white.

