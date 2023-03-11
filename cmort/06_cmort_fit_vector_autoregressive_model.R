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
# Fit Vector AR model (VAR)
#   - We might envision dynamc relations among the three series defined as the first order relation,
#     which expresses the current value of mortality as a linear combination of trend and its immediate past value and the past values of
#     temperature and particulate levels.
#     Similarly, dependence of temperature and particulate levels on the other series.
# ------------------------------------------------------------------------------


# vars Fit vector AR models via least squares.
library(vars)



# only include tempr and part
x <- cbind(cmort, tempr, part)




# ----------
# "both" fits constant + trend
# only p = 1

summary(VAR(x, p = 1, type = "both"))




# -->
# Note that each for cmort, tempr, part,
# the equation is estimated

# as expected, "part" equation is not significant





