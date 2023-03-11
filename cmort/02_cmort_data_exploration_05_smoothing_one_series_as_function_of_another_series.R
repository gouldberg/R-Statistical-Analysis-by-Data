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
# data exploration:  Smoothing one series as a function of another series
# ------------------------------------------------------------------------------

# We smooth the scatterplot of two contemporaneously measured time series, mortality as a function of temperature

par(mfrow = c(1,1))

plot(tempr, cmort, xlab = "Temperature", ylab = "Mortality", pch = 20, col = gray(0.7), main = "Y:Mortality  X:Temperature")

lines(lowess(tempr, cmort), col = "blue")



# -->
# Note that mortality increases at extreme temperatures, but in an asymmetic way.
# mortality is higher at colder temperatures than at hotter temperatures.
# The minimm mortality rate seems to occur at approximately 83 F.

