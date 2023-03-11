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
# Select VAR model by VARselect
# ------------------------------------------------------------------------------

# vars rit vector AR models via least squares.
library(vars)


x <- cbind(cmort, tempr, part)



# ----------
# "both" fits constant + trend

VARselect(x, lag.max = 10, type = "both")



# -->
# Note that BIC picks the order p = 2 model while AIC and FPE pick an order p = 9 model
# and Hannan-Quinn selects an order p = 5 model.



# ------------------------------------------------------------------------------
# Fit VAR model by p = 2
# ------------------------------------------------------------------------------

summary(fit <- VAR(x, p = 2, type = "both"))

