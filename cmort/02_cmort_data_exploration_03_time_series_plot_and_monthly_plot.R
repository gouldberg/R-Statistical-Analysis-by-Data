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
# data exploration:  time series plot by monthplot
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow = c(3,1))

monthplot(cmort)

monthplot(tempr)

monthplot(part)



# -->
# October to Decenber, 
# the larger particulate is, the larger the mortality


