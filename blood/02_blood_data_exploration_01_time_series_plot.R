setwd("//media//kswada//MyFiles//R//blood")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  blood
# ------------------------------------------------------------------------------

data(blood, package = "astsa")

str(blood)

blood



# ------------------------------------------------------------------------------
# data exploration:  time series plot
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))

plot(blood, type = "o", pch = 19, xlab = "day", main = "")


summary(blood)



# -->
# Approximately 40% of the values are missing, with missing values occurring primarily after the 35th day.