setwd("//media//kswada//MyFiles//R//flu")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  flu
# ------------------------------------------------------------------------------

data(flu, package = "astsa")

str(flu)

flu



# ------------------------------------------------------------------------------
# data exploration:  time series plot
# ------------------------------------------------------------------------------

# plot data with month initials as points

Months <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")


par(mfrow = c(1,1))

plot(flu, type = "c")
points(flu, pch = Months, cex = .8, font = 2)



# -->
# Typicaly, the number of deaths tends to increase faster than it decreases, especially during epidemics.
# Thus, if the data were plotted backward in time, that series would tend to increase slower than it decreases.
# Also, if monthly pneumonia and influenza deaths followed a linear Gaussian process, we would not expect to see such large bursts of positive
# and negative changes that occur periodically in this series.
# Moreover, although the number of deaths is typically largest during the winter months,
# the data are not perfectly seasonal.
# That is, although the peak of the series often occurs in January, in other years, the peak occurs in February or in March.
# Hence, seasonal ARMA models would not capture this behavior.
