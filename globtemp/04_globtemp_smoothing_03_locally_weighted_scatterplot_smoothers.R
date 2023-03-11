setwd("//media//kswada//MyFiles//R//globtemp")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  globtemp
# ------------------------------------------------------------------------------

data(globtemp, package = "astsa")

str(globtemp)



# ------------------------------------------------------------------------------
# Smoothing in time series context
# Locally weighted scatterplot smoothers (lowess)
# ------------------------------------------------------------------------------
# Lowess:  nearest neighbor and robust weighted regression, wherein one uses only the data {x(t-k/2),...,x(t),...,x(t+k/2)}

par(mfrow=c(1,1))

plot(globtemp)


# Smoother uses 5% of the data to smoothing
lines(lowess(globtemp, f = 0.05), lwd = 2, col = 4)


# trend (with default smoother span f = 2 / 3)
lines(lowess(globtemp), lty = 2, lwd = 2, col = 2)



# -->
# lowess default smoother shows increase of velocity of global temperature
