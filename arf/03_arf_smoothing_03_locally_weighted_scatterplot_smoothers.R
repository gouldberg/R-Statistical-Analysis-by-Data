setwd("//media//kswada//MyFiles//R//arf")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  arf
# ------------------------------------------------------------------------------

data(arf, package = "astsa")

str(arf)

head(arf)



# ------------------------------------------------------------------------------
# Smoothing in time series context
# Locally weighted scatterplot smoothers (lowess)
# ------------------------------------------------------------------------------
# Lowess:  nearest neighbor and robust weighted regression, wherein one uses only the data {x(t-k/2),...,x(t),...,x(t+k/2)}

par(mfrow=c(1,1))

plot(arf)


# Smoother uses 5% of the data
lines(lowess(arf, f = 0.05), lwd = 2, col = 4)


# trend (with default smoother span f = 2 / 3)
lines(lowess(arf), lty = 2, lwd = 2, col = 2)



# -->
# There is somewhat upward trend
