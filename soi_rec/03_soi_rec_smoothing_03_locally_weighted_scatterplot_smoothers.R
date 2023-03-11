setwd("//media//kswada//MyFiles//R//soi_rec")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  soi
# data:  rec
# ------------------------------------------------------------------------------

data(soi, package = "astsa")

data(rec, package = "astsa")




# ------------------------------------------------------------------------------
# Smoothing in time series context
# Locally weighted scatterplot smoothers (lowess)
# ------------------------------------------------------------------------------
# Lowess:  nearest neighbor and robust weighted regression, wherein one uses only the data {x(t-k/2),...,x(t),...,x(t+k/2)}

par(mfrow=c(1,1))

plot(soi)


# Smoother uses 5% of the data to obtain an estimate of the El Nino cycle of the data
lines(lowess(soi, f = 0.05), lwd = 2, col = 4)


# trend (with default smoother span f = 2 / 3)
lines(lowess(soi), lty = 2, lwd = 2, col = 2)



# -->
# Also a negative trend in SOI would indicate the long-term warming of the Pacific Ocean
