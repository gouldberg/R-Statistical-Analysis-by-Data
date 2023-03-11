
packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  UKgas
# ------------------------------------------------------------------------------


data(UKgas)


str(UKgas)


UKgas



# ----------
# remove time series property by c()

dat2 <- c(diff(log(UKgas)))





# ------------------------------------------------------------------------------
# Assess Trend in data
# ------------------------------------------------------------------------------

graphics.off()


plot(dat2, type = "l")

lines(smooth.spline(time(dat2), dat2, spar = 0.75), col = "blue", lwd = 2)

lines(smooth.spline(time(dat2), dat2, spar = 1), col = "red", lwd = 2)




# ------------------------------------------------------------------------------
# Decompose time series to stationary subintervals and estimate local spectrum
# ------------------------------------------------------------------------------

library(TSSS)
library(timsac)


# ns0:  basic local span
# Note that when setting large max.order, first data points corresponding to those lags are ignored.
# (for example: change max.order = 10 to 20)

output <- TSSS::lsar(dat2, max.arorder = 5, ns0 = 6)

output2 <- timsac::mlocar(dat2, max.order = 5, span = 6, plot = FALSE)



# ----------
# start and end point  -->  should see timsac::mlocar output !!
output2$init

output2$end

unique(output2$init)



# ----------
graphics.off()

par(mfrow = c(1,1))


# use timsac::mlocar output !!
plot(dat2, type = "l")
abline(v = unique(output2$init), col = "blue", lwd = 1)



