
packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  UKgas
# ------------------------------------------------------------------------------


data(UKgas)


str(UKgas)


UKgas



# ----------
# take log and diff
y <- diff(log(UKgas))


plot(y, type = "l")




# ------------------------------------------------------------------------------
# Estimate Time-Varying Variance and normalize time series
# ------------------------------------------------------------------------------

library(TSSS)


# estimate time-varying variance
# we apply trend.order = 1  (when applying 2, too smooth ...)

output <- tvvar(y, trend.order = 1)



# -->
# this function reduces to data points in half




# ----------
graphics.off()


par(mfrow = c(2,2))

# transformed data
# 21:  1970 2nd quarter
plot(output$sm, type = "l", main = "transformed data")
abline(v = c(21), lty = 1, col = "darkgray")

# trend of transformed data
ts.plot(output$trend[,2], type = "l", main = "trend of transformed data")
abline(v = c(21), lty = 1, col = "darkgray")


# time-varying variance = exp(trend)
plot(output$tvv, type = "l", main = "time-varying variance")
abline(v = c(21), lty = 1, col = "darkgray")

# plot(exp(output$trend[,2]), type = "l", main = "time-varying variance")


# normalized data
plot(output$nordata, type = "h", main = "normalized data")
abline(v = c(315, 513), lty = 1, col = "darkgray")




# ----------
# normalized data
dat_norm <- output$nordata




# -----------
# note that variance of normalized series (sigma2) is NOT !!! close to 1
output$sigma2

n <- length(output$sm)

var(output$nordata) * (n - 1) / n



# ----------
# tau2 is 0.015

output$tau2
