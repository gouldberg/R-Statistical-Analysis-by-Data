setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\nikkei225")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  nikkei225
# ------------------------------------------------------------------------------


dat <- read.table("nikkei225.txt", sep = "", header = T, colClasses = "numeric")


head(dat)


dat <- dat$nikkei225


dat <- diff(log(dat))




# ------------------------------------------------------------------------------
# Decompose time series to stationary subintervals and estimate local spectrum
# ------------------------------------------------------------------------------

library(TSSS)
library(timsac)


# ns0:  basic local span
# Note that when setting large max.order, first data points corresponding to those lags are ignored.
# (for example: change max.order = 10 to 20)

output <- TSSS::lsar(dat, max.arorder = 10, ns0 = 100)

output2 <- timsac::mlocar(dat, max.order = 10, span = 100, plot = FALSE)




# ----------
# start and end point  -->  should see timsac::mlocar output !!
output2$init

output2$end

unique(output2$init)



# ----------
graphics.off()

par(mfrow = c(1,1))


# use timsac::mlocar output !!
plot(dat, type = "l")
abline(v = unique(output2$init), col = "blue", lwd = 1)




# ------------------------------------------------------------------------------
# Estimation of change points
# ------------------------------------------------------------------------------


output <- lsar.chgpt(dat, max.arorder = 10, subinterval = c(200, 1000),
                     candidate = c(400, 600))


output$change.point



# -->
# point 527 is detected



# ----------
output <- lsar.chgpt(dat, max.arorder = 10, subinterval = c(528, 1000),
                     candidate = c(550, 800))


output$change.point


# -->
# point 680 is detected




# ----------
output <- lsar.chgpt(dat, max.arorder = 10, subinterval = c(681, 1300),
                     candidate = c(750,1100))


output$change.point



# -->
# point 1052 is detected



# ----------
output <- lsar.chgpt(dat, max.arorder = 10, subinterval = c(500, 1450),
                     candidate = c(1100,1350))


output$change.point



# -->
# point 1204 is detected



# ----------
output <- lsar.chgpt(dat, max.arorder = 10, subinterval = c(1205, 1470),
                     candidate = c(1250,1400))


output$change.point


# -->
# point 1277 is detected



# ----------
output <- lsar.chgpt(dat, max.arorder = 10, subinterval = c(1278, 1470),
                     candidate = c(1300,1450))


output$change.point


# -->
# point 1411 is detected


output$change.point

output$subint



plot(dat, type = "l")
abline(v = c(527, 680, 1052, 1204, 1277, 1411), col = "blue", lwd = 1)
