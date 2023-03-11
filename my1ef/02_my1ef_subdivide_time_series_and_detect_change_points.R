setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\my1ef")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  MYE1F
# ------------------------------------------------------------------------------


dat <- read.table("MYE1F.txt", sep = "", header = T, colClasses = "numeric")


head(dat)


dat <- dat$x




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
                     candidate = c(400, 800))


output$change.point



# -->
# point 630 is detected



# ----------
output <- lsar.chgpt(dat, max.arorder = 10, subinterval = c(600, 1400),
                     candidate = c(800, 1200))


output$change.point


# -->
# point 1026 is detected



# ----------
# How about detection in overall ??
output <- lsar.chgpt(dat, max.arorder = 10, subinterval = c(200, 2600),
                     candidate = c(400, 2000))


output$change.point

output$subint


