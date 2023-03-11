rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\05_anomaly_detection\\simdata")



# ------------------------------------------------------------------------------
# data:  generation
# ------------------------------------------------------------------------------


set.seed(1)


tt <- 0.1


x1 <- seq(0, 10, by = tt)


x2 <- seq(10.1, 20, by = tt)


x3 <- seq(20.2, 30, by = tt)


y1 <- sin(pi * x1) + rnorm(length(x1), sd = 0.07)


y2 <- sin(2 * pi * x2) + rnorm(length(x2), sd = 0.07)


y3 <- sin(pi * x3) + rnorm(length(x3), sd = 0.07)


xi <- c(y1, y2, y3)




# ----------
par(mfrow = c(1,1))

plot(xi, type = "l")




# ------------------------------------------------------------------------------
# Decompose time series to stationary subintervals and estimate local spectrum
# ------------------------------------------------------------------------------

library(TSSS)
library(timsac)


# ns0:  basic local span
# Note that when setting large max.order, first data points corresponding to those lags are ignored.
# (for example: change max.order = 10 to 20)

output <- TSSS::lsar(x1, max.arorder = 10, ns0 = 50)

output2 <- timsac::mlocar(x1, max.order = 10, span = 50, plot = FALSE)




# ----------
# start and end point  -->  should see timsac::mlocar output !!
output2$init

output2$end

unique(output2$init)



# ----------
graphics.off()

par(mfrow = c(1,1))


# use timsac::mlocar output !!
plot(xi, type = "l")
abline(v = unique(output2$init), col = "blue", lwd = 1)



