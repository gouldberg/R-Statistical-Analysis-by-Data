rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\05_anomaly_detection\\qtdbsel102")



# ------------------------------------------------------------------------------
# data:  qtdbsel102
# ------------------------------------------------------------------------------


data <- read.csv(file = "qtdbsel102.txt", header = FALSE, sep = "")



str(data)


head(data)





# ----------

dat <- data[,2]





# ------------------------------------------------------------------------------
# Assess Trend in data
# ------------------------------------------------------------------------------

graphics.off()


plot(dat, type = "l")

lines(smooth.spline(time(dat), dat, spar = 0.75), col = "blue", lwd = 2)

lines(smooth.spline(time(dat), dat, spar = 1), col = "red", lwd = 2)




# ------------------------------------------------------------------------------
# removing trend
# ------------------------------------------------------------------------------


dat2 <- dat - smooth.spline(time(dat), dat, spar = 0.75)$y


graphics.off()

plot(dat2, type = "l")



# ----------
# To avoid zero value, add 1
dat2 <- dat2 + 1.0



# ------------------------------------------------------------------------------
# Box-Cox Transformation
# ------------------------------------------------------------------------------


( trans <- caret::BoxCoxTrans(dat2) )


dat3 <- predict(trans, newdata = dat2)



# ----------
graphics.off()

par(mfrow = c(2,1))

plot(dat2, type = "l")

plot(dat3, type = "l")




# ------------------------------------------------------------------------------
# Decompose time series to stationary subintervals and estimate local spectrum
# ------------------------------------------------------------------------------

library(TSSS)
library(timsac)


# ns0:  basic local span
# Note that when setting large max.order, first data points corresponding to those lags are ignored.
# (for example: change max.order = 10 to 20)

output <- TSSS::lsar(dat3, max.arorder = 30, ns0 = 500)

output2 <- timsac::mlocar(dat3, max.order = 30, span = 500, plot = FALSE)



# ----------
# start and end point  -->  should see timsac::mlocar output !!
output2$init

output2$end

unique(output2$init)



# ----------
graphics.off()

par(mfrow = c(1,1))


# use timsac::mlocar output !!
plot(dat3, type = "l")
abline(v = unique(output2$init), col = "blue", lwd = 1)




# ------------------------------------------------------------------------------
# Estimation of change points
# ------------------------------------------------------------------------------


output <- lsar.chgpt(dat3, max.arorder = 30, subinterval = c(100, 15000),
                     candidate = c(3500, 5500))


output$change.point



# -->
# point 4277 is detected


