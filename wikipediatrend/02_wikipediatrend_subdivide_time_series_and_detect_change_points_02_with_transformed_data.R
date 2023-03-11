rm(list = ls())

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\05_anomaly_detection\\wikipediatrend")



# ------------------------------------------------------------------------------
# data:  wikipediatrend
# ------------------------------------------------------------------------------


data <- read.csv(file = "wikipediatrend.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)



str(data)


head(data)



# ----------

data$date <- as.Date(data$date)


# dat2 <- data

dat2 <- data %>% filter(date >= "2016-01-01", date < "2018-06-01") %>% dplyr::select(views) %>% .[[1]]



# ------------------------------------------------------------------------------
# Assess Trend in data
# ------------------------------------------------------------------------------

graphics.off()


plot(dat2, type = "l")

lines(smooth.spline(time(dat2), dat2, spar = 0.75), col = "blue", lwd = 2)

lines(smooth.spline(time(dat2), dat2, spar = 1), col = "red", lwd = 2)




# ------------------------------------------------------------------------------
# removing trend
# ------------------------------------------------------------------------------


dat2 <- dat2 - smooth.spline(time(dat2), dat2, spar = 0.75)$y


graphics.off()

plot(dat2, type = "l")



# ----------
# convert to positive value

summary(dat2)

dat2 <- dat2 + 70



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

output <- TSSS::lsar(dat3, max.arorder = 20, ns0 = 50)

output2 <- timsac::mlocar(dat3, max.order = 20, span = 50, plot = FALSE)



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




