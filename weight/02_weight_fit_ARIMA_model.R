setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\10_time_series_basics\\weight")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  weight
# ------------------------------------------------------------------------------


dat <- read.table("Weight.dat", header = FALSE)


str(dat)


car::some(dat)



# ----------
colnames(dat) <- c("weight")




# ------------------------------------------------------------------------------
# Fit ARIMA model
# ------------------------------------------------------------------------------


# ARIMA(1,1,2)
astsa::sarima(dat$weight, p = 1, d = 1, q = 2)



# -->
# MA(2) is not significant and constant is not required.




# ----------
# ARIMA(1,1,1)
arima111 <- astsa::sarima(dat$weight, p = 1, d = 1, q = 1, no.constant = TRUE)


arima111



# ----------
# If this is random walk + noise --> ARIMA(0,1,1)
arima011 <- astsa::sarima(dat$weight, p = 0, d = 1, q = 1, no.constant = TRUE)



arima011



# -->
# AIC is divided by length(dat$weight)
# AIC:  ARIMA(1,1,1) < ARIMA(0,1,1)
# BIC:  ARIMA(1,1,1) < ARIMA(0,1,1)




# ----------
graphics.off()

par(mfrow = c(1,1))

ts.plot(dat$weight, type = "b", ylim = c(82, 88))

lines(dat$weight - resid(arima111$fit), lwd = 1, col = "orange")

lines(dat$weight - resid(arima011$fit), lwd = 1, col = "blue")

