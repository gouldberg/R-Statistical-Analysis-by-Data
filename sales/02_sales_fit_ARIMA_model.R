setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\10_time_series_basics\\sales")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  sales
# ------------------------------------------------------------------------------


dat <- read.csv("sales.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)


str(dat)


car::some(dat)



# ----------
colnames(dat) <- c("month", "fabrics", "machinery", "fuel")




# ------------------------------------------------------------------------------
# Fit SARIMA model
# ------------------------------------------------------------------------------


astsa::acf2(dat$fabrics, max.lag = 40)


astsa::acf2(diff(dat$fabrics), max.lag = 40)



# try random walk + noize:  ARIMA(0,1,1)
sarima011 <- astsa::sarima(dat$fabrics, p = 0, d = 1, q = 1, no.constant = TRUE)


# add AR(1):  ARIMA(1,1,1)
sarima111 <- astsa::sarima(dat$fabrics, p = 1, d = 1, q = 1, no.constant = TRUE)


# ARIMA(0,1,1) * (0,1,1)(6)
sarima0110116 <- astsa::sarima(dat$fabrics, p = 0, d = 1, q = 1, P = 0, D = 1, Q = 1, S = 6)


# add MA term: ARIMA(0,1,1) * (0,1,6)(6)
sarima0110166 <- astsa::sarima(dat$fabrics, p = 0, d = 1, q = 1, P = 0, D = 1, Q = 6, S = 6)



# add MA term: ARIMA(0,1,2) * (0,1,6)(6)
sarima0120166 <- astsa::sarima(dat$fabrics, p = 0, d = 1, q = 2, P = 0, D = 1, Q = 6, S = 6)



# -->
# MA(2) is not significant ...




# ----------
graphics.off()

par(mfrow = c(1,1))

ts.plot(dat$fabrics, ylim = c(650, 1150))

lines(dat$fabrics - resid(sarima0110166$fit), lwd = 1, col = "blue")


