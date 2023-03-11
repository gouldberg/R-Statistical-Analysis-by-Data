setwd("//media//kswada//MyFiles//R//djia2")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  djia2 (Dow JOnes Industrial Average)
# ------------------------------------------------------------------------------

data(djia, package = "astsa")

str(djia)

djia


# library(TTR)
# djia <- getYahooData("^DJI", start = 20060420, end = 20160420, freq = "daily")

djiar <- diff(log(djia$Close))[-1]



# ------------------------------------------------------------------------------
# Estimate difference order
# ------------------------------------------------------------------------------

library(fracdiff)


dj.fd <- fracdiff(abs(djiar), nar = 0, nma = 0, M = 30)


dj.fd$d



# -->
# 0.23 < 0.5,  it is fractional noise, fractionally differenced series



# ----------
dj.fd$stderror.dpq


# -->
# Questionable result !!....



# ------------------------------------------------------------------------------
# Compare fractional differenced series and arima residuals
# ------------------------------------------------------------------------------

p <- rep(1,31)

for(k in 1:30){ p[k+1] = (k - dj.fd$d) * p[k] / ( k + 1 ) }

plot(1:30, p[-1], ylab = expression(pi(d), xlab = "Index", type = "h"))



# ----------
# fractionally differenced series:
# diffseries:  differenciate the time series data using the approximated binomial expression of the long-memory filter
# and an estimate of the memory parameter in the ARFIMA(p,d,q) model

res.fd <- diffseries(abs(djiar), dj.fd$d)



# arima residuals
res.arima <- resid(arima(abs(djiar), order = c(1,1,1)))



graphics.off()
par(mfrow = c(2,1))

acf(res.arima, 100, xlim = c(4, 97), ylim = c(-0.2, 0.8), main = "")
acf(res.fd, 100, xlim = c(4, 97), ylim = c(-0.2, 0.8), main = "")



# -->
# ??????  still auto-correlated

