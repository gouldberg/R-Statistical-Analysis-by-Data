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
# Fit AR(1)-APARCH(1,1) model (Asymmetric power ARCH model)
# ------------------------------------------------------------------------------

library(xts)
library(fGarch)


summary(djia.ap <- garchFit(~ arma(1,0) + aparch(1,1), data = djiar, cond.dist = "std"))


par(mfrow = c(3, 3))
plot(djia.ap, which = "all")



# ----------
# volatility
djia.ap@sigma.t



# ----------
# APARCH one-step-ahead predictions of the DJIA volatility, sigma.t, superimposed on part of the data including the financial crisis of 2008
graphics.off()
par(mfrow = c(1, 1))
plot(as.ts(djiar[400:900]), lty = 1, lwd = 1)
lines(djia.ap@sigma.t[400:900], lty = 1, lwd = 1, col = "blue")



