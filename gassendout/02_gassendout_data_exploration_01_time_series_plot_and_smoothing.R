setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\gassendout")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Gas Sendout
# ------------------------------------------------------------------------------

gas <- read.table("GasSendout.txt", sep = "", header = F, colClasses = "numeric")



colnames(gas) <- c("MetI", "gasSendout", "V3")


head(gas)




# ------------------------------------------------------------------------------
# data exploration:  time series plot
# ------------------------------------------------------------------------------

MTS::MTSplot(gas)




# ------------------------------------------------------------------------------
# data exploration:  Smoothing
# Smoothing splines
# ------------------------------------------------------------------------------

# In each interval, one fits a polynomial regression, typically the order is 3, and this is called cubic splines.
# Also a related method is smoothing splines, which minimizes a compromise between the fit and the degree of smoothness

graphics.off()

par(mfrow=c(2,1))


plot(gas$gasSendout, type = "l")
abline(v = seq(0, 400, by = 20), lty = 2, col = "gray")
lines(smooth.spline(time(gas$gasSendout), gas$gasSendout, spar = 0.25), lwd = 2, col = "blue")
lines(smooth.spline(time(gas$gasSendout), gas$gasSendout, spar = 0.5), lwd = 2, col = "red")
lines(smooth.spline(time(gas$gasSendout), gas$gasSendout, spar = 1), lwd = 2, col = "red")



plot(gas$MetI, type = "l")
abline(v = seq(0, 400, by = 20), lty = 2, col = "gray")
lines(smooth.spline(time(gas$MetI), gas$MetI, spar = 0.25), lwd = 2, col = "blue")
lines(smooth.spline(time(gas$MetI), gas$MetI, spar = 0.5), lwd = 2, col = "red")
lines(smooth.spline(time(gas$MetI), gas$MetI, spar = 1), lwd = 2, col = "red")



# -->
# some cycles are recognized:  weekly, yearly ?

