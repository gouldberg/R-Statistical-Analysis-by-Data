setwd("//media//kswada//MyFiles//R//gnp")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  GNP data
# ------------------------------------------------------------------------------

data(gnp, package = "astsa")

str(gnp)

head(gnp)



# ------------------------------------------------------------------------------
# Forecasting by sarima.for
# ------------------------------------------------------------------------------

( xreg <- time(gnp) )

n.ahead <- 48
( newxreg <- xreg[length(gnp)] + seq(1, n.ahead)/4 )



# ----------
# MA(2) for log diff
for_ma2 <- sarima.for(log(gnp), n.ahead = n.ahead, p = 0, d = 1, q = 2, xreg = xreg, newxreg = newxreg)

# AR(1) for log diff
for_ar1 <- sarima.for(log(gnp), n.ahead = n.ahead, p = 1, d = 1, q = 0, xreg = xreg, newxreg = newxreg)

# MA(3) for log diff
for_ma3 <- sarima.for(log(gnp), n.ahead = n.ahead, p = 0, d = 1, q = 3, xreg = xreg, newxreg = newxreg)

# AR(2) for log diff
for_ar2 <- sarima.for(log(gnp), n.ahead = n.ahead, p = 2, d = 1, q = 0, xreg = xreg, newxreg = newxreg)

# ARMA(1,2) for log diff
for_arma12 <- sarima.for(log(gnp), n.ahead = n.ahead, p = 1, d = 1, q = 2, xreg = xreg, newxreg = newxreg)




# ----------
graphics.off()
par(mfrow = c(3,2))

ts.plot(log(gnp), for_ma2$pred, lwd = 2, main = "MA(2)")
lines(for_ma2$pred + for_ma2$se, col = 4, lty = 2)
lines(for_ma2$pred - for_ma2$se, col = 4, lty = 2)

ts.plot(log(gnp), for_ar1$pred, lwd = 2, main = "AR(1)")
lines(for_ar1$pred + for_ar1$se, col = 4, lty = 2)
lines(for_ar1$pred - for_ar1$se, col = 4, lty = 2)

ts.plot(log(gnp), for_ma3$pred, lwd = 2, main = "MA(3)")
lines(for_ma3$pred + for_ma3$se, col = 4, lty = 2)
lines(for_ma3$pred - for_ma3$se, col = 4, lty = 2)

ts.plot(log(gnp), for_ar2$pred, lwd = 2, main = "AR(2)")
lines(for_ar2$pred + for_ar2$se, col = 4, lty = 2)
lines(for_ar2$pred - for_ar2$se, col = 4, lty = 2)

ts.plot(log(gnp), for_arma12$pred, lwd = 2, main = "ARMA(1,2)")
lines(for_arma12$pred + for_arma12$se, col = 4, lty = 2)
lines(for_arma12$pred - for_arma12$se, col = 4, lty = 2)



# -->
# almost no difference ...



