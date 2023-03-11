setwd("//media//kswada//MyFiles//R//lynx")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  lynx
# ------------------------------------------------------------------------------

data(lynx, package = "datasets")


str(lynx)


lynx



# ----------
lynxl <- log10(lynx)

dlynxl <- diff(log10(lynx))




# ------------------------------------------------------------------------------
# model for time series up to 1924
# ------------------------------------------------------------------------------


# time series up to 1924
x1 <- window(lynxl, end = 1924)



# time series:  last 10 time points
x2 <- window(lynxl, start = 1925)




# ----------
# model for x1 by setar and lstar

set.x1 <- setar(x1, m = 2, thDelay = 1)


ls.x1 <- lstar(x1, m = 2, thDelay = 1)




# ------------------------------------------------------------------------------
# forecasting last 10 time series
# ------------------------------------------------------------------------------


# forecasting by MCMC  (errors are assumed to Gaussian)

set.pred <- predict(set.x1, n.ahead = 10, type = c("MC"), nboot = 10000, ci = 0.95, boot1Zero = FALSE)

ls.pred <- predict(ls.x1, n.ahead = 10, type = c("MC"), nboot = 10000, ci = 0.95, boot1Zero = FALSE)


# forecasting by bootstrapping  (Gaussian assumption is not required)
ls2.pred <- predict(ls.x1, n.ahead = 10, type = c("bootstrap"), nboot = 10000, ci = 0.95, boot1Zero = FALSE)



set.pred

ls.pred



# ----------
graphics.off()

par(mfrow = c(2,2), ask = FALSE)

plot(lynxl, ylim = c(1.5, 4.5), lwd = 3)
abline(v = 1925, col = "gray", lty = 2)


plot(x2, ylim = c(1.5, 4.5), lwd = 3)

lines(set.pred$pred, lty = 2, col = 2, lwd = 3)
lines(set.pred$se[,1], lty = 3, col = 3, lwd = 3)
lines(set.pred$se[,2], lty = 3, col = 3, lwd = 3)


plot(x2, ylim = c(1.5, 4.5), lwd = 3)

lines(ls.pred$pred, lty = 2, col = 2, lwd = 3)
lines(ls.pred$se[,1], lty = 3, col = 3, lwd = 3)
lines(ls.pred$se[,2], lty = 3, col = 3, lwd = 3)


plot(x2, ylim = c(1.5, 4.5), lwd = 3)

lines(ls2.pred$pred, lty = 2, col = 2, lwd = 3)
lines(ls2.pred$se[,1], lty = 3, col = 3, lwd = 3)
lines(ls2.pred$se[,2], lty = 3, col = 3, lwd = 3)



