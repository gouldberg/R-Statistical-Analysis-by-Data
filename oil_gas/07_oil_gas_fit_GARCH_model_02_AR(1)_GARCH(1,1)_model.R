setwd("//media//kswada//MyFiles//R//oil_gas")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Oil and Gas
# ------------------------------------------------------------------------------

data(oil, package = "astsa")
data(gas, package = "astsa")

str(oil)
str(gas)

oil
gas



# ------------------------------------------------------------------------------
# Fit AR(1,0) + GARCh(1,1) model
# ------------------------------------------------------------------------------


library(fGarch)


summary(gfit <- garchFit(~ arma(1, 0) + garch(1, 1), oilgr, cond.dist = "std"))



# ----------
graphics.off()

par(mfrow = c(3,3))

plot(gfit, which = "all")




# ----------
# sigma
gfit@sigma.t


# estimated parameter
gfit@fit$par



par(mfrow = c(1,1))
plot(oilgr, lty = 1, lwd = 1, type = "l")
lines(ts(gfit@sigma.t, start = c(2000, 1), frequency = 52), lty = 1, lwd = 1, col = "blue")

