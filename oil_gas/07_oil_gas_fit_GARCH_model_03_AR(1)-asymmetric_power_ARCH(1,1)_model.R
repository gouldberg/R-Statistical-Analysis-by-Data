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
# Fit AR(1)-APARCH(1,1) model (Asymmetric power ARCH model)
# ------------------------------------------------------------------------------

library(fGarch)


summary(oilgr.ap <- garchFit(~ arma(1,0) + aparch(1,1), data = oilgr, cond.dist = "std"))


par(mfrow = c(3, 3))
plot(oilgr.ap, which = "all")



# ----------
# sigma
oilgr.ap@sigma.t


# estimated parameter
oilgr.ap@fit$par



par(mfrow = c(1,1))
plot(oilgr, lty = 1, lwd = 1, type = "l")
lines(ts(oilgr.ap@sigma.t, start = c(2000, 1), frequency = 52), lty = 1, lwd = 1, col = "blue")




