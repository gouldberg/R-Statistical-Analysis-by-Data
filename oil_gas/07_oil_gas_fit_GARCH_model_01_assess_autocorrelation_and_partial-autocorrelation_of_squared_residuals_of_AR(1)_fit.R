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
# Assess the ACF and PACF of squared residuals of AR(1) fit
#   - If the GNP noise term is ARCH, the squares of the residuals from the fit should behave like a non-Gaussian AR(1) process.
# ------------------------------------------------------------------------------

oilgr <- diff(log(oil))


# AR(1) model
u <- sarima(oil, p = 1, d = 0, q = 0)



# ----------
# ACF and PACF of the squared residuals of AR(1) fit
astsa::acf2(resid(u$fit) ^ 2, 20)



# -->
# This is NOT AR(1)-GARCH(1,0) (like GNP growth rate), but like AR(1)-GARCH(1,1)



# ------------------------------------------------------------------------------
# Obtain squared residuals of AR(1) fit
# ------------------------------------------------------------------------------

fit <- arima(oilgr, order = c(1, 0, 0))

y <- as.matrix(log(resid(fit)^2))

num <- length(y)


par(mfrow = c(1,1))

plot.ts(y, ylab = "")


