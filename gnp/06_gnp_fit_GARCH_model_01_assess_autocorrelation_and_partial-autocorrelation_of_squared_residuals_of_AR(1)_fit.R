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
# Assess the ACF and PACF of Asquared residuals of AR(1) fit
#   - It has been suggested that the U.S. GNP series has ARCH errors.
#     If the GNP noise term is ARCH, the squares of the residuals from the fit should behave like a non-Gaussian AR(1) process.
# ------------------------------------------------------------------------------

gnpgr <- diff(log(gnp))


# AR(1) model
u <- sarima(gnpgr, p = 1, d = 0, q = 0)



# ----------
# ACF and PACF of the squared residuals of AR(1) fit
astsa::acf2(resid(u$fit) ^ 2, 20)



# -->
# It appears that there may be some dependence, albeit, small, left in the redisulas.



# ------------------------------------------------------------------------------
# Obtain squared residuals of AR(1) fit
# ------------------------------------------------------------------------------

fit <- arima(gnpgr, order = c(1, 0, 0))

y <- as.matrix(log(resid(fit)^2))

num <- length(y)


par(mfrow = c(1,1))
plot.ts(y, ylab = "")


