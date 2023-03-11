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
# Assess the ACF and PACF of Asquared residuals of AR(1) fit
#   - If the noise term is ARCH, the squares of the residuals from the fit should behave like a non-Gaussian AR(1) process.
# ------------------------------------------------------------------------------

# AR(1) model
u <- sarima(djiar, p = 1, d = 0, q = 0)



# ----------
# ACF and PACF of the squared residuals of AR(1) fit
astsa::acf2(resid(u$fit) ^ 2, 20)

astsa::acf2(resid(u$fit), 20)


# -->
# NOT like AR(1) process ...



# ------------------------------------------------------------------------------
# Obtain squared residuals of AR(1) fit
# ------------------------------------------------------------------------------

fit <- arima(djiar, order = c(1, 0, 0))

y <- as.matrix(log(resid(fit)^2))

num <- length(y)


par(mfrow = c(1,1))
plot.ts(y, ylab = "")


