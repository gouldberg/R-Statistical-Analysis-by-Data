setwd("//media//kswada//MyFiles//R//climhyd")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  climhyd
# ------------------------------------------------------------------------------

data(climhyd, package = "astsa")


str(climhyd)

head(climhyd)


inf <- log(climhyd$Inflow)

prec <- sqrt(climhyd$Precip)



# ------------------------------------------------------------------------------
# Assess ACF and PACF of precipitation
# ------------------------------------------------------------------------------


astsa::acf2(prec, max.lag = 48)



# -->
# ACF:  suggesting 1st order seasonal cycle (12 months)



# ------------------------------------------------------------------------------
# Precipitation can be fitted by a 1st order seasonal moving average ?
# ------------------------------------------------------------------------------

# Fit first-order seasonal moving average

# but Q = 4 is better ...
# fit <- sarima(prec, p = 0, d = 0, q = 0, P = 0, D = 0, Q = 4, S = 12)

fit <- sarima(prec, p = 0, d = 0, q = 0, P = 0, D = 0, Q = 1, S = 12)


fit

fit$ttable


( sma1 <- fit$ttable[1,1] )




# ------------------------------------------------------------------------------
# Prewhiten precipitaion
# ------------------------------------------------------------------------------

prec.pw <- resid(fit$fit)



# ----------
par(mfrow = c(2,1))

plot(prec, type = "l")

plot(prec.pw)



# ----------
acf2(prec)

acf2(prec.pw)




# ------------------------------------------------------------------------------
# filtered inflow
# ------------------------------------------------------------------------------

inf.fil <- stats::filter(inf, filter = c(1, -sma1), sides = 1)

# inf.fil <- stats::filter(inf.fil, filter = c(1, -sma2), sides = 1)
# inf.fil <- stats::filter(inf.fil, filter = c(1, -sma3), sides = 1)
# inf.fil <- stats::filter(inf.fil, filter = c(1, -sma4), sides = 1)




# ------------------------------------------------------------------------------
# Cross correlation
# ------------------------------------------------------------------------------

# Sample CCF of the prewhitened precipitaion and the similarly transformed inflow

par(mfrow = c(1,1))

ccf(prec.pw, inf.fil, ylab = "PrePrec vs fillInflow", na.action = na.omit, panel.first = grid())



# -->
# Negative lags indicate that precipitation leads inflow



# ----------
# for comparison
par(mfrow = c(1,1))

ccf(prec, inf, ylab = "PrePrec vs fillInflow", na.action = na.omit, panel.first = grid())
