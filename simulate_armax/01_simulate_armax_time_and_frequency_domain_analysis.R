setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\simulate_armax")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS", "TSSS", "timsac", "sysid")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  simulate ARMAX model data
# ------------------------------------------------------------------------------

# define box-jenkins ARMAX model

A <- c(1, -1.5, 0.7)

B <- c(0, 1, 0.5)

C <- c(1, -1, 0.2)


mod_bj <- idpoly(A, B, C, unit = "seconds", Ts = 1)



# ----------
# generate input signals
# ’rbs’:  generates random binary signal

( u <- idinput(n = 350, type = "rbs") )
u <- ts(u)


# ----------
# simulate response of dynamic system

( y <- sim(mod_bj, u, addNoise = TRUE) )
y <- ts(y)



# ----------
# convert to idframe
z <- ts.intersect(u, y)
# z <- idframe(output = y, input = u, Ts = 1)




# ------------------------------------------------------------------------------
# data exploration:  time series plot
# ------------------------------------------------------------------------------


MTSplot(z)



# ----------
par(mfrow = c(1,1))

plot(y, type = "l")

lines(smooth.spline(y, spar = 0.5), col = "blue", lwd = 2)

lines(smooth.spline(y, spar = 1), col = "red", lwd = 2)




# ------------------------------------------------------------------------------
# auto-correlation and partial auto-correlation
# ------------------------------------------------------------------------------


apply(z, 2, forecast::ndiffs)




# ----------
# AR(3) ?
acf2(y, max.lag = 100)


sarima(y, p = 3, d = 0, q = 0, no.constant = TRUE)




# ------------------------------------------------------------------------------
# cross-correlation
# ------------------------------------------------------------------------------


ccf(y, u)




# ------------------------------------------------------------------------------
# Periodogram
# ------------------------------------------------------------------------------


per <- astsa::mvspec(y, log = "no", type = "h")


# max spectrum at freq = 0.044 (0.044 cycle per observation)
per$freq[which(per$spec == max(per$spec))]


# 1 cycle per 22.5 points
1 / per$freq[which(per$spec == max(per$spec))]



# ----------
per <- astsa::mvspec(y, spans = c(7,7), taper = 0.25, log = "no", type = "l")


# max spectrum at freq = 0.044 (0.044 cycle per observation)
per$freq[which(per$spec == max(per$spec))]




# ----------
# parametric spectral estimation --> AR(3), peak is at some little different peak

spec.ar(y, method = "yule-walker")




# ------------------------------------------------------------------------------
# squared coherency
# ------------------------------------------------------------------------------


# sr <- astsa::mvspec(z, spans = c(7,7), taper = 0.1, plot = "FALSE")


L <- 15
( m <- (L - 1) / 2 )
sr <- astsa::mvspec(z, kernel("daniell", m), plot = "FALSE")



# significance level alpha = 0.001
( f <- qf(.999, 2, sr$df -2) )

( C <- f / (L - 1 + f) )


par(mfrow = c(2,1))

plot(sr)

plot(sr, plot.type = "coh", ci.lty = 2)

abline(h = C)





# ------------------------------------------------------------------------------
# Cross spectra and power contribution by multivariate AR model
# ------------------------------------------------------------------------------


library(TSSS)



# Yule-Walker Method of Fitting Multivariate AR Model Fitting
marf <- marfit(as.matrix(z), lag = 20)



names(marf)


marf$aic



# AR(6) is AIC minimum
marf$maice.order


marf$arcoef


# covariance matrix
round(marf$v, 4)


# correlation matrix
diag(marf$v)^(-0.5) * marf$v * diag(marf$v)^(-0.5)




# ----------
# Cross spectra and power contribution

graphics.off()


# v:  innovation variance matrix
marspc(marf$arcoef, v = marf$v)




# -->
# 1st page:  spectrum, amplitude and phase
# note that phase is discontinuous due to its representation defined in [-pi < phi < phi]


# 2nd page:  spectrum and coherency

# 3rd page:  noise contribution (cumulative absolute, cumulative relative ratio)




# ----------
library(timsac)

# h  frequencies
muln <- mulnos(as.matrix(z), max.order = 10, h = 500)


muln$integr


# ----------
graphics.off()

par(mfrow = c(2,2))

matplot(t(muln$integr[1,,]), type = "l", ylim = c(0, 1))

matplot(t(muln$integr[2,,]), type = "l", ylim = c(0, 1))




# ------------------------------------------------------------------------------
# Regression with lagged variables:  impulse response function
# ------------------------------------------------------------------------------

# L:  degree of smoothing
# M:  must be even, number of terms used in the lagged regression, abs(t) >= M/2
# threshold:  the cut-off used to set small (in absolute value) regression coefficients equal to zero


mod_l <- astsa::LagReg(input = u, output = y, L = 15, M = 40, threshold = 0.005)



# -->
# note that impulse response and cross-correlations are very similar




# ------------------------------------------------------------------------------
# Lagged regression
# ------------------------------------------------------------------------------


z_t <- ts.intersect(output = stats::lag(y, 0),
                       i1 = stats::lag(u, -1),
                       i2 = stats::lag(u, -2),
                       i3 = stats::lag(u, -3),
                       i4 = stats::lag(u, -4),
                       i5 = stats::lag(u, -5),
                       i6 = stats::lag(u, -6),
                    i7 = stats::lag(u, -7))


( u7 <- lm(output ~ i1 + i2 + i3 + i4 + i5 + i6 + i7, data = z_t, na.action = NULL) )

( u6 <- lm(output ~ i1 + i2 + i3 + i4 + i5 + i6, data = z_t, na.action = NULL) )

( u5 <- lm(output ~ i1 + i2 + i3 + i4 + i5, data = z_t, na.action = NULL) )

( u4 <- lm(output ~ i1 + i2 + i3 + i4, data = z_t, na.action = NULL) )

( u3 <- lm(output ~ i1 + i2 + i3, data = z_t, na.action = NULL) )

( u2 <- lm(output ~ i1 + i2, data = z_t, na.action = NULL) )

( u1 <- lm(output ~ i1, data = z_t, na.action = NULL) )



summary(u7)


summary(u6)



# ----------
# the model can be simplified (u2)
anova(u5, u6)


AIC(u6, u5, u4, u3, u2, u1)


# -->
# best is u6



# ----------

acf2(resid(u6))




# ----------

arx6 <- sarima(z_t[,1], 1, 0, 5, xreg = z_t[,c(2:7)])

arx6$ttable


arx6$AIC



# ----------

pred <- y[-c(1:7)] - resid(arx6$fit)

par(mfrow = c(1,1))

ts.plot(y[-c(1:7)], pred, col = c('gray', "black"), lwd = c(3,1))


