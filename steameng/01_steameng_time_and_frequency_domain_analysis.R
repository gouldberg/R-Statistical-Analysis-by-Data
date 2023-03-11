setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\steameng")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS", "TSSS", "timsac")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  SteamEng
#   - Input:  Pressure and MagVolt
#   - Output: GenVolt, Speed
# ------------------------------------------------------------------------------


data <- read.table(file = "SteamEng.txt", header = T, sep = "\t", stringsAsFactors = FALSE)


str(data)


car::some(data)



# ----------
pressure <- ts(data$pressure)

magvolt <- ts(data$magvolt)

genvolt <- ts(data$genvolt)

speed <- ts(data$speed)




# ------------------------------------------------------------------------------
# data exploration:  time series plot
# ------------------------------------------------------------------------------


MTSplot(data)



# ----------
# GenVolt

par(mfrow = c(1,1))

plot(genvolt)

lines(smooth.spline(genvolt, spar = 0.5), col = "blue", lwd = 2)

lines(smooth.spline(genvolt, spar = 1), col = "red", lwd = 2)




# ----------
# Speed

par(mfrow = c(1,1))

plot(speed)

lines(smooth.spline(speed, spar = 0.5), col = "blue", lwd = 2)

lines(smooth.spline(speed, spar = 1), col = "red", lwd = 2)





# ------------------------------------------------------------------------------
# auto-correlation and partial auto-correlation
# ------------------------------------------------------------------------------


apply(data, 2, forecast::ndiffs)




# ----------
# AR(1)
acf2(genvolt, max.lag = 100)


sarima(genvolt, p = 1, d = 0, q = 0, no.constant = TRUE)



# AR(3)
acf2(speed, max.lag = 100)


sarima(speed, p = 3, d = 0, q = 0, no.constant = TRUE)



acf2(pressure, max.lag = 100)





# ------------------------------------------------------------------------------
# cross-correlation
# ------------------------------------------------------------------------------


# GenVolt
# genvolt lag 7
ccf(genvolt, pressure)


# genvolt lag 1, 2
ccf(genvolt, magvolt)



# ----------
# speed lag 2 - 13  (max at 5)
ccf(speed, pressure)


ccf(speed, magvolt)





# ------------------------------------------------------------------------------
# Periodogram
# ------------------------------------------------------------------------------


per <- astsa::mvspec(genvolt, log = "no", type = "h", spans = c(7,7), taper = 0.25)



# ----------
per <- astsa::mvspec(speed, log = "no", type = "h", spans = c(7,7), taper = 0.25)



# ----------
per <- astsa::mvspec(magvolt, log = "no", type = "h", spans = c(7,7), taper = 0.25)



# ----------
per <- astsa::mvspec(pressure, log = "no", type = "h", spans = c(7,7), taper = 0.25)



# ----------
# parametric spectral estimation

# AR(1) as expected
spec.ar(genvolt, method = "burg")


# AR(3) as expected
spec.ar(speed, method = "burg")




# ------------------------------------------------------------------------------
# squared coherency
# ------------------------------------------------------------------------------


L <- 15
( m <- (L - 1) / 2 )
sr <- astsa::mvspec(data, kernel("daniell", m), taper = 0.5, plot = "FALSE")



# significance level alpha = 0.001
( f <- qf(.999, 2, sr$df -2) )

( C <- f / (L - 1 + f) )


par(mfrow = c(2,1))

plot(sr)

plot(sr, plot.type = "coh", ci.lty = 2)

abline(h = C)




# -->
# for genvolt, magvolt has very high coherency
# pressure has some coherency to genvolt around frequency = 0.12 and 0.35




# ------------------------------------------------------------------------------
# Cross spectra and power contribution by multivariate AR model
# ------------------------------------------------------------------------------


library(TSSS)



# Yule-Walker Method of Fitting Multivariate AR Model Fitting
marf <- marfit(as.matrix(data), lag = 20)



names(marf)


marf$aic



# AR(6) is AIC minimum
marf$maice.order


marf$arcoef


# covariance matrix
round(marf$v, 4)


# correlation matrix
diag(marf$v)^(-0.5) * marf$v * diag(marf$v)^(-0.5)



# -->
# very high and low correlation between magvolt and genvolt and speed




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
muln <- mulnos(as.matrix(data), max.order = 10, h = 500)


muln$integr


# ----------
graphics.off()

par(mfrow = c(2,2))

matplot(t(muln$integr[1,,]), type = "l", ylim = c(0, 1))

matplot(t(muln$integr[2,,]), type = "l", ylim = c(0, 1))

matplot(t(muln$integr[3,,]), type = "l", ylim = c(0, 1))

matplot(t(muln$integr[4,,]), type = "l", ylim = c(0, 1))




# ------------------------------------------------------------------------------
# Regression with lagged variables:  impulse response function
# ------------------------------------------------------------------------------

# L:  degree of smoothing
# M:  must be even, number of terms used in the lagged regression, abs(t) >= M/2
# threshold:  the cut-off used to set small (in absolute value) regression coefficients equal to zero


# ----------
# magvolt to genvolt

mod_mtog <- astsa::LagReg(input = magvolt, output = genvolt, L = 15, M = 60, threshold = 0.01)


# -->
# lag 1 is very important



# ----------
# pressure to genvolt

mod_ptog <- astsa::LagReg(input = pressure, output = genvolt, L = 3, M = 60, threshold = 0.01)

armod <- arima(genvolt, order = c(1, 0, 0))

gen.pw <- resid(armod)

pre.fil <- stats::filter(pressure, filter = c(1, -c(coef(armod)[1])), sides = 1)

mod_ptog <- astsa::LagReg(input = pre.fil[2:451], output = gen.pw[2:451], L = 3, M = 60, threshold = 0.01)


# -->
# lag 3, 6 and 7 is very important



# ----------
# magvolt to speed

mod_mtos <- astsa::LagReg(input = magvolt, output = speed, L = 15, M = 60, threshold = 0.001)



# ----------
# pressure to speed

mod_ptos <- astsa::LagReg(input = pressure, output = speed, L = 15, M = 60, threshold = 0.001)


# -->
# cross correlation and coefficients bata are very similar





# ------------------------------------------------------------------------------
# Spectral Matrix
# ------------------------------------------------------------------------------

Y <- data[,c("magvolt", "genvolt", "pressure")]

L <- 15

# Spectral Matrix
par(mfrow = c(1,1))

Yspec <- mvspec(Y, spans = L, kernel = "daniell", detrend = TRUE, demean = FALSE, taper = 0.1)


Yspec$spec




# ----------
# effective sample size
( n <- Yspec$n.used )


# fundamental freqs
( Fr <- Yspec$freq )


# number of frequencies inputs (Temp and Precip)
( n.freq <- length(Fr) )


# bandwidth
Yspec$bandwidth



# ----------
# Squared Coherencies

alpha <- 0.001


Fq <- qf(1 - alpha, 2, L- 2)


# 0.001 threshold corresponding to the F-statistic, separately, for each possible predictor of inflow.
cn <- Fq / (L - 1 + Fq)



graphics.off()

par(mfrow = c(2,2),  cex.lab = 1.2)

for(i in 1:3){
  plot(Fr, Yspec$coh[,i], type = "l", ylab = "Sq Coherence", xlab = "Frequency", ylim = c(0, 1)) 
  abline(h = cn)
}



# -->
# Transformed (square root) precipidation produces the most consistently high squared coherence values
# at all frequencies (L = 25),
# with the seasonal period contributing most significantly.

# Other inputs, with the exception of wind speed, also appear to be plausible contributions.




# ------------------------------------------------------------------------------
# Multiple coherency:  for genvolt
# ------------------------------------------------------------------------------


Y <- data[,c("magvolt", "pressure", "genvolt")]


L <- 15
M <- 60

Yspec <- mvspec(Y, spans = L, kernel = "daniell", detrend = TRUE, demean = FALSE, taper = 0.1)
( Fr <- Yspec$freq )


plot(Yspec, plot.type = "coh")




# ----------
# We focus on the analysis with two predictor series, magvolt and pressure
# Number of inputs (genvolt and pressure)
nq <- 2



# Multiple coherency
# stoch.reg:  frequency domain stochasitc regression  --> plot.which = "coh"
# 1: genvolt   5: Precip

coh.12 <- stoch.reg(Y, cols.full = c(1,2), cols.red = NULL, alpha, L, M, plot.which = "coh")



# -->
# The additional contribution of pressure to genvolt is marginal, but contributes at low frequency




# ------------------------------------------------------------------------------
# Multiple coherency:  for speed
# ------------------------------------------------------------------------------


Y <- data[,c("magvolt", "pressure", "speed")]

L <- 15
M <- 60


Yspec <- mvspec(Y, spans = L, kernel = "daniell", detrend = TRUE, demean = FALSE, taper = 0.1)
( Fr <- Yspec$freq )


plot(Yspec, plot.type = "coh")




# ----------
# We focus on the analysis with two predictor series, magvolt and pressure
# Number of inputs (genvolt and pressure)
nq <- 2



# Multiple coherency
# stoch.reg:  frequency domain stochasitc regression  --> plot.which = "coh"
# 1: genvolt   5: Precip


coh.12 <- stoch.reg(Y, cols.full = c(1,2), cols.red = NULL, alpha, L, M, plot.which = "coh")



# -->
# The additional contribution of magvolt to speed is almost nothing




# ------------------------------------------------------------------------------
# Lagged regression for genvolt
# ------------------------------------------------------------------------------


g_t <- ts.intersect(genvolt = stats::lag(genvolt, 0),
                       m1 = stats::lag(magvolt, -1),
                       p3 = stats::lag(pressure, -3),
                       p6 = stats::lag(pressure, -6),
                       p7 = stats::lag(pressure, -7))


( u0 <- lm(genvolt ~ m1 + p3 + p6 + p7, data = g_t, na.action = NULL) )

( u37 <- lm(genvolt ~ m1 + p3 + p7, data = g_t, na.action = NULL) )


summary(u0)

summary(u37)



# ----------
# the model can not be simplified (p7 is required)
anova(u37, u0)



# ----------
acf2(resid(u0))




# ----------
# BUT NOTE THAT Pressure is not required when including AR(3) terms ...
# if included, not significant

arx_g <- sarima(g_t[,1], 3, 0, 1, xreg = g_t[,c(2)])

arx_g$ttable

arx_g$AIC



# ----------
pred <- genvolt[-c(1:7)] - resid(arx_g$fit)

par(mfrow = c(1,1))

ts.plot(genvolt[-c(1:7)], pred, col = c('gray', "black"), lwd = c(3,1))






# ------------------------------------------------------------------------------
# Lagged regression for speed
# ------------------------------------------------------------------------------


s_t <- ts.intersect(speed = stats::lag(speed, 0),
                    p3 = stats::lag(pressure, -3),
                    p4 = stats::lag(pressure, -4),
                    p5 = stats::lag(pressure, -5),
                    p6 = stats::lag(pressure, -6),
                    p7 = stats::lag(pressure, -7),
                    p8 = stats::lag(pressure, -8),
                    p9 = stats::lag(pressure, -9),
                    p10 = stats::lag(pressure, -10),
                    p11 = stats::lag(pressure, -11),
                    p12 = stats::lag(pressure, -12),
                    p13 = stats::lag(pressure, -13))


( u0 <- lm(speed ~ ., data = s_t, na.action = NULL) )

( u12 <- lm(speed ~ . - p13, data = s_t, na.action = NULL) )


summary(u0)

summary(u12)



# ----------
# the model can not be simplified (p13 is required)
anova(u12, u0)



# ----------
acf2(resid(u0))




# ----------
arx_s <- sarima(s_t[,1], 1, 0, 0, xreg = s_t[,c(2:11)])

arx_s$ttable

arx_s$AIC




# ----------
pred <- speed[-c(1:13)] - resid(arx_s$fit)

par(mfrow = c(1,1))

ts.plot(speed[-c(1:13)], pred, col = c('gray', "black"), lwd = c(3,1))

