setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\hakusan")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Hakusan
#   - data point recorded at each 1 second
# ------------------------------------------------------------------------------


dat <- read.table("HAKUSAN.txt", sep = "", header = T, colClasses = "numeric")


head(dat)




# ------------------------------------------------------------------------------
# data exploration:  time series plot
# ------------------------------------------------------------------------------


MTS::MTSplot(dat)



apply(dat, 2, forecast::ndiffs)




# ------------------------------------------------------------------------------
# data exploration:  demean
# ------------------------------------------------------------------------------


dat$YawRate <- dat$YawRate - mean(dat$YawRate)

dat$Rolling <- dat$Rolling - mean(dat$Rolling)

dat$Pitching <- dat$Pitching - mean(dat$Pitching)

dat$Rudder <- dat$Rudder - mean(dat$Rudder)



MTS::MTSplot(dat)




# ------------------------------------------------------------------------------
# data exploration:  time series decompose
# ------------------------------------------------------------------------------


timsac::decomp(dat$Rolling)


timsac::decomp(dat$Pitching)




# ------------------------------------------------------------------------------
# data exploration:  moothing splines
# ------------------------------------------------------------------------------


graphics.off()

par(mfrow=c(2,2))

plot(dat$Rudder, type = "l")

lines(smooth.spline(time(dat$Rudder), dat$Rudder, spar = 0.5), lwd = 2, col = "blue")


plot(dat$YawRate, type = "l")

lines(smooth.spline(time(dat$YawRate), dat$YawRate, spar = 0.5), lwd = 2, col = "blue")


plot(dat$Pitching, type = "l")

lines(smooth.spline(time(dat$Pitching), dat$Pitching, spar = 0.5), lwd = 2, col = "blue")


plot(dat$Rolling, type = "l")

lines(smooth.spline(time(dat$Rolling), dat$Rolling, spar = 0.5), lwd = 2, col = "blue")




# ------------------------------------------------------------------------------
# Correlation analysis:  autocorrelation and partial-correaltion
# ------------------------------------------------------------------------------


graphics.off()

astsa::acf2(dat$Rolling, max.lag = 50)


sarima(dat$Rolling, p = 5, d = 0, q = 0, no.constant = TRUE)



# -->
# difficult to model by itself




# ----------
astsa::acf2(dat$Pitching, max.lag = 50)


sarima(dat$Rolling, p = 5, d = 0, q = 0, no.constant = TRUE)




# ------------------------------------------------------------------------------
# Correlation analysis:  cross-correlation
# ------------------------------------------------------------------------------


acf(dat, lag = 50)


# -->
# Rolling:  seems not to be controlled by Rudder very well (large cross correlation and long oscillating lags)
# As expected, there seems to be YawRate feedback to Rudder  (Rudder has cross correlation at negative lag)




# ------------------------------------------------------------------------------
# Spectral analysis:  periodogram
# ------------------------------------------------------------------------------


nextn(nrow(dat))




# ----------
# Raw Periodogram

graphics.off()

par(mfrow = c(2,2))


astsa::mvspec(dat$YawRate, log = "no")

astsa::mvspec(dat$Pitching, log = "no")

astsa::mvspec(dat$Rolling, log = "no")

astsa::mvspec(dat$Rudder, log = "no")




# ----------
# Smoothd Periodogram

L <- 25

( m <- (L - 1) / 2 )

par(mfrow = c(1,1))

# need "scale"
mvspec(scale(dat), kernel("daniell", m), taper = 0.1, 
       col = c("red", "blue", "orange", "black"), lwd = c(2,2,2,2), lty = c(1,1,1,1))



# ----------
# YawRate:
# peak at around freq = 0.125 --> 8 sec per cycle


# -->
# Rolling and Rudder:
# spiked similar spectrum, power concentrated at around freq = 0.0625 (16 sec per cycle)
# and the bandwidth is narrow

# Rolling motion with a natural period is strong.


# -->
# Pitch:
# two peaks, different from rolling, bandwidth is broad, damping force is strong.
# 1 peak at around freq = 0.125 (8 sec per cycle) is same with YawRate

# Pitching motion with a natural period is weak,
# thus the pitch responds significantly to external disturbances such as wave forces.
# (-->  pitch is an index of wave height)





# ------------------------------------------------------------------------------
# Spectral analysis:  Autoregressive spectral estimator
# ------------------------------------------------------------------------------


graphics.off()

par(mfrow = c(2,2))

spec.ar(dat$Rudder, log = "no")

spec.ar(dat$YawRate, log = "no")

spec.ar(dat$Pitching, log = "no")

spec.ar(dat$Rolling, log = "no")





# ------------------------------------------------------------------------------
# Squared coherency
# ------------------------------------------------------------------------------

L <- 35

( m <- (L - 1) / 2 )


sr <- mvspec(dat[,c("Rudder","Rolling","Pitching","YawRate")], kernel("daniell", m), taper = 0.5, plot = FALSE)


# ----------
sr$df



# ----------
( f <- qf(.999, 2, sr$df -2) )

( C <- f / (L - 1 + f) )


par(mfrow = c(2,1))

plot(sr, plot.type = "coh", ci.lty = 2)

C



# -->
# Rolling:
# relation with Rudder at around freq = 0.1  - 0.16  (6 sec - 10 sec per cycle) is strong
# some relation with YawRate around freq = 0.2 - 0.3
# Ruder --> YawRate --> Rolling  ??


# Pitching:  large squared coherency with YawRate around freq = 0.5 - 1.5
# also with Rudder




# ------------------------------------------------------------------------------
# Impulse Response Function and Unit Response Function
#   - How much Rudder can contol Rolling
# ------------------------------------------------------------------------------

# L <- 101
L <- 3


M <- 100

mod_rudtoroll <- astsa::LagReg(input = dat$Rudder, output = dat$Rolling, L = L, M = M)



# -->
# Coefficients beta has some oscillating and decaying feature, but requires long lags



# Unit Step Response Function
par(mfrow = c(1,1))

plot(cumsum(mod_rudtoroll$betas[(M/2):(M-1),"b"]), type = "l")




# ----------
# Pitch

mod_rudtopitch <- astsa::LagReg(input = dat$Rudder, output = dat$Pitching, L = L, M = M)


# Unit Step Response Function
par(mfrow = c(1,1))

plot(cumsum(mod_rudtopitch$betas[(M/2):(M-1),"b"]), type = "l")


# -->
# fast to reach steady-state value




# ------------------------------------------------------------------------------
# Transfer Function Modeling
# ------------------------------------------------------------------------------

rud <- ts(dat$Rudder, start = 1, frequency = 1)

rol <- ts(dat$Rolling, start = 1, frequency = 1)



# ----------

acf2(rud, max.lag = 50)


armod <- arima(rud, order = c(4,0,0))

ar1 <- coef(armod)[1]

ar2 <- coef(armod)[2]

ar3 <- coef(armod)[3]

ar4 <- coef(armod)[4]

rud.pw <- resid(armod)


rol.tf <- stats::filter(rol, filter = c(1, -ar1, -ar2, -ar3, -ar4), sides = 1)





# ----------
L <- 3

M <- 100

astsa::LagReg(input = dat$Rudder, output = dat$Rolling, L = L, M = M)

mod_rudtoroll <- astsa::LagReg(input = rud.pw[5:1000], output = rol.tf[5:1000], L = L, M = M)




# ----------

ts_rr <- ts.intersect(Rolling = stats::lag(rol, 0),
                      rud = stats::lag(rud, 0),
                      rud1 = stats::lag(rud, -1),
                      rud2 = stats::lag(rud, -2),
                      rud3 = stats::lag(rud, -3),
                      rud4 = stats::lag(rud, -4),
                      rud5 = stats::lag(rud, -5),
                      rud6 = stats::lag(rud, -6),
                      rud7 = stats::lag(rud, -7))


u <- lm(Rolling ~ rud + rud1 + rud2 + rud3 + rud4 + rud5 + rud6 + rud7, data = ts_rr)

u <- lm(Rolling ~ rud6, data = ts_rr)

summary(u)



# -->
# Multiple R-squared is only 0.20



# ----------
acf2(resid(u))


mod_sar <- sarima(ts_rr[,1], p = 3, d = 0, q = 3, xreg = ts_rr[,c(8)])

mod_sar$ttable



# ----------
pred <- ts(rol, start = 1, frequency = 1)  - resid(mod_sar$fit)


graphics.off()

par(mfrow = c(1,1))


plot(ts(rol, start = 1, frequency = 1)[8:1000], type = "l", col = "darkgray")

lines(pred[1:993], col = "black", lwd = 1)



mean(abs(resid(mod_sar$fit)))


