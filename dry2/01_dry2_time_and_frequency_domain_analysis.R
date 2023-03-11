setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\dry2")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  dry2
#   - Input:  Voltage (Volt)
#   - Output:  Temperature
#   - sampling cycle = 0.08 seconds
# ------------------------------------------------------------------------------


u <- read.csv("dry2_u.txt", sep = "", header = F, colClasses = "numeric")

y <- read.csv("dry2_y.txt", sep = "", header = F, colClasses = "numeric")


dry2 <- cbind(u, y)


colnames(dry2) <- c("input", "output")


head(dry2)



# ----------
str(dry2)




# ------------------------------------------------------------------------------
# data exploration:  time series plot
# ------------------------------------------------------------------------------


MTSplot(dry2)




# ----------
par(mfrow = c(1,1))

plot(dry2$output, type = "l")

lines(smooth.spline(time(dry2$output), dry2$output, spar = 0.5), col = "blue", lwd = 2)

lines(smooth.spline(time(dry2$output), dry2$output, spar = 1), col = "red", lwd = 2)




# ------------------------------------------------------------------------------
# auto-correlation and partial auto-correlation
# ------------------------------------------------------------------------------


apply(dry2, 2, forecast::ndiffs)




# ----------
# AR(3) ?
acf2(dry2$output, max.lag = 100)


sarima(dry2$output, p = 3, d = 0, q = 0)



# ----------
# AR(1) ?
acf2(dry2$input, max.lag = 100)




# ------------------------------------------------------------------------------
# cross-correlation
# ------------------------------------------------------------------------------


ccf(dry2$output, dry2$input)



# ------------------------------------------------------------------------------
# Periodogram
# ------------------------------------------------------------------------------


per <- astsa::mvspec(dry2$output, log = "no", type = "h")


# max spectrum at freq = 0.014 (0.014 cycle per observation)
per$freq[which(per$spec == max(per$spec))]


# 1 cycle per 71.4 points
1 / per$freq[which(per$spec == max(per$spec))]



# ----------
per <- astsa::mvspec(dry2$output, spans = c(7,7), taper = 0.25, log = "no", type = "l")


# max spectrum at freq = 0.022 (0.022 cycle per observation)
per$freq[which(per$spec == max(per$spec))]


# 1 cycle per 45.5 points
1 / per$freq[which(per$spec == max(per$spec))]



# ----------
# parametric spectral estimation --> AR(3)

spec.ar(dry2$output, method = "burg")




# ----------
# input

astsa::mvspec(dry2$input2, log = "no", type = "h")


spec.ar(dry2$input2, method = "burg")




# ------------------------------------------------------------------------------
# squared coherency
# ------------------------------------------------------------------------------


# sr <- astsa::mvspec(dry2[,c("input", "output")], spans = c(7,7), taper = 0.1, plot = "FALSE")


L <- 19
( m <- (L - 1) / 2 )
sr <- astsa::mvspec(dry2[,c("input", "output")], kernel("daniell", m), plot = "FALSE")



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


# if sampling 1 point in 2 seconds
# yy <- as.matrix(HAKUSAN[, c(1,2,4)])
# nc <- dim(yy)[1]
# n <- seq(1, nc, by = 2)
# y <- yy[n, ]



library(TSSS)



# Yule-Walker Method of Fitting Multivariate AR Model Fitting
z <- marfit(as.matrix(dry2[,c("input","output")]), lag = 20)



names(z)


z$aic



# AR(5) is AIC minimum
z$maice.order


z$arcoef


# covariance matrix
round(z$v, 4)


# correlation matrix
diag(z$v)^(-0.5) * z$v * diag(z$v)^(-0.5)




# ----------
# Cross spectra and power contribution

graphics.off()


# v:  innovation variance matrix
marspc(z$arcoef, v = z$v)




# -->
# 1st page:  spectrum, amplitude and phase
# note that phase is discontinuous due to its representation defined in [-pi < phi < phi]


# 2nd page:  spectrum and coherency

# 3rd page:  noise contribution (cumulative absolute, cumulative relative ratio)




# ----------
library(timsac)

# h  frequencies
muln <- mulnos(as.matrix(dry2[,c("input", "output")]), max.order = 10, h = 500)


muln$integr


# ----------
graphics.off()

par(mfrow = c(2,2))

matplot(t(muln$integr[1,,]), type = "l", ylim = c(0, 1))

matplot(t(muln$integr[2,,]), type = "l", ylim = c(0, 1))




# ------------------------------------------------------------------------------
# Regression with lagged variables:  impulse response function
# ------------------------------------------------------------------------------


# detrending
output2 <- resid(lm(output ~ time(output), data = dry2))


# demean
input2 <- dry2$input - mean(dry2$input)
dry2$input2 <- dry2$input - mean(dry2$input)



# L:  degree of smoothing
# M:  must be even, number of terms used in the lagged regression, abs(t) >= M/2
# threshold:  the cut-off used to set small (in absolute value) regression coefficients equal to zero


mod_l <- astsa::LagReg(input = input2, output = output2, L = 15, M = 40, threshold = 0.005)



# -->
# estimate of delay is 3




# ----------
mod_i <- astsa::LagReg(input = output2, output = input2, L = 15, M = 40, inverse = TRUE,  threshold = 0.1)




# ------------------------------------------------------------------------------
# Lagged regression
# ------------------------------------------------------------------------------


dry2_t <- ts.intersect(output = stats::lag(output, 0),
                       i3 = stats::lag(input2, -3),
                       i4 = stats::lag(input2, -4),
                       i5 = stats::lag(input2, -5),
                       i6 = stats::lag(input2, -6),
                       i7 = stats::lag(input2, -7),
                       i8 = stats::lag(input2, -8),
                       i9 = stats::lag(input2, -9),
                       i10 = stats::lag(input2, -10),
                       i11 = stats::lag(input2, -11),
                       i12 = stats::lag(input2, -12))


( u6 <- lm(output ~ i3 + i4 + i5 + i6, data = dry2_t, na.action = NULL) )

( u7 <- lm(output ~ i3 + i4 + i5 + i6 + i7, data = dry2_t, na.action = NULL) )

( u8 <- lm(output ~ i3 + i4 + i5 + i6 + i7 + i8, data = dry2_t, na.action = NULL) )

( u9 <- lm(output ~ i3 + i4 + i5 + i6 + i7 + i8 + i9, data = dry2_t, na.action = NULL) )

( u12 <- lm(output ~ i3 + i4 + i5 + i6 + i7 + i8 + i9 + i10 + i11 + i12, data = dry2_t, na.action = NULL) )


summary(u6)

summary(u7)

summary(u8)

summary(u9)

summary(u12)



# ----------
# the model can be simplified (u2)
anova(u6, u7)

anova(u7, u8)

anova(u8, u9)

anova(u9, u12)


AIC(u6, u7, u8, u9, u12)


# -->
# best is u12



# ----------

acf2(resid(u6))


acf2(resid(u12))



# ----------

arx6 <- sarima(dry2_t[,1], 1, 0, 4, xreg = dry2_t[,c(2:5)])

arx6$ttable


arx12 <- sarima(dry2_t[,1], 1, 0, 3, xreg = dry2_t[,c(2:11)])

arx12$ttable


arx6$AIC

arx12$AIC




# ----------

pred <- output[-c(1:12)] - resid(arx$fit)

par(mfrow = c(1,1))

ts.plot(pred, output[-c(1:12)], col = c('black', "gray"), lwd = c(2,2))


