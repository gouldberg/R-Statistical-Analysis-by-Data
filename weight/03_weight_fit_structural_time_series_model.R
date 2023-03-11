setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\10_time_series_basics\\weight")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  weight
# ------------------------------------------------------------------------------


dat <- read.table("Weight.dat", header = FALSE)


str(dat)


car::some(dat)



# ----------
colnames(dat) <- c("weight")




# ------------------------------------------------------------------------------
# Fit Structural Time Series (by maximum likelihood)
# Local Level Model by StructTS
# ------------------------------------------------------------------------------

# local level model

( strllm <- StructTS(dat$weight, type = "level") )


# Kalman Filter used in fittting
strllm$model



# ----------
# S/N ratio = 0.475
strllm$model$V

strllm$model$h

strllm$model$V / strllm$model$h



# ----------
head(strllm$fitted)


# fitted values
head(fitted(strllm))

head(strllm$fitted[,"level"])



# ----------
graphics.off()

par(mfrow = c(1,1))

ts.plot(dat$weight, type = "b", ylim = c(82, 88))

lines(dat$weight - resid(arima111$fit), lwd = 1, col = "orange")

lines(dat$weight - resid(arima011$fit), lwd = 1, col = "blue")

lines(fitted(strllm), lwd = 2, col = "blue")

# filtered value
lines(strllm$fitted[,"level"], lwd = 2, col = "darkgreen")





# ------------------------------------------------------------------------------
# Fit Structural Time Series (by maximum likelihood)
# Local Level Model by KFAS
# ------------------------------------------------------------------------------


library(KFAS)


# local level model:  estimate Q and H (by maximum likelihood)
# Q:  variance for system equation
# H:  variance for observation equation

modLocallevel <- SSModel(dat$weight ~ SSMtrend(1, Q = NA), H = NA)


# initial value = numeric(2) = c(0, 0)
fit <- fitSSM(modLocallevel, numeric(2))



# ----------
fit$model$Q

fit$model$H


# S/N ratio = 0.475
fit$model$Q / fit$model$H



# ----------
# Kalman fitering and smoothing

kfs <- KFS(fit$model)




# ----------
names(kfs)


# filtered estimates of state
kfs$att


# Non-diffuse parts of the error covariance matrix of predicted states
kfs$P


# error variance at time point
( Pfilt <- kfs$P[,,-1] - fit$model$Q )


# smoothed estimate of state
kfs$alphahat



# error covariance matrices of smoothed states
kfs$V[,,1]



# One-step-ahead predictions of states
kfs$a


# this is local level model, one-step-ahead prediction = filtered estimates of previous time point
head(kfs$a[-1])
head(c(kfs$att))




# ----------
# confidence interval (95%) of filtered value
( afiltconf <- cbind(kfs$att + sqrt(Pfilt) * qnorm(0.025), kfs$att + sqrt(Pfilt) * qnorm(0.975)) )


# confidence interval (95%) of smoothed value
( aamconf <- cbind(kfs$alphahat + sqrt(c(kfs$V)) * qnorm(0.475), kfs$alphahat + sqrt(c(kfs$V)) * qnorm(0.975)) )



# ----------
# plot filtered value:  same with StructTS estimation
graphics.off()

par(mfrow = c(2,1))

ts.plot(dat$weight, type = "b", ylim = c(82, 88))

lines(strllm$fitted[,"level"], lwd = 2, col = "darkgreen")

ts.plot(dat$weight, type = "b", ylim = c(82, 88))

lines(kfs$att, lwd = 2, col = "blue")




# ----------
# plot filtered value and its confidence interval
graphics.off()

par(mfrow = c(1,1))

ts.plot(dat$weight, type = "b", ylim = c(82, 88))

lines(kfs$att, lwd = 2, col = "blue")
lines(afiltconf[,1], lwd = 1, lty = 2, col = "blue")
lines(afiltconf[,2], lwd = 1, lty = 2, col = "blue")




# ------------------------------------------------------------------------------
# Fit Structural Time Series (by maximum likelihood)
# by KFAS
# ------------------------------------------------------------------------------


# local level model:  r = 1,  q = 2

modLocallevel <- SSModel(dat$weight ~ SSMtrend(1, Q = NA), H = NA)

fitLocallevel <- fitSSM(modLocallevel, numeric(2))

kfsLocallevel <- KFS(fitLocallevel$model)


fitLocallevel$model$Q

fitLocallevel$model$H



# ----------
# 2nd order trend model:  r = 2,  q = 2

modTrend <- SSModel(dat$weight ~ SSMtrend(2, Q = c(list(0), list(NA))), H = NA)

fitTrend <- fitSSM(modTrend, numeric(2))

kfsTrend <- KFS(fitTrend$model)


fitTrend$model$Q

fitTrend$model$H


head(kfsTrend$alphahat)




# ----------
# local linear trend model:  r = 2,  q = 3

modLocaltrend <- SSModel(dat$weight ~ SSMtrend(2, Q = c(list(NA), list(NA))), H = NA)


# local linear trend model but with H = 0.14, Q = 0.076
# modLocaltrend <- SSModel(dat$weight ~ SSMtrend(2, Q = c(list(0.076), list(NA))), H = 0.14)


fitLocaltrend <- fitSSM(modLocaltrend, numeric(3))

kfsLocaltrend <- KFS(fitLocaltrend$model)


fitLocaltrend$model$Q

# estimated as almost zero --> almost local level model
fitLocaltrend$model$H




# ----------
# orange:  Local Level Model
# blue:  2nd order Trend model
# black:  Local Linear Trend model


graphics.off()

par(mfrow = c(2,1))

ts.plot(dat$weight, lty = 3, type = "o", ylab = "level")

lines(kfsLocallevel$alphahat[,"level"], lwd = 2, lty = 2, col = "orange")

lines(kfsTrend$alphahat[,"level"], lwd = 2, col = "blue")

# almost overlapped with original time series
lines(kfsLocaltrend$alphahat[,"level"], lwd = 2)


ts.plot(kfsTrend$alphahat[,"slope"], lwd = 2, col = "blue", ylab = "slope")

lines(kfsLocaltrend$alphahat[,"slope"], lwd = 2)



# ----------
# compare 2nd order Trend model vs. smooth.spline spar = 0.75

graphics.off()

par(mfrow = c(1,1))

ts.plot(dat$weight, type = "b")

lines(kfsTrend$alphahat[,"level"], lwd = 2, col = "blue")

lines(smooth.spline(dat$weight, spar = 0.75), lwd = 2, col = "blue", lty = 2)




# ----------
# maximum likelihood and AIC

likLocallevel <- kfsLocallevel$logLik - sum(kfsLocallevel$Finf > 0) * log(2 * pi) / 2

likTrend <- kfsTrend$logLik - sum(kfsTrend$Finf > 0) * log(2 * pi) / 2

likLocaltrend <- kfsLocaltrend$logLik - sum(kfsLocaltrend$Finf > 0) * log(2 * pi) / 2



# number of estimated parameters are r + q
-2 * likLocallevel + 2 * (2 + 1)

-2 * likTrend + 2 * (2 + 2)

-2 * likLocaltrend + 2 * (3 + 2)



# -->
# local level model is best




# ------------------------------------------------------------------------------
# Fit Structural Time Series (by maximum likelihood)
# Local Linear Trend model by StructTS
# ------------------------------------------------------------------------------

# Local Linear Trend model

( strtrend <- StructTS(dat$weight, type = "trend") )



# ----------
strtrend$model$V

# estimate is 0.14  (not zero)
strtrend$model$h

# S/N ratio = 0.5088
strtrend$model$V / strllm$model$h



# ----------
head(strtrend$fitted)


# fitted values
head(fitted(strtrend))




# ----------
# orange:  Local Level Model
# blue:  2nd order Trend model
# black:  Local Linear Trend model
# darkgreen:  Local Linear Trend model by StrucTS


graphics.off()

par(mfrow = c(1,1))

ts.plot(kfsTrend$alphahat[,"slope"], lwd = 2, col = "blue", ylab = "slope")

lines(strtrend$fitted[,"slope"], lwd = 2, col = "darkgreen")

# not smoothed slope
lines(kfsLocaltrend$alphahat[,"slope"], lwd = 2)




# ----------
graphics.off()

par(mfrow = c(1,1))

ts.plot(dat$weight, type = "b", ylim = c(83, 87))

lines(kfsLocallevel$alphahat[,"level"], lwd = 2, lty = 2, col = "orange")

lines(kfsTrend$alphahat[,"level"], lwd = 2, col = "blue")

# almost overlapped with original time series
lines(kfsLocaltrend$alphahat[,"level"], lwd = 2)

lines(strtrend$fitted[,"level"], lwd = 2, col = "darkgreen")



