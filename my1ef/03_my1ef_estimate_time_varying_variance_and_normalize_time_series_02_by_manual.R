setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\my1ef")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  MYE1F
# ------------------------------------------------------------------------------


dat <- read.table("MYE1F.txt", sep = "", header = T, colClasses = "numeric")


head(dat)


dat <- dat$x





# ------------------------------------------------------------------------------
# time series transformation   3/3
# ------------------------------------------------------------------------------


n <- length(dat)

yy <- rep(0, n)

for(i in 2:n) yy[i] <- dat[i] - 0.5 * dat[i-1]


seq2 <- seq(2, 2600, 2)

z <- log((yy[seq2]^2 + 0.1))




# ----------
graphics.off()

par(mfrow = c(2,1))

plot(dat, type = "l")

plot(z, type = "l")





# ------------------------------------------------------------------------------
# Fit Structural Time Series (by maximum likelihood) by KFAS
# local level model
# ------------------------------------------------------------------------------


library(KFAS)


# local level model:  r = 1,  q = 2

modLocallevel <- SSModel(z ~ SSMtrend(1, Q = NA), H = NA)

fitLocallevel <- fitSSM(modLocallevel, numeric(2))

kfsLocallevel <- KFS(fitLocallevel$model)




# ----------
# covariance matrix of disturbance of system
fitLocallevel$model$Q

# covariance matrix of disturbance of observation
fitLocallevel$model$H




# ------------------------------------------------------------------------------
# Fit Structural Time Series (by maximum likelihood) by KFAS
# trend model (2nd order trend model)
# ------------------------------------------------------------------------------

# 2nd order trend model:  r = 2,  q = 2

modTrend <- SSModel(z ~ SSMtrend(2, Q = c(list(0), list(NA))), H = NA)

fitTrend <- fitSSM(modTrend, numeric(2))

kfsTrend <- KFS(fitTrend$model)


fitTrend$model$Q

fitTrend$model$H


head(kfsTrend$alphahat)




# ------------------------------------------------------------------------------
# Fit Structural Time Series (by maximum likelihood) by KFAS
# local linear trend model
# ------------------------------------------------------------------------------

# local linear trend model:  r = 2,  q = 3

modlltrend <- SSModel(z ~ SSMtrend(2, Q = c(list(NA), list(NA))), H = NA)

fitlltrend <- fitSSM(modlltrend, numeric(3))

kfslltrend <- KFS(fitlltrend$model)


fitlltrend$model$Q

fitlltrend$model$H




# ------------------------------------------------------------------------------
# Model comparison
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow = c(3,2))

ts.plot(z, type = "l", ylab = "level", main = "transformed data")
abline(v = c(630, 1026), lty = 1, col = "darkgray")

ts.plot(kfsLocallevel$alphahat[,"level"], lwd = 2, col = "orange", ylab = "level", main = "local level model: level")
# ts.plot(kfsLocallevel$alphahat[,"slope"], lwd = 2, col = "orange", ylab = "slope")
abline(v = c(315, 513), lty = 1, col = "darkgray")

ts.plot(kfsTrend$alphahat[,"level"], lwd = 2, col = "blue", ylab = "level", main = "2nd order trend model: level")
abline(v = c(315, 513), lty = 1, col = "darkgray")
ts.plot(kfsTrend$alphahat[,"slope"], lwd = 2, col = "blue", ylab = "slope", main = "2nd order trend model: slope")
abline(v = c(315, 513), lty = 1, col = "darkgray")

ts.plot(kfslltrend$alphahat[,"level"], lwd = 2, col = "red", ylab = "level", main = "local linear trend model: level")
abline(v = c(315, 513), lty = 1, col = "darkgray")
ts.plot(kfslltrend$alphahat[,"slope"], lwd = 2, col = "red", ylab = "slope", main = "local linear trend model: slope")
abline(v = c(315, 513), lty = 1, col = "darkgray")



# ----------
# compare 2nd order Trend model vs. smooth.spline spar = 0.75

graphics.off()

par(mfrow = c(2,1))

ts.plot(z, type = "l")

ts.plot(kfsTrend$alphahat[,"level"], lwd = 2, col = "black")

lines(smooth.spline(time(z), z, spar = 0.75), lwd = 1, col = "blue", lty = 1)




# ----------
# maximum likelihood and AIC

likLocallevel <- kfsLocallevel$logLik - sum(kfsLocallevel$Finf > 0) * log(2 * pi) / 2

likTrend <- kfsTrend$logLik - sum(kfsTrend$Finf > 0) * log(2 * pi) / 2

liklltrend <- kfslltrend$logLik - sum(kfslltrend$Finf > 0) * log(2 * pi) / 2



# number of estimated parameters are r + q
-2 * likLocallevel + 2 * (2 + 1)

-2 * likTrend + 2 * (2 + 2)

-2 * liklltrend + 2 * (3 + 2)



# -->
# local level model is best




# ------------------------------------------------------------------------------
# Estimated time-varying sigma^2 and normalized original series
# ------------------------------------------------------------------------------


# this is the estimate of z --> log sigma^2 of original series
kfsTrend$alphahat[,"level"]


# estimated sigma^2  (time-varying sigma^2)
( est_sigma2 <- exp(kfsTrend$alphahat[,"level"]) )


head(est_sigma2)




# ----------
# normalized series (but time series are in half)
dat_norm2 <- 0.5 * (dat[seq1] + dat[seq2]) / sqrt(est_sigma2)

var(dat_norm2)




graphics.off()

par(mfcol = c(2,2))

plot(dat, type = "l", main = "original series")

ts.plot(z, type = "l", ylab = "level", ylim = c(-2, 8), main = "transformed data")

ts.plot(kfsTrend$alphahat[,"level"], lwd = 2, col = "blue", ylim = c(-2, 8), main = "estimated time-varying sigma^2")

# normalized series
plot(dat_norm2, type = "h", ylim = c(-6, 4), main = "normalized series: simga^2 close to 1")


