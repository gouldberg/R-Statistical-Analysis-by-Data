setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\hakusan")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Hakusan
# ------------------------------------------------------------------------------


dat <- read.table("HAKUSAN.txt", sep = "", header = T, colClasses = "numeric")


head(dat)



# ----------
# demean

dat$YawRate <- dat$YawRate - mean(dat$YawRate)

dat$Rolling <- dat$Rolling - mean(dat$Rolling)

dat$Pitching <- dat$Pitching - mean(dat$Pitching)

dat$Rudder <- dat$Rudder - mean(dat$Rudder)





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
z <- marfit(as.matrix(dat), lag = 20)



names(z)


z$aic



# AR(10) is AIC minimum
z$maice.order


z$arcoef


# covariance matrix
round(z$v, 4)


# correlation matrix
D <- diag(sqrt(diag(z$v)))
solve(D) %*% z$v %*% solve(D)



# -->
# YawRate and Pitching has some correlation (= 0.25)




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
# cumulative:  from bottom, YawRate --> Rolling --> Pitching --> Rudder


# YawRate:  some contribution by Rudder at lower frequency, but almost no contribution at around major freq = 0.125
# This shows that this autopilot system is designed to reduce occilation at around freq = 0.625

# Rolling:  similary, Rudder control is designed to reduce occilation at around freq = 0.625  (16 sec per cycle)

# Rudder:  contribution of YawRate is strong at freq = 0.125
# and also at low frequency < 0.8, YawRate has large contribution




# ------------------------------------------------------------------------------
# Power contribution by timsac (this is better by different coloring)
# ------------------------------------------------------------------------------


library(timsac)

# h  frequencies
muln <- mulnos(as.matrix(dat), max.order = 10, h = 500)


muln$integr




# ----------
graphics.off()

par(mfrow = c(2,2))

matplot(t(muln$integr[1,,]), type = "l")

matplot(t(muln$integr[2,,]), type = "l")

matplot(t(muln$integr[3,,]), type = "l")

matplot(t(muln$integr[4,,]), type = "l")





