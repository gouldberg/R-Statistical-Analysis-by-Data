setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\econ5")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)




# ------------------------------------------------------------------------------
# data:  econ5
# ------------------------------------------------------------------------------


econ5 <- read.table("econ5.txt", header = T, sep = "\t")


str(econ5)


car::some(econ5)



gr <- diff(log(ts(econ5, start = 1948, frequency = 4)))


# gr <- scale(gr)




# ------------------------------------------------------------------------------
# Cross spectra and power contribution by multivariate AR model
# ------------------------------------------------------------------------------

library(TSSS)



# Yule-Walker Method of Fitting Multivariate AR Model Fitting
z <- marfit(as.matrix(gr), lag = 20)



names(z)


z$aic



# AR(4) is AIC minimum
z$maice.order


z$arcoef


# covariance matrix
round(z$v, 4)


# correlation matrix
D <- diag(sqrt(diag(z$v)))
solve(D) %*% z$v %*% solve(D)



# -->
# highly correlated data ...




# ----------
# Cross spectra and power contribution
graphics.off()


# v:  innovation variance matrix
marspc(z$arcoef, v = z$v)





# ------------------------------------------------------------------------------
# Power contribution by timsac (this is better by different coloring)
# ------------------------------------------------------------------------------


library(timsac)

# h  frequencies
muln <- mulnos(as.matrix(gr), max.order = 10, h = 500)


muln$integr




# ----------
graphics.off()

par(mfrow = c(2,3))

matplot(t(muln$integr[1,,]), type = "l", lty = c(5,4,3,2,1), col = c(5,4,3,2,1))

matplot(t(muln$integr[2,,]), type = "l", lty = c(5,4,3,1,2), col = c(5,4,3,2,1))

matplot(t(muln$integr[3,,]), type = "l", lty = c(5,4,1,2,3), col = c(5,4,3,2,1))

matplot(t(muln$integr[4,,]), type = "l", lty = c(5,1,3,2,4), col = c(5,4,3,2,1))

matplot(t(muln$integr[5,,]), type = "l", lty = c(1,4,3,2,5), col = c(5,4,3,2,1))




