setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\nikkei225")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  nikkei225
# ------------------------------------------------------------------------------


dat <- read.table("nikkei225.txt", sep = "", header = T, colClasses = "numeric")


head(dat)


dat <- dat$nikkei225


dat <- diff(log(dat))




# ------------------------------------------------------------------------------
# normalize data (again)
# ------------------------------------------------------------------------------

library(TSSS)


# pre-process
n <- length(dat)
yy <- rep(0, n)
for(i in 2:n) yy[i] <- dat[i] - 0.5 * dat[i-1]
y <- yy[seq(1, n, by = 2)]


z <- tvvar(y, trend.order = 2)



# ----------
par(mfrow = c(1,1))

plot(z$sm, type = "l")


z$tau2




# ------------------------------------------------------------------------------
# Particle Filter Smoothing:  Gaussian smoother  --> does not estimate properly ....
# ------------------------------------------------------------------------------

library(TSSS)


# !!! note that tau2 (system noize) requires large values


# ----------
# Kalman filter
k0 <- trend(z$sm, trend.order = 1, tau2.ini = z$tau2 * 1000, delta = 0.0, plot = FALSE)



# ----------
# Particle Filter
# system noise:  Gaussian
# Observation noise:  Gaussian


# fixed lag = 20
# m = 1000

p1 <- pfilter(z$sm, m = 1000, model = 0, lag = 20, initd = 0, sigma2 = var(z$sm), tau2 = z$tau2 * 1000, plot = FALSE)


p1$llkhood

str(p1$smooth.dist)





# ----------
# m = 10000

p1_2 <- pfilter(z$sm, m = 10000, model = 0, lag = 20, initd = 0, sigma2 = var(z$sm), tau2 = z$tau2 * 1000, plot = FALSE)




# ----------
# m = 100000

p1_3 <- pfilter(z$sm, m = 100000, model = 0, lag = 20, initd = 0, sigma2 = var(z$sm), tau2 = z$tau2 * 1000, plot = FALSE)





# ----------
# log-likelihood

k0$llkhood

p1$llkhood

p1_2$llkhood

p1_3$llkhood




# ----------
# mean and +- 1,2,3 sigma interval
graphics.off()

outliers <- c(527, 680, 1052, 1204, 1277, 1411)
outliers_t <- outliers / 4

par(mfrow = c(2,2))

plot(k0$trend, type = "l", main = "Kalman Filter")
abline(v = outliers_t, col = "blue", lty = 2)

plot(p1)
abline(v = outliers_t, col = "blue", lty = 2)

plot(p1_2)
abline(v = outliers_t, col = "blue", lty = 2)

plot(p1_3)
abline(v = outliers_t, col = "blue", lty = 2)



# only mean
graphics.off()

par(mfrow = c(2,2))

plot(k0$trend, type = "l", main = "Kalman Filter")
abline(v = outliers_t, col = "blue", lty = 2)

plot(p1$smooth.dist[,4], type = "l", main = "Gaussian Particle Filter: m = 1000")
abline(v = outliers_t, col = "blue", lty = 2)

plot(p1_2$smooth.dist[,4], type = "l", main = "Gaussian Particle Filter: m = 10000")
abline(v = outliers_t, col = "blue", lty = 2)

plot(p1_3$smooth.dist[,4], type = "l", main = "Gaussian Particle Filter: m = 100000")
abline(v = outliers_t, col = "blue", lty = 2)




# ------------------------------------------------------------------------------
# Particle Filter Smoothing:  Non-Gaussian smoother
# ------------------------------------------------------------------------------


ngs <- ngsmth(z$sm, noisev = 2, bv = 1.0, noisew = 1, sigma2 = var(z$sm), tau2 = z$tau2, plot = FALSE)



# Particle Filter:  system noise:  Cauchy distribution

p2 <- pfilter(z$sm, m = 1000, model = 1, lag = 20, initd = 0, sigma2 = var(z$sm), tau2 = z$tau2, plot = FALSE)

p2_2 <- pfilter(z$sm, m = 10000, model = 1, lag = 20, initd = 0, sigma2 = var(z$sm), tau2 = z$tau2, plot = FALSE)

p2_3 <- pfilter(z$sm, m = 100000, model = 1, lag = 20, initd = 0, sigma2 = var(z$sm), tau2 = z$tau2, plot = FALSE)




# ----------
# log-likelihood

# ngs$llkhood

p2$llkhood

p2_2$llkhood

p2_3$llkhood




# ----------
# mean and +- 1,2,3 sigma interval
graphics.off()

par(mfrow = c(2,2))

plot(ngs, type = "trend")
abline(v = outliers_t, col = "blue", lty = 2)

plot(p2)
abline(v = outliers_t, col = "blue", lty = 2)

plot(p2_2)
abline(v = outliers_t, col = "blue", lty = 2)

plot(p2_3)
abline(v = outliers_t, col = "blue", lty = 2)



# only mean
graphics.off()

par(mfrow = c(2,2))

plot(p2$smooth.dist[,4], type = "l", main = "Non-Gaussian Particle Filter: m = 1000")
abline(v = outliers_t, col = "blue", lty = 2)

plot(p2_2$smooth.dist[,4], type = "l", main = "Non-Gaussian Particle Filter: m = 10000")
abline(v = outliers_t, col = "blue", lty = 2)

plot(p2_3$smooth.dist[,4], type = "l", main = "Non-Gaussian Particle Filter: m = 100000")
abline(v = outliers_t, col = "blue", lty = 2)




