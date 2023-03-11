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
# normalize data (again)
# ------------------------------------------------------------------------------

library(TSSS)


n <- length(dat)

yy <- rep(0, n)

for(i in 2:n) yy[i] <- dat[i] - 0.5 * dat[i-1]

y <- yy[seq(1, n, by = 2)]


z <- tvvar(y, trend.order = 2, tau2.ini = 4.909e-02, delta = 1.0e-06)



# ----------
par(mfrow = c(1,1))

plot(z$sm, type = "l")


z$tau2




# ------------------------------------------------------------------------------
# Particle Filter Smoothing:  Gaussian smoother
# ------------------------------------------------------------------------------

library(TSSS)



# ----------
# Kalman filter
k0 <- trend(z$sm, trend.order = 1, tau2.ini = z$tau2, delta = 0.0, plot = FALSE)



# ----------
# Particle Filter
# system noise:  Gaussian
# Observation noise:  Gaussian


# fixed lag = 20
# m = 1000

p1 <- pfilter(z$sm, m = 1000, model = 0, lag = 20, initd = 0, sigma2 = var(z$sm), tau2 = z$tau2, plot = FALSE)


p1$llkhood

str(p1$smooth.dist)





# ----------
# m = 10000

p1_2 <- pfilter(z$sm, m = 10000, model = 0, lag = 20, initd = 0, sigma2 = var(z$sm), tau2 = z$tau2, plot = FALSE)




# ----------
# m = 100000

p1_3 <- pfilter(z$sm, m = 100000, model = 0, lag = 20, initd = 0, sigma2 = var(z$sm), tau2 = z$tau2, plot = FALSE)





# ----------
# log-likelihood

k0$llkhood

p1$llkhood

p1_2$llkhood

p1_3$llkhood




# ----------
# mean and +- 1,2,3 sigma interval
graphics.off()

par(mfrow = c(2,2))

plot(k0$trend, type = "l", main = "Kalman Filter")

plot(p1)

plot(p1_2)

plot(p1_3)



# only mean
graphics.off()

par(mfrow = c(2,2))

plot(k0$trend, type = "l", main = "Kalman Filter")

plot(p1$smooth.dist[,3], type = "l", main = "Gaussian Particle Filter: m = 1000")

plot(p1_2$smooth.dist[,3], type = "l", main = "Gaussian Particle Filter: m = 10000")

plot(p1_3$smooth.dist[,3], type = "l", main = "Gaussian Particle Filter: m = 100000")




# ------------------------------------------------------------------------------
# Particle Filter Smoothing:  Non-Gaussian smoother
# ------------------------------------------------------------------------------


ngs <- ngsmth(z$sm, noisev = 2, bv = 1.0, noisew = 1, sigma2 = var(z$sm), tau2 = z$tau2, plot = FALSE)



# Particle Filter:  system noise:  Cauchy distribution

p2 <- pfilter(z$sm, m = 1000, model = 1, lag = 20, initd = 0, sigma2 = var(z$sm)/10, tau2 = z$tau2/10, plot = FALSE)

p2_2 <- pfilter(z$sm, m = 10000, model = 1, lag = 20, initd = 0, sigma2 = var(z$sm)/10, tau2 = z$tau2/10, plot = FALSE)

p2_3 <- pfilter(z$sm, m = 100000, model = 1, lag = 20, initd = 0, sigma2 = var(z$sm)/10, tau2 = z$tau2/10, plot = FALSE)




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

plot(p2)

plot(p2_2)

plot(p2_3)



# only mean
graphics.off()

par(mfrow = c(2,2))

plot(p2$smooth.dist[,3], type = "l", main = "Non-Gaussian Particle Filter: m = 1000")

plot(p2_2$smooth.dist[,3], type = "l", main = "Non-Gaussian Particle Filter: m = 10000")

plot(p2_3$smooth.dist[,3], type = "l", main = "Non-Gaussian Particle Filter: m = 100000")




