rm(list=ls())

packages <- c("dplyr", "dlm", "KFAS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Nile
# ------------------------------------------------------------------------------


data(Nile)


dim(Nile)


str(Nile)




# ------------------------------------------------------------------------------
# data exploration:  time series plot
# ------------------------------------------------------------------------------


ts.plot(Nile)





# ------------------------------------------------------------------------------
# Particle Filter Smoothing:  Gaussian smoother
# ------------------------------------------------------------------------------


y <- Nile


library(TSSS)



# ----------
# Kalman filter
k0 <- trend(y, trend.order = 1, tau2.ini = var(y), delta = 0.0, plot = FALSE)



# ----------
# Particle Filter
# system noise:  Gaussian
# Observation noise:  Gaussian


# fixed lag = 20
# m = 1000

# smaller system noise (tau2)

p1 <- pfilter(y, m = 1000, model = 0, lag = 20, initd = 0, sigma2 = var(y), tau2 = var(y)/5, plot = FALSE)


str(p1$smooth.dist)




# ----------
# log-likelihood

k0$llkhood

p1$llkhood



# ----------
# mean and +- 1,2,3 sigma interval
graphics.off()

par(mfrow = c(2,2))

plot(k0$trend, type = "l", main = "Kalman Filter")
abline(v = c(25), col = "blue", lty = 2)

plot(p1)
abline(v = c(25), col = "blue", lty = 2)

plot(p1$smooth.dist[,4], type = "l", main = "Gaussian Particle Filter: m = 1000", ylim = c(500, 1400))
abline(v = c(25), col = "blue", lty = 2)





# ------------------------------------------------------------------------------
# Particle Filter Smoothing:  Non-Gaussian smoother
# ------------------------------------------------------------------------------


ngs <- ngsmth(y, noisev = 2, bv = 1.0, noisew = 1, sigma2 = var(y), tau2 = var(y), plot = FALSE)



# Particle Filter:  system noise:  Cauchy distribution

p2 <- pfilter(y, m = 1000, model = 1, lag = 20, initd = 0, sigma2 = var(y), tau2 = var(y)/10, plot = FALSE)




# ----------
# log-likelihood

# ngs$llkhood

p2$llkhood




# ----------
# mean and +- 1,2,3 sigma interval
graphics.off()

par(mfrow = c(2,2))

plot(k0$trend, type = "l", main = "Kalman Filter")
abline(v = c(25), col = "blue", lty = 2)

# plot(ngs, type = "trend")

plot(p2)
abline(v = c(25), col = "blue", lty = 2)

plot(p2$smooth.dist[,4], type = "l", main = "Non-Gaussian Particle Filter: m = 1000")
abline(v = c(25), col = "blue", lty = 2)





