setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\my1ef")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  sample data
# ------------------------------------------------------------------------------


set.seed(123320)

# generate 4 time series with different mean
n1 <- rnorm(n = 100, mean = 0, sd = 1)

n2 <- rnorm(n = 100, mean = 1, sd = 1)

n3 <- rnorm(n = 100, mean = -1, sd = 1)

n4 <- rnorm(n = 100, mean = 0, sd = 1)



# ----------
# merge to one time series
y <- c(n1, n2, n3, n4)


par(mfrow = c(1,1))

plot(y, type = "l")



# ----------
var(y)




# ------------------------------------------------------------------------------
# Trend estimation by ngsmth
# ------------------------------------------------------------------------------

# noisev: type of system noise density --> 1 Gaussian (normal)  2 Type VII Pearson family  3 two-sided exponential
# noisew: type of observation noise density  --> 1 Gaussian (normal)  2 Type VII Pearson family  3 two-sided exponential  4 double exponential
# tau2:  variance of system noise disturbances
# sigma2: variance of observation disturbances
# k: number of grids for density estimation


# system noise density:  Gaussian (normal)

s1 <- ngsmth(y, noisev = 1, tau2 = var(y) / 100, noisew = 1, sigma2 = var(y), k = 100, plot = FALSE)


str(s1)



# -->
# log-likelihood = -615.931


graphics.off()

par(mfrow = c(1,2))

plot(s1, type = "smt", theta = 30, phi = 55, expand = 0.6, col = 8)

plot(s1, type = "trend")



# -->
# showing mean and +-1,2,3 standard deviation intervals of the estimated distribution




# ----------
# system noise density:  Type VII Pearson family (Student t Distribution) with b = 0.6
# b --> inf, normal distribution

# Pearson family of distributions can express variout symmetric probability density functions
# with heavier-tailed distribution


# bv: shape parameter of the system noise

# we try various bv but bv = 0.6 is best
# tau2 should be set very small

s2 <- ngsmth(y, noisev = 2, tau2 = var(y) / 1e+10, bv = 0.6, noisew = 1, sigma2 = var(y), k = 400, plot = FALSE)

# log-likelihood = -612.047


graphics.off()

par(mfrow = c(1,2))

plot(s2, type = "smt", theta = 30, phi = 55, expand = 0.6, col = 8)

plot(s2, type = "trend")


# -->
# showing 0.13, 2.27, 15.87, 50.0, 84.13, 97.73 and 99.87
# percential points of the estimated trend distribution
# that correspond to the man and +- 1,2,3 intervals of the Gaussian distribution


