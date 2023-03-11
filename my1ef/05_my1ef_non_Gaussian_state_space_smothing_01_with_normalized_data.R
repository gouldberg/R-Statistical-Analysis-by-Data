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
# Trend estimation by ngsmth:  Fitting Gaussian model with sigma2 = pi * pi / 6
# ------------------------------------------------------------------------------

# noisev: type of system noise density --> 1 Gaussian (normal)  2 Type VII Pearson family  3 two-sided exponential
# noisew: type of observation noise density  --> 1 Gaussian (normal)  2 Type VII Pearson family  3 two-sided exponential  4 double exponential
# tau2:  variance of system noise disturbances
# sigma2: variance of observation disturbances
# k: number of grids for density estimation

# double exponential distrubition for the observation noise is approximated by Gausiaan distribution by sigma2 = pi * pi / 6

s1 <- ngsmth(z$sm, noisev = 1, tau2 = z$tau2, noisew = 1, sigma2 = pi * pi / 6, k = 190, plot = FALSE)


# -->
# log-likelihood = -1105.678



# ----------
graphics.off()

par(mfrow = c(1,1))

plot(s1, type = "trend")

plot(s1, type = "smt", theta = 30, phi = 55, expand = 0.6)



# -->
# show wiggly trend
# this is due to the large deviations to the negative side, which occurred due to the elog transformation




# ------------------------------------------------------------------------------
# Trend estimation by ngsmth:  other ditribution
# ------------------------------------------------------------------------------


# observation noise:  double exponential
# system noise:  Cauchy

# set tau2 to 1/10 of z$tau2
s2 <- ngsmth(z$sm, noisev = 2, tau2 = 0.00005, noisew = 4, sigma2 = 1.0, k = 190, plot = FALSE)


# -->
# log-likelihood = -1071.597  --> larger is better, big different from Gaussian model
# Non-Gausiaan model is much better




# ----------
graphics.off()

par(mfrow = c(1,1))

plot(s2, type = "trend")

# the point of changes
c(630/4, 1026/4)


plot(s2, type = "smt", theta = 30, phi = 55, expand = 0.6)



# -->
# clearly detected sudden increases in the variance due to the arrivals of the P-wave and the S-wave
# also show that the observations with a large downward deviation influenced only the curve of 0.13 percent points
# and not that of the 50 percent points


