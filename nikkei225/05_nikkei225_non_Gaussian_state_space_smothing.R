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
# log-likelihood = -719.308



# ----------
graphics.off()

par(mfrow = c(1,1))

plot(s1, type = "trend")

plot(s1, type = "smt", theta = 30, phi = 55, expand = 0.6)






# ------------------------------------------------------------------------------
# Trend estimation by ngsmth:  other ditribution
# ------------------------------------------------------------------------------


# observation noise:  double exponential
# system noise:  Cauchy

s2 <- ngsmth(z$sm, noisev = 2, tau2 = z$tau2, noisew = 4, sigma2 = 1.0, k = 190, plot = FALSE)
# s2 <- ngsmth(z$sm, noisev = 2, tau2 = 6.9e-2, noisew = 4, sigma2 = 1.0, k = 190, plot = FALSE)


# -->
# log-likelihood = -656.254
# --> larger is better, big different from Gaussian model
# Non-Gausiaan model is much better




# ----------
graphics.off()

outliers <- c(527, 680, 1052, 1204, 1277, 1411)
outliers_t <- outliers / 4


par(mfrow = c(1,1))

plot(s2, type = "trend")

plot(s2, type = "smt", theta = 30, phi = 55, expand = 0.6)





# ----------
s3 <- s2$trend

# time varying variance

s4 <- exp(s3[,2] / 2)

graphics.off()

par(mfrow = c(3,1))

plot(dat, type = "l", main = "nikkei 225  log + dif")
abline(v = outliers, col = "blue", lty = 2)

plot(z$sm, type = "l", main = "transformed nikkei 225")
abline(v = outliers_t, col = "blue", lty = 2)

plot(s4, type = "l", main = "Time-Varying variance")
abline(v = outliers_t, col = "blue", lty = 2)


