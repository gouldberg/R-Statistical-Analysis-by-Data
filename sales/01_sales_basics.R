setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\00_basics\\10_time_series_basics\\sales")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  sales
# ------------------------------------------------------------------------------


dat <- read.csv("sales.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)


str(dat)


car::some(dat)



# ----------
colnames(dat) <- c("month", "fabrics", "machinery", "fuel")




# ------------------------------------------------------------------------------
# data exploration:  plot time series
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow = c(1,1))

MTS::MTSplot(dat[,2:4])




# ------------------------------------------------------------------------------
# data exploration:  smoothing  (spline smoothing)
# ------------------------------------------------------------------------------


ts.plot(dat$fabrics, type = "b")

lines(smooth.spline(dat$fabrics, spar = 0.25), lwd = 2, col = "darkgray")

lines(smooth.spline(dat$fabrics, spar = 0.5), lwd = 2, col = "orange")

lines(smooth.spline(dat$fabrics, spar = 0.75), lwd = 2, col = "red")

lines(smooth.spline(dat$fabrics, spar = 1), lwd = 2, col = "blue")




# ------------------------------------------------------------------------------
# convert to ts object and decompose
# ------------------------------------------------------------------------------


( fab2 <- ts(dat$fabrics, start = 1, frequency = 12) )


# ----------
# decompose
dec <- decompose(fab2, type = "additive")


plot(dec)


# -->
# local level model (trend) + seasonality



# ----------
# note that first and last 6 months are not estimated
dec$trend




# ------------------------------------------------------------------------------
# Check 1st order difference
# ------------------------------------------------------------------------------


forecast::ndiffs(dat$fabrics)


# -->
# this is zero, not required to be 1st difference ...


ts.plot(diff(dat$fabrics))




# ------------------------------------------------------------------------------
# Unit Root Test:  Augmented Dickey-Fuller Test
# ------------------------------------------------------------------------------

library(urca)


# ----------
# random walk + trend
# diff(y(t)) = beta1 + beta2 * t + pi * y(t-1) + sum{ rho(j) * diff(y(t-j)) } + alpha(t),  j = 1,2,3,4,5

summary(ur.df(dat$fabrics, type = "trend", lags = 5))


# -->
# Null hypothesis:  pi = (phi - 1) = 0  (if pi = 0 --> phi = 1 --> unit root)
# t test statistics = -3.3274 --> critical value of 5pct = -3.43 --> marginally not rejected

# Null hypothesis:  pi = (phi - 1) = 0  (if pi = 0 --> phi = 1 --> unit root) AND beta2 = 0
# F test statistics = 5.5406  --> critical value 5pct = 6.49 --> marginally not rejected




# ----------
# random walk + drift
# diff(y(t)) = beta1 + pi * y(t-1) + sum{ rho(j) * diff(y(t-j)) } + alpha(t),  j = 1,2,3,4,5

summary(ur.df(dat$fabrics, type = "drift", lags = 5))


# -->
# Null hypothesis:  pi = (phi - 1) = 0  (if pi = 0 --> phi = 1 --> unit root)
# t test statistics = -3.3416 --> critical value of 5pct = -2.88 --> rejected

# Null hypothesis:  pi = (phi - 1) = 0  (if pi = 0 --> phi = 1 --> unit root) AND beta1 = 0
# F test statistics = 5.6259  --> critical value 5pct = 4.63 --> rejected




# ----------
# random walk
# diff(y(t)) = pi * y(t-1) + sum{ rho(j) * diff(y(t-j)) } + alpha(t),  j = 1,2,3,4,5

summary(ur.df(dat$fabrics, type = "none", lags = 5))


# -->
# Null hypothesis:  pi = (phi - 1) = 0  (if pi = 0 --> phi = 1 --> unit root)
# t test statistics = 0.1592 --> critical value of 5pct = -1.95 --> not rejected




# -->
# fabrics may be random walk (local level model) or random walk + drift




# ------------------------------------------------------------------------------
# sample auto-correlation and partial auto-correlation (Correlogram)
# ------------------------------------------------------------------------------


astsa::acf2(dat$fabrics, max.lag = 40)


astsa::acf2(diff(dat$fabrics), max.lag = 40)



# -->
# MA part has strong seasonality



# ------------------------------------------------------------------------------
# raw periodogram
# ------------------------------------------------------------------------------


spc <- astsa::mvspec(dat$fabrics)



# -->
# strong peaks at 5 frequencies



spc$details


nextn(length(dat$fabrics)) * 0.0833




