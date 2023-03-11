setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\unemp")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  unemp
# ------------------------------------------------------------------------------

unemp <- read.table("unemp.txt", sep = "\t", header = T, stringsAsFactors = F)


unemp <- ts(unemp, start = 1948, end = 1978, frequency = 12)




# ------------------------------------------------------------------------------
# data exploration:  time series plot
# ------------------------------------------------------------------------------


MTSplot(unemp)



# ------------------------------------------------------------------------------
# data exploration:  decompose
# ------------------------------------------------------------------------------


plot(decompose(unemp, type = "additive"))



# ------------------------------------------------------------------------------
# data exploration:  ndiff
# ------------------------------------------------------------------------------


forecast::ndiffs(unemp)


plot(diff(unemp))


# there are increasing trend
mean(diff(unemp))



# ------------------------------------------------------------------------------
# data exploration:  smoothing to show trend
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow = c(1,1))

plot(unemp)

lines(smooth.spline(unemp, spar = 0.5), col = "red", lwd = 2)

lines(smooth.spline(unemp, spar = 1), col = "blue", lwd = 2)




# ------------------------------------------------------------------------------
# Unit Root Test:  Augmented Dickey-Fuller Test
# ------------------------------------------------------------------------------

library(urca)


# ----------
# random walk + trend
# diff(y(t)) = beta1 + beta2 * t + pi * y(t-1) + sum{ rho(j) * diff(y(t-j)) } + alpha(t),  j = 1,2,3,4,5

summary(ur.df(unemp, type = "trend", lags = 14))


# -->
# Null hypothesis:  pi = (phi - 1) = 0  (if pi = 0 --> phi = 1 --> unit root)
# t test statistics = -3.0633 --> critical value of 5pct = -3.42 --> rejected


# Null hypothesis:  pi = (phi - 1) = 0  (if pi = 0 --> phi = 1 --> unit root) AND beta2 = 0
# F test statistics = 3.2932  --> critical value 5pct = 4.71 --> not rejected




# ----------
# random walk + drift
# diff(y(t)) = beta1 + pi * y(t-1) + sum{ rho(j) * diff(y(t-j)) } + alpha(t),  j = 1,2,3,4,5

summary(ur.df(unemp, type = "drift", lags = 14))


# -->
# Null hypothesis:  pi = (phi - 1) = 0  (if pi = 0 --> phi = 1 --> unit root)
# t test statistics = -1.69 --> critical value of 5pct = -2.87 --> not rejected

# Null hypothesis:  pi = (phi - 1) = 0  (if pi = 0 --> phi = 1 --> unit root) AND beta1 = 0
# F test statistics = 1.585  --> critical value 5pct = 4.61 --> not rejected




# ----------
# random walk
# diff(y(t)) = pi * y(t-1) + sum{ rho(j) * diff(y(t-j)) } + alpha(t),  j = 1,2,3,4,5

summary(ur.df(unemp, type = "none", lags = 14))


# -->
# Null hypothesis:  pi = (phi - 1) = 0  (if pi = 0 --> phi = 1 --> unit root)
# t test statistics = -0.0448 --> critical value of 5pct = -1.95 --> not rejected



# -->
# unemp is random walk + trend or drift model




# ------------------------------------------------------------------------------
# 1st difference, deterending
# ------------------------------------------------------------------------------


unemp_df <- diff(unemp)

unemp_dt <- resid(lm(unemp ~ time(unemp)))



# ----------
plot(decompose(unemp_df))



# ----------
acf2(unemp_df, max.lag = 200)


acf2(unemp_dt, max.lag = 200)




# ------------------------------------------------------------------------------
# SARIMA model
# ------------------------------------------------------------------------------


smod <- sarima(unemp, p = 0, d = 1, q = 2, P = 0, D = 1, Q = 1, S = 12)


smod$ttable



# ----------
pred <- unemp - resid(smod$fit)


par(mfrow = c(1,1))

plot(unemp)

lines(pred, col = "blue", lwd = 2)




# ----------
# forecasting
sarima.for(unemp, n.ahead = 20, p = 0, d = 1, q = 2, P = 0, D = 1, Q = 1, S = 12)

