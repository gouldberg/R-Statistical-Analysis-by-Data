# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/housing_starts")

packages <- c("dplyr", "AER", "tseries", "forecast", "dynlm", "stargazer")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data: housing starts  
# ------------------------------------------------------------------------------

starts <- c(1252, 1313, 1463, 1610, 1529, 1473, 1165, 1292, 1508, 1467,
            1434, 2052, 2357, 2045, 1338, 1160, 1538, 1987, 2020, 1745,
            1292, 1084, 1062, 1703, 1750, 1742, 1805, 1621, 1488, 1376,
            1193, 1014, 1200, 1288, 1457, 1354, 1477, 1474, 1617, 1641,
            1569, 1603, 1705, 1848, 1956, 2068)

sales <- c(485, 656, 718, 634, 519, 549, 646, 819, 817, 709, 545,
           436, 412, 623, 639, 688, 750, 671, 676, 650, 534, 509,
           610, 666, 670, 667, 757, 804, 886, 880, 877, 908, 973, 1086, 1203, 1283)


# ----------
# Note that sales data is leading 10 years
length(starts)
length(sales)



# ----------
# add NA to first 10 years to sales
sales2 <- c(rep(NA, 10), sales)
length(sales2)



# ----------
data <- data.frame(starts = starts, sales2 = sales2)

data.ts <- ts(data, start = c(1960), end = c(2005), frequency = 1)



# ------------------------------------------------------------------------------
# Autoregressive Distributed Lag Model:  Koyck Lag Structure (geometrically declining effect of past on current events)
#   - beta(i) = beta0 * lambda ^ k   (k is lags, lambda = (0, 1))
#   - sum of beta = beta0 / (1 - lambda)
#   - The sum of coefficients at all lags is finite, even if k (= lags) is infinitely large
#   - The rate of decline depends on lambda ^ k
#   - (1 - lambda) is called the spped of adjustment
#
#   --> ARDL(0, infinite) = y(t) = alpha + beta0 * x(t) + beta1 * x(t-1) + ... + u(t)
#   --> y(t) = alpha * (1 - lamda) + lambda * y(t-1) + beta0 * x(t) + (ut - lambda * u(t-1))
# ------------------------------------------------------------------------------

library(dyn)

starts = ts(starts, start = c(1960,1))
sales = ts(sales, start = c(1970,1))


# ----------
reg.koy = dyn$lm(sales ~ lag(sales, -1) + starts)
print(c("lambda = ", round(reg.koy$coef[2], 5)), q = F)

summary(reg.koy)



# ----------
# mean lag of Koyck model = sum(k * beta(k)) / sum(beta(k)) = lambda / (1 - lambda)
# note that even if maximum lag is infinite, depending on lambda, the mean lag for Koyck mdoel need not be very long.
# if lambda = 0.5, mean lag is only one time period.
lamd = reg.koy$coef[2]
print(c("mean lag = lamd / (1 - lamd)", round(lamd / (1 - lamd), 5)), q = F)



# ----------
print(c("alpha = ", round(reg.koy$coef[1] / (1 - reg.koy$coef[2]), 5)), q = F)

summary(reg.koy)



