setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\unemp")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  UnempRate
# ------------------------------------------------------------------------------

unemp <- read.table("UnempRate.txt", sep = "\t", header = T, stringsAsFactors = F)


unemp <- ts(unemp, start = 1948, end = 2016, frequency = 12)




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


# no increasing trend
mean(diff(unemp))



# ------------------------------------------------------------------------------
# data exploration:  smoothing to show trend
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow = c(1,1))

plot(unemp)

lines(smooth.spline(unemp, spar = 0.5), col = "red", lwd = 2)

lines(smooth.spline(unemp, spar = 0.75), col = "blue", lwd = 2)

lines(smooth.spline(unemp, spar = 1), col = "blue", lwd = 2)



# -->
# cycles are combined ..



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


smod <- sarima(unemp, p = 0, d = 1, q = 5, P = 0, D = 1, Q = 1, S = 12)


smod$ttable



# ----------
pred <- unemp - resid(smod$fit)


par(mfrow = c(1,1))

plot(unemp)

lines(pred, col = "blue", lwd = 2)




# ----------
# forecasting
sarima.for(unemp, n.ahead = 20, p = 0, d = 1, q = 5, P = 0, D = 1, Q = 1, S = 12)

