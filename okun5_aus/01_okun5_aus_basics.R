# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/okun5_aus")

packages <- c("dplyr", "AER", "lmtest", "stargazer", "dynlm", "broom", "knitr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  okun5_aus
#
# u:  Australia unemployment rate
# g:  Australia growth rate
#
# Okun's law
#  - relates the contemporaneous (tmie t) change in unemployment rate
#  - u(t) - u(t-1) = - gamma * (g(t) - g(t-1))
# ------------------------------------------------------------------------------
data("okun5_aus", package = "POE5Rdata")

glimpse(okun5_aus)

str(okun5_aus)


# remove dateid01
data <- okun5_aus[, c(2,3)]



# ----------
# convert to ts object
is.ts(data)

data.ts <- ts(data, start = c(1978, 2), end = c(2016, 2), frequency = 4)



# ------------------------------------------------------------------------------
# Visualize time series
# ------------------------------------------------------------------------------

plot.ts(data.ts)



# ------------------------------------------------------------------------------
# lag and 1st difference of unemployment
#  + 1-3 lags of growth rate
# ------------------------------------------------------------------------------
data.ts.tab <- cbind(data.ts,
                     stats::lag(data.ts[,2], -1),
                     diff(data.ts[,2], lag = 1),
                     stats::lag(data.ts[,1], -1),
                     stats::lag(data.ts[,1], -2),
                     stats::lag(data.ts[,1], -3))

kable(head(data.ts.tab), caption = "The Okun dataset with differences and lags", col.names=c("g", "u", "lag(u, 1)", "diff(u, 1)", "lag(g, 1)", "lag(g, 2)", "lag(g, 3)"))



# ----------
par(cex = 1.1, lwd = 1.8, mfrow=c(3,1))
plot.ts(data.ts[,2], xlab = "Year", ylab = "Unemployment Rate")
plot.ts(diff(data.ts[,2]), xlab = "Year", ylab = "Difference in Unemployment Rate")
plot.ts(data.ts[,1], xlab = "Year", ylab = "GDP Growth Rate")

