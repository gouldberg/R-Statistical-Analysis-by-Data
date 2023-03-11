# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/usmacro")

packages <- c("dplyr", "AER", "lmtest", "stargazer", "dynlm", "broom", "knitr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  usmacro
#
# u:  U.S. unemployment rate over a period of more than six decades
# g:  U.S. growth rate
# ------------------------------------------------------------------------------
data("usmacro", package = "POE5Rdata")

data <- usmacro

glimpse(data)

str(data)


is.ts(usmacro)

u.ts <- ts(usmacro$u, start = c(1948, 1), end = c(2016, 1), frequency = 4)

g.ts <- ts(usmacro$g, start = c(1948, 1), end = c(2016, 1), frequency = 4)



# ------------------------------------------------------------------------------
# Sample autocorrelation for unemployment
# ------------------------------------------------------------------------------

u.ts1 <- stats::lag(u.ts, -1)
u.ts2 <- stats::lag(u.ts, -2)
u.ts3 <- stats::lag(u.ts, -3)
u.ts4 <- stats::lag(u.ts, -4)


ubar <- mean(u.ts)

r1 <- sum((u.ts - ubar) * (u.ts1 - ubar)) / sum((u.ts - ubar)^2)
r2 <- sum((u.ts - ubar) * (u.ts2 - ubar)) / sum((u.ts - ubar)^2)
r3 <- sum((u.ts - ubar) * (u.ts3 - ubar)) / sum((u.ts - ubar)^2)
r4 <- sum((u.ts - ubar) * (u.ts4 - ubar)) / sum((u.ts - ubar)^2)


r1; r2; r3; r4



# ------------------------------------------------------------------------------
# Sample autocorrelation for unemployment by acf()
# ------------------------------------------------------------------------------

acf(u.ts, lag.max = 4, type = "correlation", plot = FALSE)



# ----------
par(mfrow=c(3,1))
acf(u.ts, lag.max = 45, main = "Correlogram for U.S. Unemployment Rate")

acf(diff(u.ts), lag.max = 45, main = "Correlogram for Difference in U.S. Unemployment Rate")

acf(g.ts, lag.max = 45, main = "Correlogram for U.S. Growth Rate")


# -->
# The correlogram of the U.S. Growth Rate shows shorter span of autocorrelation than that of U.S. Unemployment Rate but somehow cyclical.


